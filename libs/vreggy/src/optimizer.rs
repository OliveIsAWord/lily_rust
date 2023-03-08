#![allow(unused_imports)]
use super::lir::{
    visit_branch, visit_branch_immut, visit_branchpoint, visit_branchpoint_immut, visit_op,
    visit_op_immut, Block, BlockId, Branch, BranchPoint, Op, Program, Register, ValueKind,
};
use super::{verify, verify_block, ModifyHandle, ModifyHandleInner};
use dbg_pls::{color, DebugPls};
use std::collections::hash_map::Entry;
use std::collections::{HashMap, HashSet};
use std::mem;

const MAX_INLINE_LENGTH: usize = usize::MAX;

const PROGRAM_OPTIMIZATIONS: &[fn(&mut Program) -> bool] = &[
    inline_blocks_pass,
    inline_jumps_pass,
    remove_dead_blocks_pass,
    remove_unused_inputs_pass,
];
const BLOCK_OPTIMIZATIONS: &[fn(ModifyHandle<Block>)] = &[
    const_propogation_pass,
    common_node_elimination_pass,
    remove_copy_pass,
    branch_on_not_pass,
    erase_unused_registers_pass,
    erase_useless_ops_pass,
    remove_nop_pass,
];

pub fn optimize_interactive(_program: &mut Program) {
    // let input = || {
    //     use std::io::{stdin, stdout, Write};
    //     let mut stdout = stdout();
    //     stdout.write_all(b"> ").unwrap();
    //     stdout.flush().unwrap();
    //     let mut s = String::new();
    //     stdin().read_line(&mut s).unwrap();
    //     s
    // };
    // loop {
    //     let (porb, i): (bool, usize) = loop {
    //         let buffer = input();
    //         let s = buffer.trim();
    //         let Some(first) = s.chars().next() else {
    //             continue;
    //         };
    //         let porb = match first {
    //             'p' => true,
    //             'b' => false,
    //             'l' => {
    //                 println!("{}", &program);
    //                 continue;
    //             }
    //             'e' => return,
    //             _ => {
    //                 eprintln!("Could not parse command.");
    //                 continue;
    //             }
    //         };
    //         let Ok(i) = s[1..].parse() else {
    //             eprintln!("Could not parse index `{}`.", &s[1..]);
    //             continue;
    //         };
    //         break (porb, i);
    //     };
    //     if porb && PROGRAM_OPTIMIZATIONS[i](program) {
    //         println!("{}", &program);
    //     } else {
    //         let o = BLOCK_OPTIMIZATIONS_OLD[i];
    //         for (id, block) in &mut program.blocks {
    //             if o(block) {
    //                 println!("{} {}", id, block);
    //             }
    //         }
    //     }
    // }
}

pub fn optimize(program: &mut Program) -> bool {
    verify(program).unwrap();
    let mut try_optimize = true;
    let mut did_optimize = false;
    let mut passes = 0;
    while try_optimize {
        let mut flag = false;
        flag |= optimize_blocks_pass(program);
        flag |= optimize_program_pass(program);
        if flag {
            did_optimize = true;
            passes += 1;
        }
        try_optimize = flag;
    }
    // yes we're doing this
    let plural = if passes == 1 { "" } else { "es" };
    println!("Optimized in {passes} pass{plural}.");
    did_optimize
}

fn optimize_program_pass(program: &mut Program) -> bool {
    let mut flag = false;
    for (_i, o) in PROGRAM_OPTIMIZATIONS.iter().enumerate() {
        let x = o(program);
        // if x {
        //     println!("p{i}")
        // }
        flag |= x;
        verify(program).unwrap();
    }
    flag
}

fn optimize_blocks_pass(program: &mut Program) -> bool {
    let mut did_optimize = false;
    for (_id, block) in &mut program.blocks {
        let mut flag = false;
        for (_i, o) in BLOCK_OPTIMIZATIONS.iter().enumerate() {
            let mut handle = ModifyHandleInner::new(block);
            o(handle.as_handle());
            flag |= handle.was_modified();
            verify_block(block).unwrap();
        }
        did_optimize |= flag;
    }
    did_optimize
}

fn inline_blocks_pass(program: &mut Program) -> bool {
    let mut inlines = vec![];
    for (id, block) in &program.blocks {
        let inline_candidate = match &block.exit {
            Branch::Jump(BranchPoint::Block(id, _)) => {
                let to_block = program.blocks.get(id).unwrap();
                let is_sufficiently_small = to_block.ops.len() <= MAX_INLINE_LENGTH;
                let block_can_recurse = to_block.can_jump_to(*id);
                (is_sufficiently_small && !block_can_recurse).then_some(*id)
            }
            _ => None,
        };
        if let Some(to_id) = inline_candidate {
            inlines.push((*id, to_id));
        }
    }
    for (dst, src) in &inlines {
        let src = program.blocks.get(src).unwrap().clone();
        let dst = program.blocks.get_mut(dst).unwrap();
        dst.inline(src);
    }
    !inlines.is_empty()
}

fn inline_jumps_pass(program: &mut Program) -> bool {
    #[derive(Debug, DebugPls)]
    enum InlineBranchPoint {
        Return(usize),
        Block(BlockId, Vec<usize>),
    }
    let mut inlines = HashMap::new();
    for (id, block) in &program.blocks {
        if block.ops.is_empty() {
            if let Branch::Jump(bp) = &block.exit {
                let get = |r| block.inputs.iter().position(|x| x == r).unwrap();
                let ibp = match bp {
                    BranchPoint::Return(r) => InlineBranchPoint::Return(get(r)),
                    BranchPoint::Block(jid, args) => {
                        let args = args.iter().map(get).collect();
                        InlineBranchPoint::Block(*jid, args)
                    }
                };
                inlines.insert(*id, ibp);
            }
        }
    }
    if inlines.is_empty() {
        return false;
    }
    let mut did_optimize = false;
    for block in program.blocks.values_mut() {
        // todo: Call op
        let replace = |bp: &mut BranchPoint| {
            let get_inline = |id, args: &[Register]| {
                inlines.get(id).map(|ibp| match ibp {
                    InlineBranchPoint::Return(i) => BranchPoint::Return(args[*i]),
                    InlineBranchPoint::Block(jid, jargs) => {
                        let jargs = jargs.iter().map(|&i| args[i]).collect();
                        BranchPoint::Block(*jid, jargs)
                    }
                })
            };
            match bp {
                BranchPoint::Block(id, args) => {
                    get_inline(id, args).map(|new_bp| *bp = new_bp).is_some()
                }
                BranchPoint::Return(_) => false,
            }
        };
        did_optimize |= match &mut block.exit {
            Branch::Jump(bp) => replace(bp),
            Branch::Branch(_, bp1, bp2) => replace(bp1) || replace(bp2),
        }
    }
    did_optimize
}

fn remove_dead_blocks_pass(program: &mut Program) -> bool {
    let mut alive = HashSet::new();
    let mut explored = vec![program.entry];
    while !explored.is_empty() {
        alive.extend(explored.iter().copied());
        let mut next = vec![];
        for i in explored {
            let block = program.blocks.get(&i).unwrap();
            // for (_, op) in &block.ops {
            //     if let Call(id, _) = op {
            //         next.push(id);
            //     }
            // }
            let mut add_id = |bp: &BranchPoint| match bp {
                BranchPoint::Block(id, _) => next.push(*id),
                BranchPoint::Return(_) => (),
            };
            match &block.exit {
                Branch::Jump(bp) => add_id(bp),
                Branch::Branch(_, bp1, bp2) => {
                    add_id(bp1);
                    add_id(bp2);
                }
            };
        }
        next.retain(|id| !alive.contains(id));
        explored = next;
    }
    let old_len = program.blocks.len();
    program.blocks.retain(|k, _| alive.contains(k));
    program.blocks.len() < old_len
}

fn remove_unused_inputs_pass(program: &mut Program) -> bool {
    let mut cull_args = HashMap::new();
    for (id, block) in &program.blocks {
        let inputs_used = detect_unused_inputs(block);
        if inputs_used.iter().any(|&x| !x) {
            cull_args.insert(*id, inputs_used);
        }
    }
    let cull_args = cull_args;
    if cull_args.is_empty() {
        return false;
    }
    for (id, block) in &mut program.blocks {
        let get_mask = |id| cull_args.get(id).map(|r| r.iter().copied());
        let retain_by = |args: &mut Vec<Register>, id| {
            get_mask(id).map(|mut retained| args.retain(|_| retained.next().unwrap()))
        };
        let retain_branch = |bp: &mut BranchPoint| {
            if let BranchPoint::Block(id, args) = bp {
                let get_mask = |id| cull_args.get(id).map(|r| r.iter().copied());
                let retain_by = |args: &mut Vec<Register>, id| {
                    get_mask(id).map(|mut retained| args.retain(|_| retained.next().unwrap()))
                };
                retain_by(args, id);
            }
        };
        retain_by(&mut block.inputs, id);
        // todo: call op
        match &mut block.exit {
            Branch::Jump(bp) => retain_branch(bp),
            Branch::Branch(_, bp1, bp2) => {
                retain_branch(bp1);
                retain_branch(bp2);
            }
        }
    }
    true
}

fn remove_nop_pass(mut b: ModifyHandle<Block>) {
    let block = b.request_mut();
    let old_len = block.ops.len();
    block.ops.retain(|(_, op)| op != &Op::Nop);
    let did_optimize = block.ops.len() < old_len;
    b.set_modified(did_optimize);
}

fn remove_copy_pass(mut b: ModifyHandle<Block>) {
    let copy_map: HashMap<Register, Register> = b
        .get()
        .ops
        .iter()
        .filter_map(|rop| match rop {
            &(Some(dst), Op::Copy(src)) => Some((dst, src)),
            _ => None,
        })
        .collect();
    let replace_copy = |r: &mut Register| {
        if let Some(&r_copy) = copy_map.get(&*r) {
            //println!("Meow! {r} {r_copy}");
            *r = r_copy;
        }
    };
    if copy_map.is_empty() {
        return;
    }
    let block = b.request_mut();
    for (r, op) in &mut block.ops {
        visit_op(op, |old_dst| replace_copy(old_dst));
        if let Some(r_some) = r {
            if copy_map.contains_key(r_some) {
                *r = None;
                *op = Op::Nop;
            }
        }
    }
    visit_branch(&mut block.exit, |r| replace_copy(r));
}

fn erase_unused_registers_pass(mut b: ModifyHandle<Block>) {
    let mut regs: HashMap<Register, bool> = HashMap::new();
    let mark = |regs: &mut HashMap<Register, bool>, r: Register| {
        if let Some(r) = regs.get_mut(&r) {
            *r = false;
        }
    };
    let block = b.get();
    for (reg, op) in &block.ops {
        visit_op_immut(op, |r| mark(&mut regs, r));
        if let Some(r) = reg {
            regs.insert(*r, true);
        }
    }
    visit_branch_immut(&block.exit, |r| mark(&mut regs, r));
    let unused_regs: HashSet<_> = regs
        .into_iter()
        .filter_map(|(r, b)| b.then_some(r))
        .collect();
    if unused_regs.is_empty() {
        return;
    }
    for (reg, _) in &mut b.request_mut().ops {
        match reg {
            Some(r) if unused_regs.contains(r) => *reg = None,
            _ => (),
        }
    }
}

fn erase_useless_ops_pass(mut b: ModifyHandle<Block>) {
    for i in 0..b.get().ops.len() {
        let (reg, op) = &b.get().ops[i];
        if reg.is_none() && op.is_pure() && op != &Op::Nop {
            b.request_mut().ops[i].1 = Op::Nop;
        }
    }
}

fn const_propogation_pass(mut b: ModifyHandle<Block>) {
    let block = b.request_mut();
    let mut did_optimize = false;
    let mut const_evaled = HashMap::<Register, u64>::new();
    for (reg, op) in block
        .ops
        .iter_mut()
        .filter_map(|(r, op)| r.map(|r| (r, op)))
    {
        let g = |evaled: &HashMap<Register, u64>, r| evaled.get(r).copied();
        let op2 = |evaled, r1, r2, op: fn(u64, u64) -> u64| {
            let v1 = g(evaled, r1)?;
            let v2 = g(evaled, r2)?;
            Some(op(v1, v2))
        };
        let regs = &const_evaled;
        let eval = match op {
            Op::Nop
            | Op::Print(_)
            | Op::PrintSlice(..)
            | Op::Allocate(_)
            | Op::Deallocate(_)
            | Op::ReadSlice(_, _)
            | Op::WriteSlice(_, _, _) => None,
            Op::Constant(v) => match v {
                ValueKind::Integer(i) => Some(*i),
                ValueKind::Pointer(_) => None,
            },
            Op::Copy(r) => {
                // Don't convert a Copy to a Constant, but still const-propogate its value.
                // The Copy op is a renaming placeholder for signaling to the optimizer
                if let Some(v) = g(regs, r) {
                    const_evaled.insert(reg, v);
                }
                None
            }
            Op::Not(r) => g(regs, r).map(|v| if v == 0 { 1 } else { 0 }),
            Op::Add(r1, r2) => op2(regs, r1, r2, u64::wrapping_add),
            Op::Sub(r1, r2) => op2(regs, r1, r2, u64::wrapping_sub),
            Op::Mul(r1, r2) => op2(regs, r1, r2, u64::wrapping_mul),
            Op::Div(r1, r2) => op2(&regs, r1, r2, |a, b| {
                a.checked_div(b).expect("division by zero")
            }),
            Op::Mod(r1, r2) => op2(&regs, r1, r2, |a, b| {
                a.checked_rem(b).expect("division by zero")
            }),
            Op::Cmp(r1, r2) => op2(regs, r1, r2, |x, y| u64::from(x == y)),
        };
        if let Some(eval) = eval {
            const_evaled.insert(reg, eval);
            if !matches!(op, Op::Constant(_)) {
                *op = Op::Constant(ValueKind::Integer(eval));
                did_optimize = true;
            }
        }
    }
    if let Branch::Branch(r, bp1, bp2) = &block.exit {
        if let Some(&val) = const_evaled.get(r) {
            let const_bp = if val == 0 { bp2 } else { bp1 };
            // TODO: remove this clone
            block.exit = Branch::Jump(const_bp.clone());
            did_optimize = true;
        }
    }
    b.set_modified(did_optimize);
}

fn common_node_elimination_pass(mut b: ModifyHandle<Block>) {
    let block = b.request_mut();
    let mut did_optimize = false;
    let mut nodes: HashMap<Op, Register> = HashMap::new();
    let iter = block.ops.iter_mut().filter_map(|(r, op)| match *r {
        Some(r) if op.is_idempotent() => Some((r, op)),
        _ => None,
    });
    for (reg, op) in iter {
        if let Some(equal_reg) = nodes.insert(op.clone(), reg) {
            assert_ne!(reg, equal_reg);
            //color!(&op);
            *op = Op::Copy(equal_reg);
            did_optimize = true;
        }
    }
    b.set_modified(did_optimize);
}

fn branch_on_not_pass(mut b: ModifyHandle<Block>) {
    let Branch::Branch(r, _, _) = &b.get().exit else { return };
    let Some(Op::Not(not_r)) = b
        .get()
        .ops
        .iter()
        .rev()
        .find_map(|(reg, op)| (reg == &Some(*r)).then_some(op))
        .cloned()
    else {
        return
    };
    let Branch::Branch(r, bp1, bp2) = &mut b.request_mut().exit else { unreachable!() };
    *r = not_r;
    mem::swap(bp1, bp2);
}

fn detect_unused_inputs(b: &Block) -> Vec<bool> {
    let mut used = vec![false; b.inputs.len()];
    let input_map: HashMap<_, _> = b
        .inputs
        .iter()
        .enumerate()
        .map(|(index, input)| (input, index))
        .collect();
    let mark = |used: &mut [bool], r: Register| {
        if let Some(&i) = input_map.get(&r) {
            used[i] = true;
        }
    };
    let u = used.as_mut();
    for (_, op) in &b.ops {
        visit_op_immut(op, |r| mark(u, r));
    }
    visit_branch_immut(&b.exit, |r| mark(u, r));
    used
}
