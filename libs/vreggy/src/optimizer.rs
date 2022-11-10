#![allow(unused_imports)]
use super::lir::{Block, BlockId, Branch, BranchPoint, Op, Program, Register};
use super::{verify, verify_block};
use dbg_pls::{color, DebugPls};
use std::collections::{HashMap, HashSet};
use std::mem;

const MAX_INLINE_LENGTH: usize = usize::MAX;

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
    let optimizers = [
        inline_blocks_pass,
        inline_jumps_pass,
        remove_dead_blocks_pass,
        remove_unused_inputs_pass,
    ];
    let mut flag = false;
    for o in optimizers {
        flag |= o(program);
        verify(program).unwrap();
    }
    flag
}

fn optimize_blocks_pass(program: &mut Program) -> bool {
    let optimizers = [
        const_propogation_pass,
        branch_on_not_pass,
        erase_unused_registers_pass,
        erase_useless_ops_pass,
        remove_nop_pass,
    ];
    let mut did_optimize = false;
    for block in program.blocks.values_mut() {
        let mut flag = false;
        for o in optimizers {
            flag |= o(block);
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
                (to_block.ops.len() <= MAX_INLINE_LENGTH && !to_block.can_jump_to(*id))
                    .then_some(*id)
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

fn remove_nop_pass(b: &mut Block) -> bool {
    let old_len = b.ops.len();
    b.ops.retain(|(_, op)| op != &Op::Nop);
    b.ops.len() < old_len
}

fn erase_unused_registers_pass(b: &mut Block) -> bool {
    let mut regs: HashMap<Register, bool> = HashMap::new();
    let mark = |regs: &mut HashMap<Register, bool>, r: &Register| {
        if let Some(r) = regs.get_mut(r) {
            *r = false;
        }
    };
    for (reg, op) in &b.ops {
        match op {
            Op::Nop | Op::Constant(_) => (),
            Op::Not(r) | Op::Print(r) => mark(&mut regs, r),
            Op::Add(r1, r2) | Op::Sub(r1, r2) | Op::Mul(r1, r2) | Op::Cmp(r1, r2) => {
                mark(&mut regs, r1);
                mark(&mut regs, r2);
            }
        }
        if let Some(r) = reg {
            regs.insert(*r, true);
        }
    }
    let mark_branchpoint = |regs: &mut HashMap<Register, bool>, bp: &BranchPoint| match bp {
        BranchPoint::Block(_, args) => {
            for r in args {
                mark(regs, r);
            }
        }
        BranchPoint::Return(r) => mark(regs, r),
    };
    match &b.exit {
        Branch::Jump(bp) => mark_branchpoint(&mut regs, bp),
        Branch::Branch(r, bp1, bp2) => {
            mark(&mut regs, r);
            mark_branchpoint(&mut regs, bp1);
            mark_branchpoint(&mut regs, bp2);
        }
    }
    let unused_regs: HashSet<_> = regs
        .into_iter()
        .filter_map(|(r, b)| b.then_some(r))
        .collect();
    if unused_regs.is_empty() {
        return false;
    }
    for (reg, _) in &mut b.ops {
        match reg {
            Some(r) if unused_regs.contains(r) => *reg = None,
            _ => (),
        }
    }
    true
}

fn erase_useless_ops_pass(b: &mut Block) -> bool {
    let mut did_optimize = false;
    for op in b
        .ops
        .iter_mut()
        .filter_map(|(reg, op)| reg.is_none().then_some(op))
    {
        if op.is_pure() {
            *op = Op::Nop;
            did_optimize = true;
        }
    }
    did_optimize
}

fn const_propogation_pass(b: &mut Block) -> bool {
    let mut did_optimize = false;
    let mut const_evaled = HashMap::<Register, u64>::new();
    for (reg, op) in b.ops.iter_mut().filter_map(|(r, op)| r.map(|r| (r, op))) {
        let g = |evaled: &HashMap<Register, u64>, r| evaled.get(r).copied();
        let op2 = |evaled, r1, r2, op: fn(u64, u64) -> u64| {
            let v1 = g(evaled, r1)?;
            let v2 = g(evaled, r2)?;
            Some(op(v1, v2))
        };
        let regs = &const_evaled;
        let eval = match op {
            Op::Nop | Op::Print(_) => None,
            Op::Constant(v) => Some(*v),
            Op::Not(r) => g(regs, r).map(|v| if v == 0 { 1 } else { 0 }),
            Op::Add(r1, r2) => op2(regs, r1, r2, u64::wrapping_add),
            Op::Sub(r1, r2) => op2(regs, r1, r2, u64::wrapping_sub),
            Op::Mul(r1, r2) => op2(regs, r1, r2, u64::wrapping_mul),
            Op::Cmp(r1, r2) => op2(regs, r1, r2, |x, y| u64::from(x == y)),
        };
        if let Some(eval) = eval {
            const_evaled.insert(reg, eval);
            if !matches!(op, Op::Constant(_)) {
                *op = Op::Constant(eval);
                did_optimize = true;
            }
        }
    }
    if let Branch::Branch(r, bp1, bp2) = &b.exit {
        if let Some(&val) = const_evaled.get(r) {
            let const_bp = if val == 0 { bp2 } else { bp1 };
            // TODO: remove this clone
            b.exit = Branch::Jump(const_bp.clone());
            did_optimize = true;
        }
    }
    did_optimize
}

fn branch_on_not_pass(b: &mut Block) -> bool {
    //color!(&b);
    let Branch::Branch(r, bp1, bp2) = &mut b.exit else { return false; };
    let Some(Op::Not(not_r)) = b.ops
    .iter()
    .rev()
    .find_map(|(reg, op)| (reg == &Some(*r)).then_some(op)) else  { return false; };
    *r = *not_r;
    mem::swap(bp1, bp2);
    true
}

fn detect_unused_inputs(b: &Block) -> Vec<bool> {
    let mut used = vec![false; b.inputs.len()];
    let input_map: HashMap<_, _> = b
        .inputs
        .iter()
        .enumerate()
        .map(|(index, input)| (input, index))
        .collect();
    let mark = |used: &mut [bool], r| {
        if let Some(&i) = input_map.get(r) {
            used[i] = true;
        }
    };
    let u = used.as_mut();
    for (_, op) in &b.ops {
        match op {
            Op::Nop | Op::Constant(_) => (),
            Op::Not(r) | Op::Print(r) => mark(u, r),
            Op::Add(r1, r2) | Op::Sub(r1, r2) | Op::Mul(r1, r2) | Op::Cmp(r1, r2) => {
                mark(u, r1);
                mark(u, r2);
            }
        }
    }
    let mark_branchpoint = |used: &mut [bool], bp: &BranchPoint| match bp {
        BranchPoint::Block(_, args) => {
            for r in args {
                if let Some(&i) = input_map.get(r) {
                    used[i] = true;
                }
            }
        }
        BranchPoint::Return(r) => {
            if let Some(&i) = input_map.get(r) {
                used[i] = true;
            }
        }
    };
    match &b.exit {
        Branch::Jump(bp) => mark_branchpoint(u, bp),
        Branch::Branch(r, bp1, bp2) => {
            mark(u, r);
            mark_branchpoint(u, bp1);
            mark_branchpoint(u, bp2);
        }
    }
    used
}
