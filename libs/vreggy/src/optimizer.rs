#![allow(unused_imports)]
use super::lir::{Block, BlockId, Branch, BranchPoint, Op, Program, Register};
use std::collections::{HashMap, HashSet};

pub fn optimize(program: &mut Program) -> bool {
    let mut try_optimize = true;
    let mut did_optimize = false;
    let mut passes = 0;
    while try_optimize {
        try_optimize = false;
        try_optimize |= optimize_blocks_pass(program);
        try_optimize |= optimize_program_pass(program);
        if try_optimize {
            did_optimize = true;
            passes += 1;
        }
    }
    // yes we're doing this
    let plural = if passes == 1 { "" } else { "es" };
    println!("Optimized in {passes} pass{plural}.");
    did_optimize
}

fn optimize_program_pass(program: &mut Program) -> bool {
    let mut flag = false;
    flag |= inline_blocks_pass(program);
    flag |= remove_dead_blocks_pass(program);
    flag
}

fn inline_blocks_pass(program: &mut Program) -> bool {
    let mut inlines = vec![];
    for (id, block) in &program.blocks {
        let inline_candidate = match &block.exit {
            Branch::Jump(BranchPoint::Block(id, _)) => {
                let to_block = program.blocks.get(id).unwrap();
                (to_block.ops.len() <= 5).then_some(*id)
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

fn optimize_blocks_pass(program: &mut Program) -> bool {
    let mut did_optimize = false;
    for block in program.blocks.values_mut() {
        let mut flag = false;
        flag |= erase_unused_registers_pass(block);
        flag |= erase_useless_ops_pass(block);
        flag |= remove_nop_pass(block);
        did_optimize |= flag;
    }
    did_optimize
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
