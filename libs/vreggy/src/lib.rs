#![allow(dead_code, unused_imports)]
use dbg_pls::{color, DebugPls};
use parser::{ExprKind, StatementKind};
use std::collections::HashMap;
use std::num::NonZeroI32;

type RegId = NonZeroI32;
type BlockId = u32;

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
struct Register(RegId);

impl Register {
    fn is_input(self) -> bool {
        self.0.get() < 0
    }
}

impl DebugPls for Register {
    fn fmt(&self, f: dbg_pls::Formatter<'_>) {
        f.debug_tuple_struct("Register")
            .field(&self.0.get())
            .finish();
    }
}

mod clean {
    use super::*;
    type R = Register;
    #[derive(Debug, DebugPls)]
    pub(super) enum Op {
        Constant(u64),
        Copy(R),
        Add(R, R),
        Sub(R, R),
        Mul(R, R),
        Cmp(R, R),
        Call(BlockId, Vec<R>),
    }
}
use clean::*;

#[derive(Debug, DebugPls)]
enum Branch {
    Jump(BranchPoint),
    Branch(Register, BranchPoint, BranchPoint),
}

#[derive(Debug, DebugPls)]
enum BranchPoint {
    Block(BlockId, Vec<Register>),
    Return(Register),
}

#[derive(Debug, DebugPls)]
struct Block {
    inputs: Vec<Register>,
    ops: Vec<(Option<Register>, Op)>,
    exit: Branch,
}

#[derive(Debug, DebugPls)]
struct BlockBuilder {
    reg_accum: i32,
    input_accum: i32,
    inputs: Vec<Register>,
    id: BlockId,
    ops: Vec<(Option<Register>, Op)>,
}

impl BlockBuilder {
    fn new(id: BlockId) -> Self {
        Self {
            reg_accum: 0,
            input_accum: 0,
            id,
            inputs: vec![],
            ops: vec![],
        }
    }
    pub fn id(&self) -> BlockId {
        self.id
    }
    pub fn new_input(&mut self) -> Register {
        let reg_id = self
            .input_accum
            .checked_sub(1)
            .and_then(RegId::new)
            .unwrap();
        self.input_accum = reg_id.get();
        let reg = Register(reg_id);
        self.inputs.push(reg);
        reg
    }
    pub fn new_register(&mut self) -> Register {
        let reg_id = self.reg_accum.checked_add(1).and_then(RegId::new).unwrap();
        self.reg_accum = reg_id.get();
        Register(reg_id)
    }
    pub fn assign(&mut self, r: Register, op: Op) {
        debug_assert!(!r.is_input());
        self.ops.push((Some(r), op));
    }
}

#[derive(Debug, DebugPls)]
struct ProgramBuilder {
    block_id_accum: BlockId,
    entry_block: Option<BlockId>,
    blocks: HashMap<BlockId, Block>,
}

impl ProgramBuilder {
    pub fn new() -> Self {
        Self {
            block_id_accum: 42,
            blocks: HashMap::new(),
            entry_block: None,
        }
    }
    pub fn create_block(&mut self) -> BlockBuilder {
        let id = self.block_id_accum;
        self.block_id_accum += 1;
        BlockBuilder::new(id)
    }
    pub fn set_entry(&mut self, id: BlockId) {
        self.entry_block = Some(id);
    }
    pub fn finish_block(&mut self, bb: BlockBuilder, exit: Branch) {
        let BlockBuilder {
            inputs, ops, id, ..
        } = bb;
        let block = Block { inputs, ops, exit };
        self.blocks.insert(id, block);
    }
    pub fn finish_block_jump(&mut self, bb: BlockBuilder, to: BranchPoint) {
        self.finish_block(bb, Branch::Jump(to));
    }
    pub fn finish_block_branch(
        &mut self,
        bb: BlockBuilder,
        r: Register,
        to1: BranchPoint,
        to2: BranchPoint,
    ) {
        // heehee, tutu
        self.finish_block(bb, Branch::Branch(r, to1, to2));
    }
}

pub fn compile(_statements: &[StatementKind]) {
    fact();
}

fn fact() {
    let mut pb = ProgramBuilder::new();
    let mut bb1 = pb.create_block();
    let i0 = bb1.new_input();
    let r0 = bb1.new_register();
    let r1 = bb1.new_register();
    let r2 = bb1.new_register();
    bb1.assign(r0, Op::Constant(0));
    bb1.assign(r1, Op::Cmp(i0, r0));
    bb1.assign(r2, Op::Constant(1));
    let mut bb2 = pb.create_block();
    let bb1_id = bb1.id();
    pb.finish_block_branch(
        bb1,
        r1,
        BranchPoint::Return(r2),
        BranchPoint::Block(bb2.id(), vec![i0]),
    );
    let i0 = bb2.new_input();
    let r0 = bb2.new_register();
    let r1 = bb2.new_register();
    let r2 = bb2.new_register();
    let r3 = bb2.new_register();
    bb2.assign(r0, Op::Constant(1));
    bb2.assign(r1, Op::Sub(i0, r0));
    bb2.assign(r2, Op::Call(bb1_id, vec![r1]));
    bb2.assign(r3, Op::Mul(i0, r2));
    pb.finish_block_jump(bb2, BranchPoint::Return(r3));
    let mut bb3 = pb.create_block();
    let r0 = bb3.new_register();
    bb3.assign(r0, Op::Constant(5));
    pb.set_entry(bb3.id());
    pb.finish_block_jump(bb3, BranchPoint::Block(bb1_id, vec![r0]));
    color!(&pb);
    meow(&pb);
}

fn meow(pb: &ProgramBuilder) {
    let entry_id = pb.entry_block.unwrap();
    let val = execute(&pb.blocks, entry_id, vec![]);
    println!("Terminated with {val}");
}

fn execute(p: &HashMap<BlockId, Block>, mut block_id: BlockId, mut params: Vec<u64>) -> u64 {
    loop {
        //println!("Running block {block_id}");
        //color!(&params);
        //pause();
        let block = p.get(&block_id).unwrap();
        let mut regs = HashMap::new();
        let g = |regs: &HashMap<Register, u64>, r: &Register| -> u64 {
            if let Some(i) =
                r.0.get()
                    .checked_add(1)
                    .and_then(i32::checked_neg)
                    .and_then(|x| usize::try_from(x).ok())
            {
                params[i]
            } else {
                *regs.get(r).unwrap()
            }
        };
        let op2 = |regs: &HashMap<Register, u64>, r1, r2, op: fn(u64, u64) -> u64| {
            op(g(regs, r1), g(regs, r2))
        };
        for (reg, op) in &block.ops {
            //color!(reg, op);
            let val = match op {
                Op::Constant(v) => *v,
                Op::Copy(r) => g(&regs, r),
                Op::Add(r1, r2) => op2(&regs, r1, r2, u64::wrapping_add),
                Op::Sub(r1, r2) => op2(&regs, r1, r2, u64::wrapping_sub),
                Op::Mul(r1, r2) => op2(&regs, r1, r2, u64::wrapping_mul),
                Op::Cmp(r1, r2) => op2(&regs, r1, r2, |x, y| (x == y) as u64),
                Op::Call(id, args) => execute(p, *id, args.iter().map(|r| g(&regs, &r)).collect()),
            };
            let val: u64 = val;
            if let Some(reg) = reg {
                let prev_value = regs.insert(*reg, val);
                debug_assert!(prev_value.is_none());
            }
            //color!(&regs);
        }
        let jump_to = match &block.exit {
            Branch::Jump(j) => j,
            Branch::Branch(r, j1, j2) => {
                if g(&regs, r) != 0 {
                    j1
                } else {
                    j2
                }
            }
        };
        match jump_to {
            BranchPoint::Return(r) => return g(&regs, &r),
            BranchPoint::Block(id, args) => {
                block_id = *id;
                params = args.iter().map(|r| g(&regs, &r)).collect();
            }
        }
    }
}

// fn foo() {
//     let mut h: HashMap<String, String> = HashMap::new();
//     h.insert(42.to_string(), false.to_string());
//     let f = |map: &HashMap<String, String>, index: String| map.get(&index).unwrap().clone();
//     let val = f(&h, 42.to_string());
//     h.insert(43.to_string(), val);
// }

#[allow(dead_code)]
fn pause() {
    use std::io::{stdin, stdout, Write};
    let mut stdout = stdout();
    stdout.write_all(b"Press Enter to continue...").unwrap();
    stdout.flush().unwrap();
    let mut _line = String::new();
    stdin().read_line(&mut _line).unwrap();
}
