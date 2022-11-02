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
        let reg_id = self.input_accum.checked_sub(1).and_then(RegId::new).unwrap();
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
        let BlockBuilder { inputs, ops, id, .. } = bb;
        let block = Block {
            inputs,
            ops,
            exit,
        };
        self.blocks.insert(id, block);
    }
    pub fn finish_block_jump(&mut self, bb: BlockBuilder, to: BranchPoint) {
        self.finish_block(bb, Branch::Jump(to));
    }
    pub fn finish_block_branch(&mut self, bb: BlockBuilder, r: Register, to1: BranchPoint, to2: BranchPoint) {
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
    bb2.assign(r2, Op::Call(bb1_id, vec![r0]));
    bb2.assign(r3, Op::Mul(i0, r2));
    pb.finish_block_jump(bb2, BranchPoint::Return(r3));
    color!(pb);
    dbg!(NonZeroI32::new(69).unwrap());
}
