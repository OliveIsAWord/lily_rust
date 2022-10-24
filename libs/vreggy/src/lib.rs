use dbg_pls::{color, DebugPls};
use std::collections::HashMap;

type RegId = u32;

#[derive(Clone, Copy, Debug, DebugPls, Default)]
struct Register(RegId);

#[derive(Clone, Copy, Debug, DebugPls, Default)]
struct Function(u32);

mod clean {
    use super::*;
    type R = Register;
    #[derive(Debug, DebugPls)]
    pub(super) enum SubOp {
        Constant(u64),
        Copy(R),
        Add(R, R),
    }

    #[derive(Debug, DebugPls)]
    pub(super) enum Op {
        Assign(R, SubOp),
        Call(Function, Vec<R>),
    }
}
use clean::*;

#[derive(Debug, DebugPls)]
struct BlockBuilder {
    id: u32,
    ops: Vec<Op>,
}

impl BlockBuilder {
    fn assign(&mut self, reg: Register, op: SubOp) {
        self.ops.push(Op::Assign(reg, op));
    }
}

#[derive(Debug, DebugPls)]
struct Block {
    ops: Vec<Op>,
    branch: Branch,
}

#[derive(Debug, DebugPls)]
enum Branch {
    Jmp(u32),
    Branch(Register, u32, u32),
}

#[derive(Debug, DebugPls)]
struct BlockEntry {
    parent_id: u32,
    offset: u8,
}

#[derive(Debug, DebugPls, Default)]
struct RegisterMachine {
    reg_accum: RegId,
    block_accum: u32,
    blocks: HashMap<u32, Block>,
}

impl RegisterMachine {
    pub fn entry(&mut self) -> BlockBuilder {
        debug_assert_eq!(self.block_accum, 0, "Bad! Entry generated already!");
        self.block_accum = 43;
        BlockBuilder {
            id: 42,
            ops: vec![],
        }
    }
    pub fn new_register(&mut self) -> Register {
        let i = self.reg_accum;
        let r = Register(i);
        self.reg_accum = i.checked_add(1).expect("register saturation");
        r
    }
    pub fn add_block_jmp(&mut self, b: BlockBuilder) -> (u32, BlockEntry) {
        let BlockBuilder { id, ops } = b;
        let block = Block {
            ops,
            branch: Branch::Jmp(u32::MAX),
        };
        self.blocks.insert(id, block);
        let be = BlockEntry {
            parent_id: id,
            offset: 69,
        };
        (id, be)
    }
    pub fn add_block_branch(
        &mut self,
        b: BlockBuilder,
        r: Register,
    ) -> (u32, BlockEntry, BlockEntry) {
        let BlockBuilder { id, ops } = b;
        let block = Block {
            ops,
            branch: Branch::Branch(r, u32::MAX, u32::MAX),
        };
        self.blocks.insert(id, block);
        let be1 = BlockEntry {
            parent_id: id,
            offset: 0,
        };
        let be2 = BlockEntry {
            parent_id: id,
            offset: 1,
        };
        (id, be1, be2)
    }
    fn block_ref(&mut self, be: BlockEntry) -> &mut u32 {
        let i = match &mut self.blocks.get_mut(&be.parent_id).unwrap().branch {
            Branch::Jmp(i) => {
                debug_assert_eq!(be.offset, 69);
                i
            }
            Branch::Branch(_, i1, i2) => match be.offset {
                0 => i1,
                1 => i2,
                e => unreachable!("{}", e),
            },
        };
        debug_assert_eq!(*i, u32::MAX);
        i
    }
    fn gen_block(&mut self) -> BlockBuilder {
        let id = self.block_accum;
        self.block_accum += 1;
        BlockBuilder { id, ops: vec![] }
    }
    pub fn link(&mut self, be: BlockEntry, id: u32) {
        debug_assert!(self.blocks.get(&id).is_some());
        *self.block_ref(be) = id;
    }
    pub fn new_block(&mut self, be: BlockEntry) -> BlockBuilder {
        let block = self.gen_block();
        self.link(be, block.id);
        block
    }
}

pub fn maine() {
    let mut machine = RegisterMachine::default();
    let mut block = machine.entry();
    let v0 = machine.new_register();
    let v1 = machine.new_register();
    let v2 = machine.new_register();
    let v3 = machine.new_register();
    block.assign(v0, SubOp::Constant(34));
    block.assign(v1, SubOp::Constant(35));
    block.assign(v2, SubOp::Add(v0, v1));
    block.assign(v3, SubOp::Copy(v2));
    color!(block);
}

fn execute() {}
