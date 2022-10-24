#![allow(dead_code)]

use dbg_pls::DebugPls;
use std::collections::HashMap;

type RegId = u32;

#[derive(Clone, Copy, Debug, DebugPls, Default, Eq, Hash, PartialEq)]
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
        //Phi(R, R),
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
    fn call(&mut self, func: Function, regs: Vec<Register>) {
        self.ops.push(Op::Call(func, regs));
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
struct NonCopy;

impl Drop for NonCopy {
    fn drop(&mut self) {}
}

#[derive(Debug, DebugPls)]
struct BlockEntry {
    parent_id: u32,
    offset: u8,
    non_copy: NonCopy,
}

impl BlockEntry {
    const fn new(parent_id: u32, offset: u8) -> Self {
        Self {
            parent_id,
            offset,
            non_copy: NonCopy,
        }
    }
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
        (id, BlockEntry::new(id, 69))
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
        let be1 = BlockEntry::new(id, 0);
        let be2 = BlockEntry::new(id, 1);
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
        drop(be);
        i
    }
    fn gen_block(&mut self) -> BlockBuilder {
        let id = self.block_accum;
        self.block_accum += 1;
        BlockBuilder { id, ops: vec![] }
    }
    pub fn link(&mut self, be: BlockEntry, id: u32) {
        //debug_assert!(self.blocks.get(&id).is_some());
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
    let (_, be) = machine.add_block_jmp(block);
    let mut block2 = machine.new_block(be);
    block2.assign(v3, SubOp::Copy(v2));
    block2.call(Function(1337), vec![v0, v1, v2, v3]);
    let (_, be2) = machine.add_block_jmp(block2);
    machine.link(be2, 0);
    //color!(&block);
    execute(&machine.blocks);
}

fn execute(blocks: &HashMap<u32, Block>) {
    use std::ops::Add;
    let mut registers: HashMap<Register, u64> = HashMap::new();
    let op1 =
        |registers: &HashMap<Register, u64>, r1, f: fn(u64) -> u64| f(*registers.get(r1).unwrap());
    let op2 = |registers: &HashMap<Register, u64>, r1, r2, f: fn(u64, u64) -> u64| {
        f(*registers.get(r1).unwrap(), *registers.get(r2).unwrap())
    };
    let mut block = blocks.get(&42).expect("no entry block found");
    println!("Running...");
    loop {
        for op in &block.ops {
            match op {
                Op::Assign(dest, sub_op) => {
                    let val = match sub_op {
                        SubOp::Constant(v) => *v,
                        SubOp::Add(r1, r2) => op2(&registers, r1, r2, u64::add),
                        SubOp::Copy(r1) => op1(&registers, r1, |x| x),
                    };
                    let prev_value = registers.insert(*dest, val);
                    debug_assert!(prev_value.is_none());
                }
                Op::Call(_func_id, regs) => {
                    print!("LilyRust print[");
                    for (i, r) in regs.iter().enumerate() {
                        if i > 0 {
                            print!(", ");
                        }
                        print!("r{}: {}", r.0, *registers.get(r).unwrap());
                    }
                    println!("]");
                }
            }
            //dbg_pls::color!(&registers);
        }
        let next_id = match block.branch {
            Branch::Jmp(id) => id,
            Branch::Branch(reg, id1, id2) => {
                if *registers.get(&reg).unwrap() > 0 {
                    id1
                } else {
                    id2
                }
            }
        };
        match next_id {
            0 => {
                println!("happy exit!");
                break;
            }
            1 => {
                println!("sad exit! :(((((((((");
                break;
            }
            id => {
                println!("jumping to block {id}!");
                block = blocks.get(&id).unwrap();
            }
        }
    }
}
