use dbg_pls::DebugPls;
use std::collections::HashMap;
use std::fmt;
use std::mem;
use std::num::NonZeroI32;

pub type RegId = NonZeroI32;
pub type BlockId = u32;

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct Register(pub(super) RegId);

type R = Register;

impl Register {
    #[must_use]
    pub const fn is_input(self) -> bool {
        self.0.get() < 0
    }
}

#[derive(Clone, Debug, DebugPls, Eq, PartialEq)]
pub enum Op {
    Nop,
    Constant(u64),
    //Copy(R),
    Not(R),
    Add(R, R),
    Sub(R, R),
    Mul(R, R),
    Cmp(R, R),
    //Call(BlockId, Vec<R>),
    Print(R),
}

impl Op {
    pub const fn is_pure(&self) -> bool {
        match self {
            Self::Nop
            | Self::Constant(_)
            | Self::Not(_)
            | Self::Add(..)
            | Self::Sub(..)
            | Self::Mul(..)
            | Self::Cmp(..) => true,
            Self::Print(_) => false,
        }
    }
}

#[derive(Clone, Debug, DebugPls)]
pub enum Branch {
    Jump(BranchPoint),
    Branch(R, BranchPoint, BranchPoint),
}

#[derive(Clone, Debug, DebugPls)]
pub enum BranchPoint {
    Block(BlockId, Vec<R>),
    Return(R),
}

#[derive(Clone, Debug, DebugPls)]
pub struct Block {
    pub(super) inputs: Vec<R>,
    pub(super) ops: Vec<(Option<R>, Op)>,
    pub(super) exit: Branch,
}

impl Block {
    pub(super) fn inline(&mut self, jump_point: Self) {
        let mut reg_accum: i32 = self.ops.iter().filter_map(|&(r, _)| r.map(|r| r.0.get())).max().unwrap_or(1).abs();
        let mut new_register = || {
            reg_accum += 1;
            Register(RegId::new(reg_accum).unwrap())
        };
        let Self {
            inputs,
            ops,
            mut exit,
        } = jump_point;
        let jump_args = match &mut self.exit {
            Branch::Jump(BranchPoint::Block(_, args)) => mem::take(args),
            e => panic!("{e:?}"),
        };
        assert_eq!(jump_args.len(), inputs.len());
        let mut arg_map: HashMap<_, _> = inputs.into_iter().zip(jump_args).collect();
        let convert = |arg_map: &HashMap<_, _>, r: &mut Register| {
            if let Some(&new_r) = arg_map.get(r) {
                *r = new_r
            }
        };
        for (mut reg, mut op) in ops {
            match &mut op {
                Op::Nop | Op::Constant(_) => (),
                Op::Not(r) | Op::Print(r) => convert(&arg_map, r),
                Op::Add(r1, r2) | Op::Sub(r1, r2) | Op::Mul(r1, r2) | Op::Cmp(r1, r2) => {
                    convert(&arg_map, r1);
                    convert(&arg_map, r2);
                }
            }
            if let Some(r) = &mut reg {
                let new_reg = new_register();
                arg_map.insert(*r, new_reg);
                *r = new_reg;
            }
            self.ops.push((reg, op));
        }
        let convert_branchpoint = |bp: &mut BranchPoint| match bp {
            BranchPoint::Block(_, args) => {
                for r in args {
                    convert(&arg_map, r);
                }
            }
            BranchPoint::Return(r) => convert(&arg_map, r),
        };
        match &mut exit {
            Branch::Jump(bp) => convert_branchpoint(bp),
            Branch::Branch(r, bp1, bp2) => {
                convert(&arg_map, r);
                convert_branchpoint(bp1);
                convert_branchpoint(bp2);
            }
        }
        self.exit = exit;
    }
}

#[derive(Debug, DebugPls)]
pub struct Program {
    pub entry: BlockId,
    pub blocks: HashMap<BlockId, Block>,
}

impl Program {
    pub(super) fn fmt_internal(
        entry_id: BlockId,
        blocks: &HashMap<BlockId, Block>,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        writeln!(f, "entry = block_{entry_id}()")?;
        let mut blocks: Vec<_> = blocks.iter().collect();
        blocks.sort_by_key(|b| b.0);
        let mut is_first_block = true;
        for (id, block) in blocks {
            if !is_first_block {
                writeln!(f)?;
            }
            is_first_block = false;
            write!(f, "block_{id}(")?;
            for (i, r) in block.inputs.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{r}")?;
            }
            write!(f, "):")?; // ):
            for (reg, ops) in &block.ops {
                write!(f, "\n    ")?;
                match reg {
                    Some(r) => write!(f, "{r}")?,
                    None => write!(f, "_")?,
                }
                write!(f, " = {ops}")?;
            }
            write!(f, "\n    ")?;
            match &block.exit {
                Branch::Jump(j) => write!(f, "{j}")?,
                Branch::Branch(r, j1, j2) => write!(f, "branch {r} {{ {j1} }} else {{ {j2} }}")?,
            }
        }
        Ok(())
    }
}

impl DebugPls for Register {
    fn fmt(&self, f: dbg_pls::Formatter<'_>) {
        f.debug_tuple_struct("Register")
            .field(&self.0.get())
            .finish();
    }
}

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_input() {
            write!(f, "i{}", -self.0.get())
        } else {
            write!(f, "r{}", self.0.get())
        }
    }
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Nop => write!(f, "nop"),
            Self::Constant(v) => write!(f, "{v}"),
            //Self::Copy(r) => write!(f, "{r}"),
            Self::Not(r) => write!(f, "not {r}"),
            Self::Add(r1, r2) => write!(f, "{r1} + {r2}"),
            Self::Sub(r1, r2) => write!(f, "{r1} - {r2}"),
            Self::Mul(r1, r2) => write!(f, "{r1} * {r2}"),
            Self::Cmp(r1, r2) => write!(f, "{r1} == {r2}"),
            // Self::Call(id, regs) => {
            //     write!(f, "block_{id}(")?;
            //     for (i, r) in regs.iter().enumerate() {
            //         if i > 0 {
            //             write!(f, ", ")?;
            //         }
            //         write!(f, "{r}")?;
            //     }
            //     write!(f, ")")
            // }
            Self::Print(r) => write!(f, "print {r}"),
        }
    }
}

impl fmt::Display for BranchPoint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Block(id, regs) => {
                write!(f, "jump block_{id}(")?;
                for (i, r) in regs.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{r}")?;
                }
                write!(f, ")")
            }
            Self::Return(r) => write!(f, "return {r}"),
        }
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Self::fmt_internal(self.entry, &self.blocks, f)
    }
}
