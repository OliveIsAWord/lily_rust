use dbg_pls::DebugPls;
use std::collections::HashMap;
use std::fmt;
use std::mem;
use std::num::{NonZeroI32, NonZeroUsize};

pub type ProvenanceId = NonZeroUsize;
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct Provenance(pub(super) ProvenanceId);

#[derive(Clone, Copy, Debug, DebugPls, Eq, Hash, PartialEq)]
pub enum ValueKind {
    Integer(u64),
    Pointer(Provenance),
}

pub type RegId = NonZeroI32;
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct Register(pub(super) RegId);

type R = Register;

impl Register {
    #[must_use]
    pub const fn is_input(self) -> bool {
        self.0.get() < 0
    }
}

#[derive(Clone, Debug, DebugPls, Eq, Hash, PartialEq)]
pub enum Op {
    Nop,
    Constant(ValueKind),
    Copy(R),
    Not(R),
    Add(R, R),
    Sub(R, R),
    Mul(R, R),
    Div(R, R),
    Mod(R, R),
    Cmp(R, R),
    //Call(BlockId, Vec<R>),
    Print(R),
    PrintSlice(R, R),
    Allocate(R),
    Deallocate(R),
    ReadSlice(R, R),
    WriteSlice(R, R, R),
}

impl Op {
    pub const fn is_pure(&self) -> bool {
        match self {
            Self::Nop
            | Self::Constant(_)
            | Self::Copy(_)
            | Self::Not(_)
            | Self::Add(..)
            | Self::Sub(..)
            | Self::Mul(..)
            | Self::Div(..)
            | Self::Mod(..)
            | Self::Cmp(..)
            | Self::Allocate(_)
            | Self::ReadSlice(..) => true,
            Self::Print(_) | Self::PrintSlice(..) | Self::Deallocate(_) | Self::WriteSlice(..) => {
                false
            }
        }
    }
    pub const fn is_uninhabited(&self) -> bool {
        match self {
            Self::Constant(_)
            | Self::Copy(_)
            | Self::Not(_)
            | Self::Add(..)
            | Self::Sub(..)
            | Self::Mul(..)
            | Self::Div(..)
            | Self::Mod(..)
            | Self::Cmp(..)
            | Self::Allocate(_)
            | Self::ReadSlice(..) => false,
            Self::Nop
            | Self::Print(_)
            | Self::PrintSlice(..)
            | Self::Deallocate(_)
            | Self::WriteSlice(..) => true,
        }
    }
    pub const fn is_idempotent(&self) -> bool {
        match self {
            Self::Allocate(_) | Self::ReadSlice(..) | Self::WriteSlice(..) => false,
            _ => true,
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

pub fn visit_op(op: &mut Op, mut f: impl FnMut(&mut Register)) {
    match op {
        Op::Nop | Op::Constant(_) => (),
        Op::Copy(r) | Op::Not(r) | Op::Print(r) | Op::Allocate(r) | Op::Deallocate(r) => f(r),
        Op::Add(r1, r2)
        | Op::Sub(r1, r2)
        | Op::Mul(r1, r2)
        | Op::Div(r1, r2)
        | Op::Mod(r1, r2)
        | Op::Cmp(r1, r2)
        | Op::PrintSlice(r1, r2)
        | Op::ReadSlice(r1, r2) => {
            f(r1);
            f(r2);
        }
        Op::WriteSlice(r1, r2, r3) => {
            f(r1);
            f(r2);
            f(r3);
        }
    }
}

pub fn visit_branch(branch: &mut Branch, mut f: impl FnMut(&mut Register)) {
    match branch {
        Branch::Jump(bp) => visit_branchpoint(bp, f),
        Branch::Branch(r, bp1, bp2) => {
            f(r);
            visit_branchpoint(bp1, &mut f);
            visit_branchpoint(bp2, f);
        }
    }
}

pub fn visit_branchpoint(bp: &mut BranchPoint, mut f: impl FnMut(&mut Register)) {
    match bp {
        BranchPoint::Block(_, args) => args.into_iter().for_each(f),
        BranchPoint::Return(r) => f(r),
    }
}

pub fn visit_op_immut(op: &Op, mut f: impl FnMut(Register)) {
    match op {
        Op::Nop | Op::Constant(_) => (),
        Op::Copy(r) | Op::Not(r) | Op::Print(r) | Op::Allocate(r) | Op::Deallocate(r) => f(*r),
        Op::Add(r1, r2)
        | Op::Sub(r1, r2)
        | Op::Mul(r1, r2)
        | Op::Div(r1, r2)
        | Op::Mod(r1, r2)
        | Op::Cmp(r1, r2)
        | Op::PrintSlice(r1, r2)
        | Op::ReadSlice(r1, r2) => {
            f(*r1);
            f(*r2);
        }
        Op::WriteSlice(r1, r2, r3) => {
            f(*r1);
            f(*r2);
            f(*r3);
        }
    }
}
pub fn visit_branch_immut(branch: &Branch, mut f: impl FnMut(Register)) {
    match branch {
        Branch::Jump(bp) => visit_branchpoint_immut(bp, f),
        Branch::Branch(r, bp1, bp2) => {
            f(*r);
            visit_branchpoint_immut(bp1, &mut f);
            visit_branchpoint_immut(bp2, f);
        }
    }
}
pub fn visit_branchpoint_immut(bp: &BranchPoint, mut f: impl FnMut(Register)) {
    match bp {
        BranchPoint::Block(_, args) => args.iter().copied().for_each(f),
        BranchPoint::Return(r) => f(*r),
    }
}

pub type BlockId = u32;
#[derive(Clone, Debug, DebugPls)]
pub struct Block {
    pub inputs: Vec<R>,
    pub ops: Vec<(Option<R>, Op)>,
    pub exit: Branch,
}

impl Block {
    pub(super) fn can_jump_to(&self, block_id: BlockId) -> bool {
        let cans = |bp: &BranchPoint| {
            matches!(bp,
                BranchPoint::Block(id, _) if block_id == *id
            )
        };
        match &self.exit {
            Branch::Jump(bp) => cans(bp),
            Branch::Branch(_, bp1, bp2) => cans(bp1) || cans(bp2),
        }
    }
    pub(super) fn inline(&mut self, jump_point: Self) {
        let mut reg_accum: i32 = self
            .ops
            .iter()
            .filter_map(|&(r, _)| r.map(|r| r.0.get()))
            .max()
            .unwrap_or(1)
            .abs();
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
                *r = new_r;
            }
        };
        for (mut reg, mut op) in ops {
            visit_op(&mut op, |r| convert(&arg_map, r));
            if let Some(r) = &mut reg {
                let new_reg = new_register();
                arg_map.insert(*r, new_reg);
                *r = new_reg;
            }
            self.ops.push((reg, op));
        }
        visit_branch(&mut exit, |r| convert(&arg_map, r));
        self.exit = exit;
    }
}

#[derive(Debug, DebugPls)]
pub struct Program {
    pub entry: BlockId,
    pub blocks: HashMap<BlockId, Block>,
}

impl Block {
    pub(super) fn fmt_internal(
        &self,
        id: Option<BlockId>,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        write!(f, "block")?;
        if let Some(id) = id {
            write!(f, "_{id}")?;
        }
        write!(f, "(")?;
        for (i, r) in self.inputs.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{r}")?;
        }
        writeln!(f, "):")?; // ):
        for (reg, op) in &self.ops {
            write!(f, "    ")?;
            if !op.is_uninhabited() {
                match reg {
                    Some(r) => write!(f, "{r}")?,
                    None => write!(f, "_")?,
                }
                write!(f, " = ")?;
            } else if let Some(r) = reg {
                write!(f, "{r} ?= ")?;
            }
            writeln!(f, "{op}")?;
        }
        write!(f, "    ")?;
        match &self.exit {
            Branch::Jump(j) => write!(f, "{j}"),
            Branch::Branch(r, j1, j2) => write!(f, "branch {r} {{ {j1} }} else {{ {j2} }}"),
        }
    }
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
            block.fmt_internal(Some(*id), f)?;
        }
        Ok(())
    }
}

impl DebugPls for Provenance {
    fn fmt(&self, f: dbg_pls::Formatter<'_>) {
        f.debug_tuple_struct("Provenance")
            .field(&self.0.get())
            .finish();
    }
}

impl DebugPls for Register {
    fn fmt(&self, f: dbg_pls::Formatter<'_>) {
        f.debug_tuple_struct("Register")
            .field(&self.0.get())
            .finish();
    }
}

impl fmt::Display for ValueKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Integer(i) => write!(f, "{i}"),
            Self::Pointer(_) => write!(f, "[ptr]"),
        }
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
            Self::Copy(r) => write!(f, "{r}"),
            Self::Not(r) => write!(f, "not {r}"),
            Self::Add(r1, r2) => write!(f, "{r1} + {r2}"),
            Self::Sub(r1, r2) => write!(f, "{r1} - {r2}"),
            Self::Mul(r1, r2) => write!(f, "{r1} * {r2}"),
            Self::Div(r1, r2) => write!(f, "{r1} / {r2}"),
            Self::Mod(r1, r2) => write!(f, "{r1} % {r2}"),
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
            Self::PrintSlice(r1, r2) => write!(f, "print_slice({r1}, {r2})"),
            Self::Allocate(r) => write!(f, "alloc {r}"),
            Self::Deallocate(r) => write!(f, "dealloc {r}"),
            Self::ReadSlice(src, i) => write!(f, "{src}[{i}]"),
            Self::WriteSlice(src, i, val) => write!(f, "{src}[{i}] = {val}"),
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

impl fmt::Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_internal(None, f)
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Self::fmt_internal(self.entry, &self.blocks, f)
    }
}
