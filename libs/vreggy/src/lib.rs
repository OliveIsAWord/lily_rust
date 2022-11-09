#![allow(dead_code, unused_imports)]
use dbg_pls::{color, DebugPls};
use parser::{Block as ParserBlock, ExprKind, Ident, Literal, Mutability, StatementKind};
use std::collections::HashMap;
use std::fmt;
use std::iter::once;
use std::mem;
use std::num::NonZeroI32;

type RegId = NonZeroI32;
type BlockId = u32;

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct Register(RegId);

impl Register {
    #[must_use]
    pub const fn is_input(self) -> bool {
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

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_input() {
            write!(f, "i{}", -self.0.get())
        } else {
            write!(f, "r{}", self.0.get())
        }
    }
}

mod clean {
    use super::*;
    type R = Register;
    #[derive(Debug, DebugPls)]
    pub enum Op {
        Constant(u64),
        Copy(R),
        Add(R, R),
        Sub(R, R),
        Mul(R, R),
        Cmp(R, R),
        Call(BlockId, Vec<R>),
    }

    impl fmt::Display for Op {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                Self::Constant(v) => write!(f, "{v}"),
                Self::Copy(r) => write!(f, "{r}"),
                Self::Add(r1, r2) => write!(f, "{r1} + {r2}"),
                Self::Sub(r1, r2) => write!(f, "{r1} - {r2}"),
                Self::Mul(r1, r2) => write!(f, "{r1} * {r2}"),
                Self::Cmp(r1, r2) => write!(f, "{r1} == {r2}"),
                Self::Call(id, regs) => {
                    write!(f, "block_{id}(")?;
                    for (i, r) in regs.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{r}")?;
                    }
                    write!(f, ")")
                }
            }
        }
    }
}
pub use clean::*;

#[derive(Debug, DebugPls)]
enum Branch {
    Jump(BranchPoint),
    Branch(Register, BranchPoint, BranchPoint),
}

#[derive(Debug, DebugPls)]
pub enum BranchPoint {
    Block(BlockId, Vec<Register>),
    Return(Register),
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
    const fn new(id: BlockId) -> Self {
        Self {
            reg_accum: 0,
            input_accum: 0,
            id,
            inputs: vec![],
            ops: vec![],
        }
    }
    pub const fn id(&self) -> BlockId {
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
    pub fn unit_register(&mut self) -> Register {
        let reg_id = self.reg_accum.checked_add(1).and_then(RegId::new).unwrap();
        self.reg_accum = reg_id.get();
        let r = Register(reg_id);
        self.assign(r, Op::Constant(69));
        r
    }
    pub fn assign(&mut self, r: Register, op: Op) {
        debug_assert!(!r.is_input());
        self.ops.push((Some(r), op));
    }
    pub fn erase_register(&mut self, reg: Register) -> bool {
        self.ops
            .iter_mut()
            .rev()
            .find_map(|(r, _)| (*r == Some(reg)).then(|| *r = None))
            .is_some()
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

impl fmt::Display for ProgramBuilder {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(entry_id) = self.entry_block {
            writeln!(f, "entry = block_{entry_id}()")?;
        }
        let mut blocks: Vec<_> = self.blocks.iter().collect();
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

pub fn compile(body: &ParserBlock) {
    //_fact();
    let mut compiler = Compiler::new();
    compiler.compile_func(body);
    compiler.pb.set_entry(42);
    println!("{}", compiler.pb);
    meow(&compiler.pb);
}

struct Compiler<'a> {
    pb: ProgramBuilder,
    bb: BlockBuilder,
    vars: Vec<(&'a str, Mutability, Register)>,
}

impl<'a> Compiler<'a> {
    fn new() -> Self {
        let mut pb = ProgramBuilder::new();
        let bb = pb.create_block();
        let vars = vec![];
        Self { pb, bb, vars }
    }
    fn scopes(&self) -> impl Iterator<Item = &(&'a str, Mutability, Register)> {
        self.vars.iter().rev()
    }
    fn get_var(&self, x: &str) -> (Mutability, Register) {
        self.scopes()
            .find_map(|&(var, m, r)| (var == x).then_some((m, r)))
            .unwrap()
    }
    fn compile_func(&mut self, func: &'a ParserBlock) {
        for s in &func.statements {
            self.compile_statement(s);
        }
        let return_reg = match &func.tail {
            Some(e) => self.compile_expr(e),
            None => {
                let null = self.bb.new_register();
                self.bb.assign(null, Op::Constant(69));
                null
            }
        };
        let bb = mem::replace(&mut self.bb, self.pb.create_block());
        self.pb
            .finish_block_jump(bb, BranchPoint::Return(return_reg));
    }
    fn compile_statement(&mut self, statement: &'a StatementKind) {
        match statement {
            StatementKind::Empty => (),
            StatementKind::Item(_) => todo!("local items"),
            StatementKind::LetStatement(ident, mutability, val) => {
                let r = self.compile_expr(val.as_ref().unwrap());
                self.vars.push((ident, *mutability, r));
            }
            StatementKind::ExprStatement(e) => {
                let r = self.compile_expr(e);
                let did_erase = self.bb.erase_register(r);
                assert!(did_erase);
            }
        }
    }
    fn compile_parser_block(&mut self, block: &'a ParserBlock) -> Register {
        let ParserBlock { statements, tail } = block;
        let old_vars_len = self.vars.len();
        for s in statements {
            self.compile_statement(s);
        }
        let r = match tail {
            Some(e) => self.compile_expr(e),
            None => {
                let null = self.bb.new_register();
                self.bb.assign(null, Op::Constant(0));
                null
            }
        };
        debug_assert!(self.vars.len() >= old_vars_len);
        self.vars.truncate(old_vars_len);
        r
    }
    fn make_child_scope(&mut self) -> (BlockBuilder, Vec<(&'a str, Mutability, Register)>) {
        let mut bb = self.pb.create_block();
        let vars = self
            .vars
            .iter()
            .map(|&(var, m, _)| (var, m, bb.new_input()))
            .collect();
        (bb, vars)
    }
    fn compile_expr(&mut self, expr: &'a ExprKind) -> Register {
        //color!(expr, &self.vars, &self.nonlocal_vars);
        match expr {
            ExprKind::Literal(lit) => match lit {
                Literal::Integer(i) => {
                    let r = self.bb.new_register();
                    self.bb.assign(r, Op::Constant(*i));
                    r
                }
                Literal::String(_) => todo!(),
            },
            ExprKind::Variable(v) => {
                let r = self.get_var(v).1;
                color!(&v, r, &self.vars);
                r
            }
            ExprKind::Assign(_var, _expr) => todo!(),
            ExprKind::Call(func, args) => {
                let func = match func.as_ref() {
                    ExprKind::Variable(s) => s,
                    _ => todo!(),
                };
                let reg_args: Vec<_> = args.iter().map(|e| self.compile_expr(e)).collect();
                let op = match func.as_ref() {
                    "add" => Op::Add(reg_args[0], reg_args[1]),
                    f => panic!("Unrecognized function {f}"),
                };
                let r = self.bb.new_register();
                self.bb.assign(r, op);
                r
            }
            ExprKind::Block(b) => self.compile_parser_block(b),
            ExprKind::If(condition, if_parser_block, else_expr) => {
                let else_parser_block =
                    if let ExprKind::Block(b) = else_expr.as_ref().unwrap().as_ref() {
                        b
                    } else {
                        todo!("else expr")
                    };
                let condition_reg = self.compile_expr(condition);
                let mut uwu = |new_parser_block| {
                    let (new_block, new_vars) = self.make_child_scope();
                    let old_block = mem::replace(&mut self.bb, new_block);
                    let old_vars = mem::replace(&mut self.vars, new_vars);
                    let new_evaled = self.compile_parser_block(new_parser_block);
                    let new_block = mem::replace(&mut self.bb, old_block);
                    let new_vars = mem::replace(&mut self.vars, old_vars);
                    let new_vars: Vec<_> = new_vars.into_iter().map(|(_, _, r)| r).collect();
                    (new_block, new_evaled, new_vars)
                };
                let (if_block, if_evaled, mut if_vars) = uwu(if_parser_block);
                let (else_block, else_evaled, mut else_vars) = uwu(else_parser_block);
                let (mut endif_block, endif_vars) = self.make_child_scope();
                let (if_block_id, else_block_id, endif_block_id) =
                    (if_block.id, else_block.id, endif_block.id);
                let evaled_reg = endif_block.new_input();
                if_vars.push(if_evaled);
                else_vars.push(else_evaled);
                self.pb
                    .finish_block_jump(if_block, BranchPoint::Block(endif_block_id, if_vars));
                self.pb
                    .finish_block_jump(else_block, BranchPoint::Block(endif_block_id, else_vars));
                let beginif_block = mem::replace(&mut self.bb, endif_block);
                let beginif_vars = mem::replace(&mut self.vars, endif_vars);
                let beginif_vars: Vec<_> = beginif_vars.into_iter().map(|(_, _, r)| r).collect();
                self.pb.finish_block_branch(
                    beginif_block,
                    condition_reg,
                    BranchPoint::Block(if_block_id, beginif_vars.clone()),
                    BranchPoint::Block(else_block_id, beginif_vars),
                );
                evaled_reg
            }
            ExprKind::While(..) => todo!(),
            // ExprKind::If(condition, if_parser_block, else_expr) => {
            //     let condition_reg = self.compile_expr(condition);
            //     let parent_scope: Vec<(&'a str, (Mutability, Register))> = {
            //         let mut flattened = HashMap::new();
            //         for (&var, &reg) in self.scopes().flatten() {
            //             flattened.entry(var).or_insert(reg);
            //         }
            //         flattened.into_iter().collect()
            //     };
            //     let branch_args: Vec<_> = parent_scope.iter().map(|&(_, (_, r))| r).collect();
            //     color!(&parent_scope);
            //     fn create_branch_scope<'a>(
            //         parent_scope: &[(&'a str, (Mutability, Register))],
            //         bb: &mut BlockBuilder,
            //     ) -> HashMap<&'a str, (Mutability, Register)> {
            //         parent_scope
            //             .iter()
            //             .copied()
            //             .map(|(var, (m, _))| (var, (m, bb.new_input())))
            //             .collect()
            //     }
            //     type Self_<'ignore> = Compiler<'ignore>;
            //     fn create_branch_block<'c, T, F>(
            //         self_: &mut Self_<'c>,
            //         ast: &'c T,
            //         compiler_func: F,
            //         parent_scope: &[(&'c str, (Mutability, Register))],
            //     ) -> (Register, BlockBuilder)
            //     where
            //         F: FnOnce(&mut Self_<'c>, &'c T) -> Register,
            //     {
            //         let old_block = mem::replace(&mut self_.bb, self_.pb.create_block());
            //         let new_block_vars = create_branch_scope(parent_scope, &mut self_.bb);
            //         color!(&new_block_vars);
            //         self_.nonlocal_vars.push(new_block_vars);
            //         assert_eq!(self_.nonlocal_vars.len(), 1);
            //         let r = compiler_func(self_, ast);
            //         assert_eq!(self_.nonlocal_vars.len(), 1);
            //         let new_block = mem::replace(&mut self_.bb, old_block);
            //         (r, new_block)
            //     }
            //     let (if_reg, if_block) = create_branch_block(
            //         self,
            //         if_parser_block,
            //         Self::compile_parser_block,
            //         &parent_scope,
            //     );
            //     color!(&if_block);
            //     let if_vars = self.nonlocal_vars.pop().unwrap();
            //     let if_vars: Vec<_> = parent_scope
            //         .iter()
            //         .map(|(var, _)| if_vars.get(var).unwrap().1)
            //         .chain(once(if_reg))
            //         .collect();
            //     let (else_reg, else_block) = match else_expr {
            //         Some(e) => {
            //             create_branch_block(self, e.as_ref(), Self::compile_expr, &parent_scope)
            //         }
            //         None => {
            //             let mut bb = self.pb.create_block();
            //             for _ in 0..parent_scope.len() {
            //                 let _ = bb.new_input();
            //             }
            //             let r = bb.unit_register();
            //             (r, bb)
            //         }
            //     };
            //     let else_vars = self.nonlocal_vars.pop().unwrap();
            //     let else_vars: Vec<_> = parent_scope
            //         .iter()
            //         .map(|(var, _)| else_vars.get(var).unwrap().1)
            //         .chain(once(else_reg))
            //         .collect();
            //     color!(&if_vars, &else_vars);
            //     let start_if_block = mem::replace(&mut self.bb, self.pb.create_block());
            //     self.pb.finish_block_branch(
            //         start_if_block,
            //         condition_reg,
            //         BranchPoint::Block(if_block.id, branch_args.clone()),
            //         BranchPoint::Block(else_block.id, branch_args),
            //     );
            //     let _ = self.bb.new_input();
            //     let evaled_reg = self.bb.new_input();
            //     self.pb.finish_block_jump(
            //         if_block,
            //         BranchPoint::Block(self.bb.id, if_vars),
            //     );
            //     self.pb.finish_block_jump(
            //         else_block,
            //         BranchPoint::Block(self.bb.id, else_vars),
            //     );
            //     //let new_vars = create_branch_scope(&parent_scope, &mut self.bb);
            //     evaled_reg
            // }
        }
    }
}

fn _fact() {
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
    println!("{}", pb);
    meow(&pb);
}

fn meow(pb: &ProgramBuilder) {
    println!("Executing...");
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
            r.0.get()
                .checked_add(1)
                .and_then(i32::checked_neg)
                .and_then(|x| usize::try_from(x).ok())
                .map_or_else(|| *regs.get(r).unwrap(), |i| params[i])
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
                Op::Cmp(r1, r2) => op2(&regs, r1, r2, |x, y| u64::from(x == y)),
                Op::Call(id, args) => execute(p, *id, args.iter().map(|r| g(&regs, r)).collect()),
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
                if g(&regs, r) > 0 {
                    j1
                } else {
                    j2
                }
            }
        };
        match jump_to {
            BranchPoint::Return(r) => return g(&regs, r),
            BranchPoint::Block(id, args) => {
                block_id = *id;
                params = args.iter().map(|r| g(&regs, r)).collect();
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
    stdin().read_line(&mut String::new()).unwrap();
}
