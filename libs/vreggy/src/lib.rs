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
        //Copy(R),
        Not(R),
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
                //Self::Copy(r) => write!(f, "{r}"),
                Self::Not(r) => write!(f, "not {r}"),
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
    fn get_var(&self, x: &str) -> (Mutability, Register) {
        self.vars
            .iter()
            .rev()
            .find_map(|&(var, m, r)| (var == x).then_some((m, r)))
            .unwrap()
    }
    fn get_var_mut(&mut self, x: &str) -> (Mutability, &mut Register) {
        self.vars
            .iter_mut()
            .rev()
            .find_map(|&mut (var, m, ref mut r)| (var == x).then_some((m, r)))
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
                assert!(did_erase || r.is_input());
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
            ExprKind::Assign(var, expr) => {
                let new_r = self.compile_expr(expr);
                let (m, r) = self.get_var_mut(var);
                assert!(m == Mutability::Mut, "assigning value to non-mut variable");
                *r = new_r;
                self.bb.unit_register()
            }
            ExprKind::Call(func, args) => {
                let func = match func.as_ref() {
                    ExprKind::Variable(s) => s,
                    _ => todo!(),
                };
                let reg_args: Vec<_> = args.iter().map(|e| self.compile_expr(e)).collect();
                let op = match func.as_ref() {
                    "add" => Op::Add(reg_args[0], reg_args[1]),
                    "mul" => Op::Mul(reg_args[0], reg_args[1]),
                    "eq" => Op::Cmp(reg_args[0], reg_args[1]),
                    "not" => Op::Not(reg_args[0]),
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
                    let new_vars = vars_to_args(new_vars);
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
                let beginif_vars = vars_to_args(beginif_vars);
                self.pb.finish_block_branch(
                    beginif_block,
                    condition_reg,
                    BranchPoint::Block(if_block_id, beginif_vars.clone()),
                    BranchPoint::Block(else_block_id, beginif_vars),
                );
                evaled_reg
            }
            ExprKind::While(condition, body, else_expr) => {
                assert!(else_expr.is_none());
                let (begin_block, begin_vars) = self.change_to_new_state();
                let evaled_condition = self.compile_expr(condition);
                let (condition_block, condition_vars) = self.change_to_new_state();
                let _ill_get_to_it = self.compile_parser_block(body);
                let (body_block, body_vars) = self.change_to_new_state();
                let (begin_vars, condition_vars, body_vars) = (
                    vars_to_args(begin_vars),
                    vars_to_args(condition_vars),
                    vars_to_args(body_vars),
                );
                self.pb.finish_block_jump(
                    begin_block,
                    BranchPoint::Block(condition_block.id, begin_vars),
                );
                let condition_block_id = condition_block.id;
                self.pb.finish_block_branch(
                    condition_block,
                    evaled_condition,
                    BranchPoint::Block(body_block.id, condition_vars.clone()),
                    BranchPoint::Block(self.bb.id, condition_vars),
                );
                self.pb.finish_block_jump(
                    body_block,
                    BranchPoint::Block(condition_block_id, body_vars),
                );
                self.bb.unit_register()
            }
        }
    }
    fn change_state(
        &mut self,
        state: (BlockBuilder, Vec<(&'a str, Mutability, Register)>),
    ) -> (BlockBuilder, Vec<(&'a str, Mutability, Register)>) {
        let old_block = mem::replace(&mut self.bb, state.0);
        let old_vars = mem::replace(&mut self.vars, state.1);
        (old_block, old_vars)
    }
    fn change_to_new_state(&mut self) -> (BlockBuilder, Vec<(&'a str, Mutability, Register)>) {
        let x = self.make_child_scope();
        self.change_state(x)
    }
}

fn vars_to_args(vars: Vec<(&str, Mutability, Register)>) -> Vec<Register> {
    vars.into_iter().map(|(_, _, r)| r).collect()
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
        // println!("Running block {block_id}");
        // color!(&params);
        // pause();
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
                //Op::Copy(r) => g(&regs, r),
                Op::Not(r) => u64::from(g(&regs, r) == 0),
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
