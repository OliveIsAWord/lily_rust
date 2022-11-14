#![allow(unused_imports)]
mod lir;
mod optimizer;
mod verifier;

pub use optimizer::{optimize, optimize_interactive};
pub use verifier::{verify, verify_block};

use dbg_pls::{color, DebugPls, pretty};
use lir::{
    Block, BlockId, Branch, BranchPoint, Op, Program, Provenance, RegId, Register, ValueKind,
};
use parser::{Block as ParserBlock, ExprKind, Literal, Mutability, StatementKind};
use std::collections::HashMap;
use std::fmt;
use std::mem;

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
        self.assign(r, Op::Constant(ValueKind::Integer(69)));
        r
    }
    pub fn assign(&mut self, reg: Register, op: Op) {
        debug_assert!(!reg.is_input());
        //debug_assert!(!op.is_uninhabited());
        self.ops.push((Some(reg), op));
    }
    pub fn _add_nop(&mut self) {
        self.ops.push((None, Op::Nop));
    }
    pub fn _add_print(&mut self, reg: Register) {
        self.ops.push((None, Op::Print(reg)));
    }
    pub fn erase_register(&mut self, reg: Register) -> bool {
        debug_assert!(!reg.is_input());
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
    // pub fn set_entry(&mut self, id: BlockId) {
    //     self.entry_block = Some(id);
    // }
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
        Program::fmt_internal(self.entry_block.unwrap_or(0), &self.blocks, f)
    }
}

#[must_use]
pub fn compile(body: &ParserBlock) -> Program {
    let mut compiler = Compiler::new();
    compiler.compile_func(body);
    Program {
        entry: 42,
        blocks: compiler.pb.blocks,
    }
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
            None => self.bb.unit_register(),
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
            None => self.bb.unit_register(),
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
        //color!(expr, &self.vars, &self.bb);
        //pause();
        match expr {
            ExprKind::Literal(lit) => match lit {
                Literal::Integer(i) => {
                    let r = self.bb.new_register();
                    self.bb.assign(r, Op::Constant(ValueKind::Integer(*i)));
                    r
                }
                Literal::String(_) => todo!(),
            },
            ExprKind::Variable(v) => {
                self.get_var(v).1
                //color!(&v, r, &self.vars);
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
                    "not" => Op::Not(reg_args[0]),
                    "add" => Op::Add(reg_args[0], reg_args[1]),
                    "sub" => Op::Sub(reg_args[0], reg_args[1]),
                    "mul" => Op::Mul(reg_args[0], reg_args[1]),
                    "div" => Op::Div(reg_args[0], reg_args[1]),
                    "mod" => Op::Mod(reg_args[0], reg_args[1]),
                    "eq" => Op::Cmp(reg_args[0], reg_args[1]),
                    "print" => Op::Print(reg_args[0]),
                    "print_slice" => Op::PrintSlice(reg_args[0], reg_args[1]),
                    "alloc" => Op::Allocate(reg_args[0]),
                    "dealloc" => Op::Deallocate(reg_args[0]),
                    "get" => Op::ReadSlice(reg_args[0], reg_args[1]),
                    "set" => Op::WriteSlice(reg_args[0], reg_args[1], reg_args[2]),
                    f => panic!("Unrecognized function {f}"),
                };
                let r = self.bb.new_register();
                self.bb.assign(r, op);
                r
            }
            ExprKind::Block(b) => self.compile_parser_block(b),
            ExprKind::If(condition, if_parser_block, else_expr) => {
                // TODO: god, rewrite this code
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
                // Warning: imperative code.
                assert!(else_expr.is_none());
                let (begin_block, begin_vars) = self.change_to_new_state();
                let condition_id = self.bb.id;
                let evaled_condition = self.compile_expr(condition);
                let (condition_block, condition_vars) = self.change_to_new_state();
                let body_id = self.bb.id;
                let _ill_get_to_it = self.compile_parser_block(body);
                let (body_block, body_vars) = self.change_to_new_state();
                let endwhile_id = self.bb.id;
                let (begin_vars, condition_vars, body_vars) = (
                    vars_to_args(begin_vars),
                    vars_to_args(condition_vars),
                    vars_to_args(body_vars),
                );
                self.pb
                    .finish_block_jump(begin_block, BranchPoint::Block(condition_id, begin_vars));
                self.pb.finish_block_branch(
                    condition_block,
                    evaled_condition,
                    BranchPoint::Block(body_id, condition_vars.clone()),
                    BranchPoint::Block(endwhile_id, condition_vars),
                );
                self.pb
                    .finish_block_jump(body_block, BranchPoint::Block(condition_id, body_vars));
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

pub fn execute(p: &Program) {
    println!("Executing...");
    let val = execute_inner(&p.blocks, p.entry, vec![]);
    println!("Terminated with {val}");
}

fn execute_inner(
    p: &HashMap<BlockId, Block>,
    mut block_id: BlockId,
    mut params: Vec<ValueKind>,
) -> u64 {
    use std::num::NonZeroUsize;
    let mut heap_memory: HashMap<Provenance, Box<[Option<ValueKind>]>> = HashMap::new();
    let mut provenance_iota = NonZeroUsize::new(1).unwrap();
    let mut new_provenance = || {
        let p = provenance_iota;
        provenance_iota = provenance_iota
            .checked_add(1)
            .expect("ran out of heap pointers");
        Provenance(p)
    };
    loop {
        //println!("Running block {block_id}");
        // color!(&params);
        // pause();
        let block = p.get(&block_id).unwrap();
        let mut regs: HashMap<Register, ValueKind> = block
            .inputs
            .iter()
            .zip(&params)
            .map(|(&r, &v)| (r, v))
            .collect();
        let g = |regs: &HashMap<Register, ValueKind>, r| *regs.get(r).unwrap();
        let gi = |regs: &HashMap<Register, ValueKind>, r| match g(regs, r) {
            ValueKind::Integer(i) => i,
            ValueKind::Pointer(p) => panic!("expected integer, found pointer {p:?}"),
        };
        let gp = |regs: &HashMap<Register, ValueKind>, r| match g(regs, r) {
            ValueKind::Pointer(p) => p,
            ValueKind::Integer(i) => panic!("expected pointer, found integer {i}"),
        };
        let op2 = |regs: &HashMap<Register, ValueKind>, r1, r2, op: fn(u64, u64) -> u64| {
            let i1 = gi(regs, r1);
            let i2 = gi(regs, r2);
            ValueKind::Integer(op(i1, i2))
        };
        let uninhabited = ValueKind::Integer(0);
        for (reg, op) in &block.ops {
            //println!("{} = {}", pretty(reg), color(op));
            let val = match op {
                Op::Nop => uninhabited,
                Op::Constant(v) => *v,
                Op::Copy(r) => g(&regs, r),
                Op::Not(r) => ValueKind::Integer(u64::from(gi(&regs, r) == 0)),
                Op::Add(r1, r2) => op2(&regs, r1, r2, u64::wrapping_add),
                Op::Sub(r1, r2) => op2(&regs, r1, r2, u64::wrapping_sub),
                Op::Mul(r1, r2) => op2(&regs, r1, r2, u64::wrapping_mul),
                Op::Div(r1, r2) => op2(&regs, r1, r2, |a, b| {
                    a.checked_div(b).expect("division by zero")
                }),
                Op::Mod(r1, r2) => op2(&regs, r1, r2, |a, b| {
                    a.checked_rem(b).expect("division by zero")
                }),
                Op::Cmp(r1, r2) => op2(&regs, r1, r2, |x, y| u64::from(x == y)),
                // Op::Call(id, args) => {
                //     execute_inner(p, *id, args.iter().map(|r| g(&regs, r)).collect())
                // }
                Op::Print(r) => {
                    let x = g(&regs, r);
                    println!("Printing: {x}");
                    uninhabited
                }
                Op::PrintSlice(r1, r2) => {
                    let p = gp(&regs, r1);
                    let len = gi(&regs, r2);
                    let len = usize::try_from(len).unwrap();
                    let slice = &heap_memory.get(&p).expect("use after free")[..len];
                    print!("Printing: [");
                    for (i, x) in slice.iter().enumerate() {
                        if i > 0 {
                            print!(", ");
                        }
                        let x = x.expect("printed uninitialized memory");
                        print!("{x}");
                    }
                    println!("]");
                    uninhabited
                }
                Op::Allocate(r) => {
                    let p = new_provenance();
                    let size = gi(&regs, r);
                    let size = usize::try_from(size).unwrap();
                    heap_memory.insert(p, vec![None; size].into_boxed_slice());
                    ValueKind::Pointer(p)
                }
                Op::Deallocate(r) => {
                    let p = gp(&regs, r);
                    let _ = heap_memory.remove(&p).expect("double free");
                    uninhabited
                }
                Op::ReadSlice(r1, r2) => {
                    let p = gp(&regs, r1);
                    let i = gi(&regs, r2);
                    //println!("{p:?} {i}");
                    //color!(&heap_memory);
                    let i = usize::try_from(i).unwrap();
                    let slice = heap_memory.get(&p).expect("use after free");
                    slice
                        .get(i)
                        .expect(&format!(
                            "out of bounds access: {i} >= {}. {slice:?}",
                            slice.len()
                        ))
                        .expect("reading uninitialized memory")
                }
                Op::WriteSlice(r1, r2, r3) => {
                    let p = gp(&regs, r1);
                    let i = gi(&regs, r2);
                    let i = usize::try_from(i).unwrap();
                    let slice = heap_memory.get_mut(&p).expect("use after free");
                    *slice.get_mut(i).expect("out of bounds access") = Some(g(&regs, r3));
                    uninhabited
                } //e => todo!("{:?}", e),
            };
            if let Some(reg) = reg {
                let prev_value = regs.insert(*reg, val);
                debug_assert!(prev_value.is_none());
            }
            //color!(&regs);
        }
        let jump_to = match &block.exit {
            Branch::Jump(j) => j,
            Branch::Branch(r, j1, j2) => {
                if gi(&regs, r) > 0 {
                    j1
                } else {
                    j2
                }
            }
        };
        match jump_to {
            BranchPoint::Return(r) => {   
                if !heap_memory.is_empty() {
                    let len = heap_memory.len();
                    let s = if len == 1 {""} else {"s"};
                    println!("Leaked {len} allocation{s}.");
                }
                return gi(&regs, r);
            }
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
