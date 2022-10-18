use cranelift::prelude::*;
use cranelift::codegen::ir::Function;
use parser::StatementKind;

pub fn compile(b: &[StatementKind]) {
    let mut ctx = FunctionBuilderContext::new();
    let mut func = Function::new();
    let builder = FunctionBuilder::new(&mut func, &mut ctx);
    todo!()
}