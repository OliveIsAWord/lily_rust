use super::{Block, Branch, BranchPoint, Program};

#[derive(Debug)]
pub enum ProgramErrorKind {
    InvalidInputRegister,
    InvalidWorkingRegister,
    AssigningUninhabitedValue,
    IncorrectArguments,
    InvalidBlockId,
}

#[cfg(debug_assertions)]
pub fn verify(program: &Program) -> Result<(), ProgramErrorKind> {
    // check every BranchPoint::Block has a valid id
    // check every Block call/jump has the correct number of args
    for block in program.blocks.values() {
        verify_block(block)?;
        verify_block_with_program(block, program)?;
    }
    Ok(())
}

#[cfg(debug_assertions)]
pub fn verify_block(block: &Block) -> Result<(), ProgramErrorKind> {
    if block.inputs.iter().find(|r| !r.is_input()).is_some() {
        return Err(ProgramErrorKind::InvalidInputRegister);
    }
    for (reg, op) in &block.ops {
        // check every register is only used after it's defined
        let Some(reg) = reg else { continue};
        if reg.is_input() {
            return Err(ProgramErrorKind::InvalidWorkingRegister);
        }
        if op.is_uninhabited() {
            return Err(ProgramErrorKind::AssigningUninhabitedValue);
        }
    }
    Ok(())
}

pub fn verify_block_with_program(block: &Block, program: &Program) -> Result<(), ProgramErrorKind> {
    let check_num_args = |bp: &BranchPoint| match bp {
        BranchPoint::Block(id, args)
            if program
                .blocks
                .get(&id)
                .ok_or(ProgramErrorKind::InvalidBlockId)?
                .inputs
                .len()
                != args.len() =>
        {
            println!("MEOWMEOWMEOW {id}");
            Err(ProgramErrorKind::IncorrectArguments)
        }
        _ => Ok(()),
    };
    match &block.exit {
        Branch::Jump(bp) => check_num_args(bp),
        Branch::Branch(_, bp1, bp2) => {
            check_num_args(bp1)?;
            check_num_args(bp2)
        }
    }
}

#[cfg(not(debug_assertions))]
#[inline(always)]
pub fn verify(_program: &Program) -> Result<(), ProgramErrorKind> {
    Ok(())
}

#[cfg(not(debug_assertions))]
#[inline(always)]
pub fn verify_block(_block: &Block) -> Result<(), ProgramErrorKind> {
    Ok(())
}
