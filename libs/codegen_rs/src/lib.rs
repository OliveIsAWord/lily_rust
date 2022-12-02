use std::fmt::{Result as FmtResult, Write};
use vreggy::{Branch, BranchPoint, Op, Program};

pub fn gen_code<F: Write>(program: &Program, f: &mut F) -> FmtResult {
    writeln!(f, "enum State {{")?;
    for (id, block) in &program.blocks {
        write!(f, "    S{id}(")?;
        for (i, _input) in block.inputs.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "u64")?;
        }
        writeln!(f, "),")?;
    }
    writeln!(f, "}}")?;
    writeln!(f, "use State::*;")?;
    writeln!(f, "\nfn main() {{")?;
    writeln!(f, "    let exit_val = lily_entry();")?;
    writeln!(f, "    println!(\"Exited with code {{exit_val}}\");")?;
    writeln!(f, "}}")?;
    writeln!(f, "\nfn lily_entry() -> u64 {{")?;
    writeln!(f, "    let mut state = S{}();", program.entry)?;
    writeln!(f, "    loop {{")?;
    writeln!(f, "        state = match state {{")?;
    for (id, block) in &program.blocks {
        write!(f, "            S{id}(")?;
        for (i, input) in block.inputs.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{input}")?;
        }
        writeln!(f, ") => {{")?;
        for (reg, op) in &block.ops {
            write!(f, "                ")?;
            if let Some(r) = reg {
                write!(f, "let {r} = ")?;
            }
            match op {
                Op::Nop => write!(f, "0"),
                Op::Constant(v) => write!(f, "{v}"),
                Op::Copy(r) => write!(f, "{r}"),
                Op::Not(r) => write!(f, "if {r} != 0 {{ 0 }} else  {{ 1 }}"),
                Op::Add(r1, r2) => write!(f, "{r1}.wrapping_add({r2})"),
                Op::Sub(r1, r2) => write!(f, "{r1}.wrapping_sub({r2})"),
                Op::Mul(r1, r2) => write!(f, "{r1}.wrapping_mul({r2})"),
                Op::Div(r1, r2) => write!(f, "{r1} / {r2}"),
                Op::Mod(r1, r2) => write!(f, "{r1} % {r2}"),
                Op::Cmp(r1, r2) => write!(f, "u64::from({r1} == {r2})"),
                Op::Print(r) => write!(f, "println!(\"{{{r}}}\")"),
                e => todo!("{}", e),
            }?;
            writeln!(f, ";")?;
        }
        write!(f, "                ")?;
        let fmt_bp = |f: &mut F, bp: &BranchPoint| -> FmtResult {
            match bp {
                BranchPoint::Return(r) => write!(f, "return {r}"),
                BranchPoint::Block(id, args) => {
                    write!(f, "S{id}(")?;
                    for (i, arg) in args.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{arg}")?;
                    }
                    write!(f, ")")
                }
            }
        };
        match &block.exit {
            Branch::Jump(bp) => fmt_bp(f, bp)?,
            Branch::Branch(r, bp1, bp2) => {
                write!(f, "if {r} != 0 {{ ")?;
                fmt_bp(f, bp1)?;
                write!(f, " }} else {{ ")?;
                fmt_bp(f, bp2)?;
                write!(f, " }}")?;
            }
        }
        writeln!(f, "\n            }}")?;
    }
    writeln!(f, "        }}")?;
    writeln!(f, "    }}")?;
    write!(f, "}}")?;
    Ok(())
}
