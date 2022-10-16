// The following lint should only be silenced until the upstream issue in `dbg_pls` is resolved
#![allow(clippy::use_self)]

mod lexer;
mod parser;

use std::fs::read_to_string;

use dbg_pls::color;

fn main() {
    let sample = read_to_string("sample4.rs").unwrap();
    let tokens = lexer::lex(&sample).unwrap();
    // println!("=== Tokens ===");
    // println!("{:?}\n", color(&tokens));
    let ast = parser::parse(&tokens).unwrap();
    println!("=== Items ===");
    println!("{:?}", color(&ast));
}
