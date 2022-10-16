mod lexer;
mod parser;

use std::fs::read_to_string;

use dbg_pls::color;

fn main() {
    let sample = read_to_string("sample5.rs").unwrap();
    let tokens = lexer::lex(&sample).unwrap();
    // println!("=== Tokens ===");
    // println!("{:?}\n", color(&tokens));
    let ast = parser::parse(&tokens).unwrap();
    println!("=== Items ===");
    println!("{:?}", color(&ast));
}
