mod lexer;
mod parser;

use std::fs::read_to_string;

fn main() {
    let sample = read_to_string("sample3.rs").unwrap();
    let tokens = lexer::lex(&sample).unwrap();
    println!("=== Tokens ===");
    println!("{:?}", tokens);
    let ast = parser::parse(&tokens).unwrap();
    println!("\n=== Items ===");
    println!("{:?}", ast);
}
