mod lexer;

use std::fs::read_to_string;

fn main() {
    let sample = read_to_string("sample2.rs").unwrap();
    let tokens = lexer::lex(&sample).unwrap();
    println!("{:?}", tokens);
}
