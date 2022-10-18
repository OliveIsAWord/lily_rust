use std::fs::read_to_string;

use dbg_pls::color;

fn main() {
    let sample = read_to_string("example.rs").unwrap();

    let tokens = lexer::lex(&sample).unwrap();
    // println!("\n=== Tokens ===");
    // for t in tokens.iter().map(|t| &t.kind) {
    //     println!("{:?}", color(t));
    // }

    let ast = parser::parse(&tokens).unwrap();
    println!("\n=== Items ===");
    for item in &ast {
        println!("{:?}", color(item));
    }
}
