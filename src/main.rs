use std::fs::read_to_string;

use dbg_pls::color;

fn main() {
    let sample = read_to_string("example4.rs").unwrap();

    let tokens = lexer::lex(&sample).unwrap();
    // println!("\n=== Tokens ===");
    // for (i, t) in tokens.iter().map(|t| &t.kind).enumerate() {
    //     println!("{i} {:?}", color(t));
    // }

    let ast = parser::parse(&tokens).unwrap();
    // println!("\n=== Items ===");
    // for item in &ast {
    //     println!("{:?}", color(item));
    // }

    assert_eq!(ast.len(), 1);
    let main_ = match &ast[0] {
        parser::ItemKind::Fn(f) => f,
        e => panic!("{:?}", color(e)),
    };
    assert_eq!(main_.name, "main");
    assert!(main_.params.is_empty());

    let block = &main_.body;
    //println!("{:?}", color(block));
    println!("Compiling...");
    vreggy::compile(block);
    //println!("=== Register Machine Instructions ===");
    //println!("{:?}", color(&metrocop));
}
