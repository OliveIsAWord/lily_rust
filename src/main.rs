use std::fs::read_to_string;

use dbg_pls::color;

fn main() {
    let sample = read_to_string("example5.rs").unwrap();
    //let sample = read_to_string("nested_loops.rs").unwrap();

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
    let mut ir = vreggy::compile(block);
    //println!("=== Register Machine Instructions ===");
    //println!("{}", ir);
    vreggy::verify(&ir).unwrap();
    //vreggy::optimize_interactive(&mut ir);
    if vreggy::optimize(&mut ir) {
        println!("=== Optimized ===");
        println!("{}", ir);
        // let did_regress = include_str!("../did_regress.txt").trim();
        // let meow = format!("{}", ir);
        // let nya = meow.trim();
        // if nya != did_regress {
        //     let i = nya.chars().zip(did_regress.chars()).position(|(a, b)| a != b).unwrap();
        //     panic!("regression at char {i}!!!");
        // }
    } else {
        println!("Could not optimize further.");
    }

    let mut rs_source = String::new();
    codegen_rs::gen_code(&ir, &mut rs_source).unwrap();
    //println!("\n{rs_source}");
    //vreggy::execute(&ir);
}
