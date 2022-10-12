mod lexer;

fn main() {
    let sample = "   ;  _val;id_1dent ";
    let tokens = lexer::lex(sample).unwrap();
    println!("{:?}", tokens);
}
