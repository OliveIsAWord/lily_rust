use crate::lexer::{Keyword, Punctuation, Token, TokenKind};
use nom::{
    bytes::complete::tag,
    combinator::{fail, map},
    IResult
};

pub type Ident = String;

#[derive(Debug)]
pub enum StatementKind {
    LetStatement(Ident, Ident),
}

pub fn parse(input: &[Token]) -> Result<StatementKind, ()> {
    match let_statement(input) {
        Ok((_, x)) => Ok(x),
        Err(_) => Err(()),
    }
}

// pub fn let_statement(input: &[Token]) -> IResult<&[Token], StatementKind> {
//     map(tag(todo!()), |_| {
//         StatementKind::LetStatement("left".to_owned(), "right".to_owned())
//     })(input)
// }

// pub fn let_statement(input: &[Token]) -> IResult<&[Token], StatementKind> {
//     if let [Token {
//         kind: TokenKind::Keyword(Keyword::Let),
//     }, Token {
//         kind: TokenKind::Identifier(left),
//     }, Token {
//         kind: TokenKind::Punctuation(Punctuation::Eq),
//     }, Token {
//         kind: TokenKind::Identifier(right),
//     }, rest @ ..] = input
//     {
//         Ok((rest, StatementKind::LetStatement(left.clone(), right.clone())))
//     } else {
//         fail(input)
//     }
// }
