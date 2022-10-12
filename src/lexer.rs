use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, multispace0},
    combinator::{all_consuming, fail, map, recognize},
    multi::{many0, many0_count},
    sequence::{pair, preceded, terminated},
    IResult,
};

#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
}

#[derive(Debug)]
pub enum TokenKind {
    Identifier(String),
    Punctuation(Punctuation),
}

#[derive(Clone, Copy, Debug)]
pub enum Punctuation {
    Semicolon,
}

const PUNCTUATION_MAP: &[(Punctuation, &str)] = &[(Punctuation::Semicolon, ";")];

pub fn lex(src: &str) -> Result<Vec<Token>, ()> {
    match all_consuming(terminated(many0(preceded(multispace0, token)), multispace0))(src) {
        Ok((_, nya)) => Ok(nya),
        _ => Err(()),
    }
}

pub fn token(input: &str) -> IResult<&str, Token> {
    alt((identifier, punctuation))(input)
}

pub fn identifier(input: &str) -> IResult<&str, Token> {
    map(
        recognize(pair(
            alt((alpha1, tag("_"))),
            many0_count(alt((alphanumeric1, tag("_")))),
        )),
        |x: &str| Token {
            kind: TokenKind::Identifier(x.to_owned()),
        },
    )(input)
}

pub fn punctuation(input: &str) -> IResult<&str, Token> {
    for &(p, s) in PUNCTUATION_MAP {
        if let Some(rest) = input.strip_prefix(s) {
            let token = Token {
                kind: TokenKind::Punctuation(p),
            };
            return Ok((rest, token));
        }
    }
    fail(input)
}
