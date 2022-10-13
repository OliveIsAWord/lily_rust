use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, multispace0},
    combinator::{all_consuming, fail, map, recognize},
    multi::{many0, many0_count},
    sequence::{pair, preceded, terminated},
    IResult,
};

#[derive(Debug, Eq, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
}

#[derive(Debug, Eq, PartialEq)]
pub enum TokenKind {
    Identifier(String),
    Punctuation(Punctuation),
    Keyword(Keyword),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Punctuation {
    BraceOpen,
    BraceClose,
    ParenOpen,
    ParenClose,
    Semicolon,
    EqEq,
    FatArrow,
    Eq,
    PathSep,
    Colon,
    ThinArrow,
}

const PUNCTUATION_MAP: &[(Punctuation, &str)] = &[
    (Punctuation::BraceOpen, "{"),
    (Punctuation::BraceClose, "}"),
    (Punctuation::ParenOpen, "("),
    (Punctuation::ParenClose, ")"),
    (Punctuation::Semicolon, ";"),
    (Punctuation::EqEq, "=="),
    (Punctuation::FatArrow, "=>"),
    (Punctuation::Eq, "="),
    (Punctuation::PathSep, "::"),
    (Punctuation::Colon, ":"),
    (Punctuation::ThinArrow, "->"),
];

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Keyword {
    Fn,
    Let,
}

const KEYWORD_MAP: &[(Keyword, &str)] = &[(Keyword::Fn, "fn"), (Keyword::Let, "let")];

pub fn lex(src: &str) -> Result<Vec<Token>, ()> {
    match all_consuming(terminated(many0(preceded(multispace0, token)), multispace0))(src) {
        Ok((_, nya)) => Ok(nya),
        _ => Err(()),
    }
}

pub fn token(input: &str) -> IResult<&str, Token> {
    alt((punctuation, keyword, identifier))(input)
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
    PUNCTUATION_MAP
        .iter()
        .find_map(|&(p, s)| {
            // Check for punctuation at start of string
            input.strip_prefix(s).map(|rest| {
                (
                    rest,
                    Token {
                        kind: TokenKind::Punctuation(p),
                    },
                )
            })
        })
        .ok_or(())
        .or_else(|_| fail(input))
}

pub fn keyword(input: &str) -> IResult<&str, Token> {
    KEYWORD_MAP
        .iter()
        .find_map(|&(kw, s)| {
            input
                // Check for keyword at start of string
                .strip_prefix(s)
                // Skip if immediately followed by [a-zA-Z0-9_], since that's an identifier
                // e.g. `let_it_be`
                .filter(|rest| {
                    !matches!(
                        rest.chars().next(),
                        Some(v) if v.is_ascii_alphanumeric() || v == '_'
                    )
                })
                .map(|rest| {
                    (
                        rest,
                        Token {
                            kind: TokenKind::Keyword(kw),
                        },
                    )
                })
        })
        .ok_or(())
        .or_else(|_| fail(input))
}
