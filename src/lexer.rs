// use dbg_pls::DebugPls;
// use nom::character::complete::satisfy;
// use nom::{
//     branch::alt,
//     bytes::complete::is_not,
//     character::complete::{char as char_, multispace0, u64 as u64_},
//     combinator::{all_consuming, fail, map, recognize},
//     multi::{many0, many0_count},
//     sequence::{delimited, pair, preceded, terminated},
//     IResult,
// };

// #[derive(Clone, Debug, DebugPls, Eq, Hash, PartialEq)]
// pub struct Token {
//     pub kind: TokenKind,
// }

// #[derive(Clone, Debug, DebugPls, Eq, Hash, PartialEq)]
// pub enum TokenKind {
//     Identifier(String),
//     Punctuation(Punctuation),
//     Keyword(Keyword),
//     Literal(Literal),
// }

// #[derive(Clone, Debug, DebugPls, Eq, Hash, PartialEq)]
// pub enum Literal {
//     Integer(u64),
//     String(String),
// }

// #[derive(Clone, Copy, Debug, DebugPls, Eq, Hash, PartialEq)]
// pub enum Punctuation {
//     BraceOpen,
//     BraceClose,
//     Comma,
//     ParenOpen,
//     ParenClose,
//     Semicolon,
//     AndAnd,
//     And,
//     EqEq,
//     FatArrow,
//     Eq,
//     PathSep,
//     Colon,
//     ThinArrow,
// }

// const PUNCTUATION_MAP: &[(Punctuation, &str)] = &[
//     (Punctuation::BraceOpen, "{"),
//     (Punctuation::BraceClose, "}"),
//     (Punctuation::Comma, ","),
//     (Punctuation::ParenOpen, "("),
//     (Punctuation::ParenClose, ")"),
//     (Punctuation::Semicolon, ";"),
//     (Punctuation::AndAnd, "&&"),
//     (Punctuation::And, "&"),
//     (Punctuation::EqEq, "=="),
//     (Punctuation::FatArrow, "=>"),
//     (Punctuation::Eq, "="),
//     (Punctuation::PathSep, "::"),
//     (Punctuation::Colon, ":"),
//     (Punctuation::ThinArrow, "->"),
// ];

// #[derive(Clone, Copy, Debug, DebugPls, Eq, Hash, PartialEq)]
// pub enum Keyword {
//     As,
//     Else,
//     If,
//     Fn,
//     Let,
//     Mut,
//     While,
// }

// const KEYWORD_MAP: &[(Keyword, &str)] = &[
//     (Keyword::As, "as"),
//     (Keyword::Else, "else"),
//     (Keyword::If, "if"),
//     (Keyword::Fn, "fn"),
//     (Keyword::Let, "let"),
//     (Keyword::Mut, "mut"),
//     (Keyword::While, "while"),
// ];

// pub fn lex(src: &str) -> Result<Vec<Token>, ()> {
//     match all_consuming(terminated(many0(preceded(multispace0, token)), multispace0))(src) {
//         Ok((_, nya)) => Ok(nya),
//         _ => Err(()),
//     }
// }

// pub fn token(input: &str) -> IResult<&str, Token> {
//     alt((literal, punctuation, keyword, identifier))(input)
// }

// pub fn literal(input: &str) -> IResult<&str, Token> {
//     let number = map(u64_, |n| Token {
//         kind: TokenKind::Literal(Literal::Integer(n)),
//     });
//     let string = map(
//         delimited(char_('"'), is_not("\""), char_('"')),
//         |s: &str| Token {
//             kind: TokenKind::Literal(Literal::String(s.to_owned())),
//         },
//     );
//     alt((number, string))(input)
// }

// pub fn punctuation(input: &str) -> IResult<&str, Token> {
//     PUNCTUATION_MAP
//         .iter()
//         .find_map(|&(p, s)| {
//             // Check for punctuation at start of string
//             input.strip_prefix(s).map(|rest| {
//                 (
//                     rest,
//                     Token {
//                         kind: TokenKind::Punctuation(p),
//                     },
//                 )
//             })
//         })
//         .ok_or(())
//         .or_else(|_| fail(input))
// }

// pub fn keyword(input: &str) -> IResult<&str, Token> {
//     KEYWORD_MAP
//         .iter()
//         .find_map(|&(kw, s)| {
//             input
//                 // Check for keyword at start of string
//                 .strip_prefix(s)
//                 // Skip if immediately followed by [a-zA-Z0-9_], since that's an identifier
//                 // e.g. `let_it_be`
//                 .filter(|rest| {
//                     !matches!(
//                         rest.chars().next(),
//                         Some(v) if is_id_continue(v)
//                     )
//                 })
//                 .map(|rest| {
//                     (
//                         rest,
//                         Token {
//                             kind: TokenKind::Keyword(kw),
//                         },
//                     )
//                 })
//         })
//         .ok_or(())
//         .or_else(|_| fail(input))
// }

// pub fn identifier(input: &str) -> IResult<&str, Token> {
//     map(
//         recognize(pair(
//             satisfy(is_id_start),
//             many0_count(satisfy(is_id_continue)),
//         )),
//         |x: &str| Token {
//             kind: TokenKind::Identifier(x.to_owned()),
//         },
//     )(input)
// }

// const fn is_id_start(c: char) -> bool {
//     c.is_ascii_alphabetic() || c == '_'
// }

// const fn is_id_continue(c: char) -> bool {
//     c.is_ascii_alphanumeric() || c == '_' || c == '\''
// }
