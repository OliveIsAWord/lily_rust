use crate::lexer::{Keyword, Punctuation, Token, TokenKind};
use chumsky::prelude::*;
use dbg_pls::DebugPls;

pub type Ident = String;

// .map(|x| {
//     dbg!("Meowww");
//     std::thread::sleep(std::time::Duration::from_secs(1));
//     x
// })

#[derive(Clone, Debug, DebugPls)]
pub enum ItemKind {
    Fn(Fn),
}

#[derive(Clone, Debug, DebugPls)]
pub struct Fn {
    pub name: Ident,
    // make type annotations optional, e.g. for `self` or type inference?
    pub params: Vec<(Ident, TypeKind)>,
    pub return_type: Option<TypeKind>,
    pub body: Block,
}

#[derive(Clone, Debug, DebugPls)]
pub struct Block {
    pub statements: Vec<StatementKind>,
    pub tail: Option<ExprKind>,
}

#[derive(Clone, Debug, DebugPls)]
pub enum StatementKind {
    Empty,
    ExprStatement(Box<ExprKind>),
    Item(ItemKind),
    LetStatement(Ident, Box<ExprKind>),
}

#[derive(Clone, Debug, DebugPls)]
pub enum ExprKind {
    Variable(Ident),
    Block(Box<Block>),
}

#[derive(Clone, Debug, DebugPls)]
pub enum TypeKind {
    Name(Ident),
    Paren(Box<Self>),
    Tuple(Vec<Self>),
}

impl Default for TypeKind {
    fn default() -> Self {
        Self::Tuple(vec![])
    }
}

pub fn parse(input: &[Token]) -> Result<Vec<ItemKind>, String> {
    item()
        .repeated()
        .then_ignore(end())
        .parse(input)
        .map_err(|x| format!("{x:?}"))
}

fn item<'a>() -> impl Parser<Token, ItemKind, Error = Simple<Token>> + 'a {
    let ident = select! {
        Token { kind: TokenKind::Identifier(s) } => s,
    };
    let comma = just(Token {
        kind: TokenKind::Punctuation(Punctuation::Comma),
    });

    let type_ = recursive(|type_: Recursive<Token, TypeKind, Simple<Token>>| {
        ident.map(TypeKind::Name).or(type_
            .separated_by(comma.clone())
            .then(comma.clone().or_not())
            .delimited_by(
                just(Token {
                    kind: TokenKind::Punctuation(Punctuation::ParenOpen),
                }),
                just(Token {
                    kind: TokenKind::Punctuation(Punctuation::ParenClose),
                }),
            )
            .map(|(fields, trailing_comma)| {
                if fields.len() == 1 && trailing_comma.is_none() {
                    // TODO: i hate this clone
                    TypeKind::Paren(Box::new(fields[0].clone()))
                } else {
                    TypeKind::Tuple(fields)
                }
            }))
    });

    let fn_head = just(Token {
        kind: TokenKind::Keyword(Keyword::Fn),
    })
    .ignore_then(ident)
    .then(
        ident
            .then_ignore(just(Token {
                kind: TokenKind::Punctuation(Punctuation::Colon),
            }))
            .then(type_.clone())
            .separated_by(comma.clone())
            .allow_trailing()
            .delimited_by(
                just(Token {
                    kind: TokenKind::Punctuation(Punctuation::ParenOpen),
                }),
                just(Token {
                    kind: TokenKind::Punctuation(Punctuation::ParenClose),
                }),
            ),
    )
    .then(
        just(Token {
            kind: TokenKind::Punctuation(Punctuation::ThinArrow),
        })
        .ignore_then(type_)
        .or_not(),
    );

    let expr = |block: Recursive<'a, _, _, _>| {
        ident
            .map(ExprKind::Variable)
            .or(block.map(|b| ExprKind::Block(Box::new(b))))
    };

    let statement = |item: Recursive<'a, _, _, _>, block: Recursive<'a, _, _, _>| {
        let let_statement = just(Token {
            kind: TokenKind::Keyword(Keyword::Let),
        })
        .ignore_then(select! {
            Token { kind: TokenKind::Identifier(s) } => s,
        })
        .then_ignore(just(Token {
            kind: TokenKind::Punctuation(Punctuation::Eq),
        }))
        .then(expr(block.clone()))
        .then_ignore(just(Token {
            kind: TokenKind::Punctuation(Punctuation::Semicolon),
        }))
        .map(|(lhs, rhs)| StatementKind::LetStatement(lhs, Box::new(rhs)));
        let expr_statement = expr(block)
            .then_ignore(just(Token {
                kind: TokenKind::Punctuation(Punctuation::Semicolon),
            }))
            .map(|e| StatementKind::ExprStatement(Box::new(e)));
        let empty = just(Token {
            kind: TokenKind::Punctuation(Punctuation::Semicolon),
        })
        .to(StatementKind::Empty);
        let_statement
            .or(empty)
            .or(item.map(StatementKind::Item))
            .or(expr_statement)
    };
    recursive(|item| {
        let block = recursive(|block| {
            statement(item, block.clone())
                .repeated()
                .then(expr(block).or_not())
                .delimited_by(
                    just(Token {
                        kind: TokenKind::Punctuation(Punctuation::BraceOpen),
                    }),
                    just(Token {
                        kind: TokenKind::Punctuation(Punctuation::BraceClose),
                    }),
                )
                .map(|(statements, tail)| Block { statements, tail })
        });
        fn_head
            .then(block)
            .map(|(((name, params), return_type), body)| {
                ItemKind::Fn(Fn {
                    name,
                    params,
                    return_type,
                    body,
                })
            })
    })
}
