use chumsky::prelude::*;
use dbg_pls::DebugPls;
pub use lexer::Literal;
use lexer::{Keyword, Punctuation, Token, TokenKind};

pub type Ident = String;

// .map(|x| {
//     dbg!("Meowww");
//     std::thread::sleep(std::time::Duration::from_secs(1));
//     x
// })

#[derive(Clone, Debug, DebugPls)]
#[non_exhaustive]
pub enum ItemKind {
    Fn(Fn),
}

#[derive(Clone, Debug, DebugPls)]
pub struct Fn {
    pub name: Ident,
    // make type annotations optional, e.g. for `self` or type inference?
    pub params: Vec<(Ident, Mutability, TypeKind)>,
    pub return_type: Option<TypeKind>,
    pub body: Block,
}

#[derive(Clone, Debug, DebugPls)]
pub enum StatementKind {
    Empty,
    ExprStatement(Box<ExprKind>),
    Item(ItemKind),
    LetStatement(Ident, Mutability, Option<Box<ExprKind>>),
}

#[derive(Clone, Debug, DebugPls)]
pub enum ExprKind {
    Literal(Literal),
    Variable(Ident),
    Block(Block),
    If(Box<Self>, Block, Option<Box<Self>>),
    While(Box<Self>, Block, Option<Box<Self>>),
    Call(Box<Self>, Vec<Self>),
}

#[derive(Clone, Debug, DebugPls)]
pub struct Block {
    pub statements: Vec<StatementKind>,
    pub tail: Option<Box<ExprKind>>,
}

impl Block {
    fn new(mut statements: Vec<StatementKind>, tail: Option<Box<ExprKind>>) -> Self {
        // If the final statement can be read as an expr and there is no tail, move it to the tail
        // TODO: this code is not very good :)
        let tail = tail.or_else(|| match statements.pop()? {
            StatementKind::ExprStatement(e) if !e.ends_with_semicolon() => Some(e),
            last => {
                statements.push(last);
                None
            }
        });
        Self { statements, tail }
    }
}

impl ExprKind {
    #[must_use]
    pub fn ends_with_semicolon(&self) -> bool {
        match self {
            Self::Variable(_) | Self::Literal(_) | Self::Call(..) => true,
            Self::Block(_) => false,
            Self::If(_, _, else_expr) | Self::While(_, _, else_expr) => {
                matches!(else_expr, Some(e) if e.ends_with_semicolon())
            }
        }
    }
}

#[derive(Clone, Debug, DebugPls)]
pub enum TypeKind {
    Name(Ident),
    Paren(Box<Self>),
    Ref(Box<Self>, Mutability),
    Tuple(Vec<Self>),
}

//// I assume this will be useful at some point
// impl TypeKind {
//     pub const UNIT: Self = Self::Tuple(vec![]);
// }

#[derive(Clone, Copy, Debug, DebugPls)]
pub enum Mutability {
    Const,
    Mut,
}

pub fn parse(input: &[Token]) -> Result<Vec<ItemKind>, String> {
    item()
        .repeated()
        .then_ignore(end())
        .parse(input)
        .map_err(|x| format!("{x:?}"))
}

// Justify: Chumsky's API plus Rust's mutually recursive grammar
// basically necessitates all parsing be done in a single function
#[allow(clippy::too_many_lines)]
fn item<'a>() -> impl Parser<Token, ItemKind, Error = Simple<Token>> + 'a {
    let just_punc = |p| {
        just(Token {
            kind: TokenKind::Punctuation(p),
        })
    };
    let just_kw = |k| {
        just(Token {
            kind: TokenKind::Keyword(k),
        })
    };
    let comma = just_punc(Punctuation::Comma);
    let maybe_mut = just_kw(Keyword::Mut).or_not().map(|maybe_mut| {
        if maybe_mut.is_some() {
            Mutability::Mut
        } else {
            Mutability::Const
        }
    });
    let ref_ = just_punc(Punctuation::And).ignore_then(maybe_mut.clone());
    let literal = select! {
        Token { kind: TokenKind::Literal(v) } => v,
    };
    let ident = select! {
        Token { kind: TokenKind::Identifier(s) } => s,
    };

    let type_ = recursive(|type_: Recursive<Token, TypeKind, Simple<Token>>| {
        ident
            .map(TypeKind::Name)
            .or(ref_
                .then(type_.clone())
                .map(|(is_mut, pointee)| TypeKind::Ref(Box::new(pointee), is_mut)))
            .or(type_
                .separated_by(comma.clone())
                .then(comma.clone().or_not())
                .delimited_by(
                    just_punc(Punctuation::ParenOpen),
                    just_punc(Punctuation::ParenClose),
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

    let fn_signature = just_kw(Keyword::Fn)
        .ignore_then(ident)
        .then(
            maybe_mut
                .clone()
                .then(ident)
                .then_ignore(just_punc(Punctuation::Colon))
                .then(type_.clone())
                .map(|((is_mut, param), param_type)| (param, is_mut, param_type))
                .separated_by(comma.clone())
                .allow_trailing()
                .delimited_by(
                    just_punc(Punctuation::ParenOpen),
                    just_punc(Punctuation::ParenClose),
                ),
        )
        .then(
            just_punc(Punctuation::ThinArrow)
                .ignore_then(type_)
                .or_not(),
        );

    let expr = |block: Recursive<'a, _, _, _>| {
        recursive(|expr| {
            let if_ = just_kw(Keyword::If)
                .ignore_then(expr.clone())
                .then(block.clone())
                .then(
                    just_kw(Keyword::Else)
                        .ignore_then(expr.clone())
                        .map(Box::new)
                        .or_not(),
                )
                .map(|((cond, body), else_)| ExprKind::If(Box::new(cond), body, else_));
            let while_ = just_kw(Keyword::While)
                .ignore_then(expr.clone())
                .then(block.clone())
                .then(
                    just_kw(Keyword::Else)
                        .ignore_then(expr.clone())
                        .map(Box::new)
                        .or_not(),
                )
                .map(|((cond, body), else_)| ExprKind::While(Box::new(cond), body, else_));
            let call = expr
                .clone()
                .separated_by(just_punc(Punctuation::Comma))
                .allow_trailing()
                .delimited_by(
                    just_punc(Punctuation::ParenOpen),
                    just_punc(Punctuation::ParenClose),
                );
            choice((
                literal.map(ExprKind::Literal),
                ident.map(ExprKind::Variable),
                block.clone().map(ExprKind::Block),
                if_,
                while_,
            ))
            .then_with(move |e| {
                // TODO: how to remove clones?
                call.clone().repeated().map(move |calls| {
                    calls
                        .into_iter()
                        .fold(e.clone(), |func, args| ExprKind::Call(Box::new(func), args))
                })
            })
        })
    };

    let statement = |item: Recursive<'a, _, _, _>, block: Recursive<'a, _, _, _>| {
        let let_statement = just_kw(Keyword::Let)
            .ignore_then(maybe_mut)
            .then(ident)
            .then(
                just_punc(Punctuation::Eq)
                    .ignore_then(expr(block.clone()))
                    .or_not(),
            )
            .then_ignore(just_punc(Punctuation::Semicolon))
            .map(|((is_mut, lhs), rhs)| {
                StatementKind::LetStatement(lhs, is_mut, rhs.map(Box::new))
            });
        let expr_statement = expr(block)
            // TODO: replace with non-boxing zero cost abstraction
            .then_with(|e| {
                if e.ends_with_semicolon() {
                    just(Token {
                        kind: TokenKind::Punctuation(Punctuation::Semicolon),
                    })
                    .to(e)
                    .boxed()
                } else {
                    empty().to(e).boxed()
                }
            })
            .map(|e| StatementKind::ExprStatement(Box::new(e)));
        let empty = just_punc(Punctuation::Semicolon).to(StatementKind::Empty);
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
                    just_punc(Punctuation::BraceOpen),
                    just_punc(Punctuation::BraceClose),
                )
                .map(|(statements, tail)| Block::new(statements, tail.map(Box::new)))
        });
        fn_signature
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
