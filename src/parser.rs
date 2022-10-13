use crate::lexer::{Keyword, Punctuation, Token, TokenKind};
use chumsky::prelude::*;

pub type Ident = String;

// .map(|x| {
//     dbg!("Meowww");
//     std::thread::sleep(std::time::Duration::from_secs(1));
//     x
// })

#[derive(Clone, Debug)]
pub enum ItemKind {
    Fn(Ident, Block),
}

#[derive(Clone, Debug, Default)]
pub struct Block {
    statements: Vec<StatementKind>,
    tail: Option<ExprKind>,
}

#[derive(Clone, Debug)]
pub enum StatementKind {
    Empty,
    ExprStatement(Box<ExprKind>),
    Item(ItemKind),
    LetStatement(Ident, Box<ExprKind>),
}

#[derive(Clone, Debug)]
pub enum ExprKind {
    Variable(Ident),
    Block(Box<Block>),
}

pub fn parse(input: &[Token]) -> Result<Vec<ItemKind>, String> {
    println!("Start!");
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

    let fn_head = just(Token {
        kind: TokenKind::Keyword(Keyword::Fn),
    })
    .ignore_then(ident)
    .then_ignore(just(Token {
        kind: TokenKind::Punctuation(Punctuation::ParenOpen),
    }))
    // TODO: function args
    .then_ignore(just(Token {
        kind: TokenKind::Punctuation(Punctuation::ParenClose),
    }));

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
            .map(|(name, body)| ItemKind::Fn(name, body))
    })
}

// fn statement() -> impl Parser<Token, StatementKind, Error = Simple<Token>> {
//     let let_statement = just(Token {
//         kind: TokenKind::Keyword(Keyword::Let),
//     })
//     .ignore_then(select! {
//         Token { kind: TokenKind::Identifier(s) } => s,
//     })
//     .then_ignore(just(Token {
//         kind: TokenKind::Punctuation(Punctuation::Eq),
//     }))
//     .then(expr())
//     .then_ignore(just(Token {
//         kind: TokenKind::Punctuation(Punctuation::Semicolon),
//     }))
//     .map(|(lhs, rhs)| StatementKind::LetStatement(lhs, Box::new(rhs)));

//     let empty = just(Token {
//         kind: TokenKind::Punctuation(Punctuation::Semicolon),
//     })
//     .to(StatementKind::Empty);

//     let_statement.or(empty).or(item().map(StatementKind::Item))
// }

// fn expr() -> impl Parser<Token, ExprKind, Error = Simple<Token>> {
//     select! {
//         Token { kind: TokenKind::Identifier(s) } => s,
//     }
//     .map(ExprKind::Variable)
// }

// fn block() -> BoxedParser<'static, Token, Block, Simple<Token>> {
//     statement()
//         .repeated()
//         .then(expr().or_not())
//         .delimited_by(
//             just(Token {
//                 kind: TokenKind::Punctuation(Punctuation::BraceOpen),
//             }),
//             just(Token {
//                 kind: TokenKind::Punctuation(Punctuation::BraceClose),
//             }),
//         )
//         .map(|(statements, tail)| Block { statements, tail })
//         .boxed()
// }

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
