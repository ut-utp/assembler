use std::convert::TryFrom;
use chumsky::prelude::*;
use chumsky::Stream;
use lc3_isa::{Reg, Word};

use crate::Spanned;
use crate::LeniencyLevel;
use crate::lexer::{LiteralValue, Opcode, Token};

pub(crate) type WithErrData<T> = Spanned<Result<T, ()>>;

#[derive(Debug, Eq, PartialEq)]
pub struct Program {
    pub(crate) orig: WithErrData<Instruction>,
    pub(crate) instructions: Vec<WithErrData<Instruction>>,
}

#[derive(Debug, Eq, PartialEq)]
pub(crate) struct Instruction {
    pub(crate) label: Option<WithErrData<String>>,
    pub(crate) opcode: WithErrData<Opcode>,
    pub(crate) operands: WithErrData<Vec<WithErrData<Operand>>>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) enum Operand {
    Register(Reg),
    UnqualifiedNumberLiteral(Word),
    NumberLiteral(LiteralValue),
    StringLiteral(String),
    Label(String),
}

impl TryFrom<Operand> for Reg {
    type Error = ();

    fn try_from(e: Operand) -> Result<Self, Self::Error> {
        if let Operand::Register(r) = e {
            Ok(r)
        } else {
            Err(())
        }
    }
}

impl TryFrom<Operand> for LiteralValue {
    type Error = ();

    fn try_from(e: Operand) -> Result<Self, Self::Error> {
        if let Operand::NumberLiteral(v) = e {
            Ok(v)
        } else {
            Err(())
        }
    }
}

impl Operand {
    pub(crate) fn string(self) -> String {
        if let Self::StringLiteral(s) = self {
            s
        } else {
            panic!("Not a string literal")
        }
    }

    pub(crate) fn label(self) -> String {
        if let Self::Label(l) = self {
            l
        } else {
            panic!("Not a label")
        }
    }

    pub(crate) fn unqualified_number_value(self) -> Word {
        if let Self::UnqualifiedNumberLiteral(w) = self {
            w
        } else {
            panic!("Not an unqualified number literal")
        }
    }
}

fn operand() -> impl Parser<Token, WithErrData<Operand>, Error = Simple<Token>> {
    let operand = select! {
        Token::Register(reg)                 => Ok(Operand::Register(reg)),
        Token::UnqualifiedNumberLiteral(val) => Ok(Operand::UnqualifiedNumberLiteral(val)),
        Token::NumberLiteral(val)            => Ok(Operand::NumberLiteral(val)),
        Token::StringLiteral(s)              => Ok(Operand::StringLiteral(s)),
        Token::Label(s)                      => Ok(Operand::Label(s)),
        Token::Invalid                       => Err(()),
    };
    operand.map_with_span(|o, span| (o, span))
}

fn any_opcode() -> impl Parser<Token, WithErrData<Opcode>, Error = Simple<Token>> {
    filter_map(move |span, t: Token|
        match t.clone() {
            Token::Opcode(o) => Ok(Ok(o)),
            Token::Invalid => Ok(Err(())),
            _ => Err(Simple::expected_input_found(span, None, Some(t))) // TODO: improve error, expected
        })
        .map_with_span(|o, span| (o, span))
}

fn opcode(expected: Opcode) -> impl Parser<Token, WithErrData<Opcode>, Error = Simple<Token>> {
    let expected_token = Token::Opcode(expected);
    filter_map(move |span, t|
        if t == expected_token {
            if let Token::Opcode(o) = t {
                Ok(Ok(o))
            } else { unreachable!() }
        } else if let Token::Invalid = t {
            Ok(Err(()))
        } else {
            Err(Simple::expected_input_found(span, [Some(expected_token.clone())], Some(t)))
        })
        .map_with_span(|o, span| (o, span))
}

fn instruction(orig: bool, leniency: LeniencyLevel) -> impl Parser<Token, Spanned<Instruction>, Error = Simple<Token>> {
    let label =
        select! {
            Token::Label(s) => Ok(s),
            Token::Invalid => Err(())
        }
        .map_with_span(|l, s| (l, s));

    let oc: Box<dyn Parser<Token, WithErrData<Opcode>, Error = Simple<Token>>> =
        if orig {
            Box::new(opcode(Opcode::Orig))
        } else {
            Box::new(any_opcode())
        };

    let operand_separator: Box<dyn Parser<Token, (), Error = Simple<Token>>> =
        match leniency {
            LeniencyLevel::Lenient => Box::new(just(Token::Comma).or_not().ignored()),
            LeniencyLevel::Strict => Box::new(just(Token::Comma).ignored()),
        };

    let operands =
        operand()
            .separated_by(operand_separator)
            .map_with_span(|os, span| (Ok(os), span));

    label.or_not()
        .then_ignore(just(Token::Newline).repeated())
        .then(oc)
        .then(operands)
        .map_with_span(|((l, o), os), span| {
            let instruction = Instruction {
                label: l,
                opcode: o,
                operands: os,
            };
            (instruction, span)
        })
}

fn comments_and_newlines() -> impl Parser<Token, (), Error = Simple<Token>> {
    just(Token::Comment).or_not()
        .then(just(Token::Newline).repeated().at_least(1))
        .repeated()
        .ignored()
}

fn program(leniency: LeniencyLevel) -> impl Parser<Token, Spanned<Program>, Error = Simple<Token>> {
    instruction(true, leniency)
        .map(|(i, span)| (Ok(i), span))
        .then(
            instruction(false, leniency)
                .map(|(i, span)| (Ok(i), span))
                .separated_by(comments_and_newlines())
                .allow_leading()
                .allow_trailing()
        )
        .then_ignore(just::<_, Token, _>(Token::End))
        .map_with_span(|(orig, instructions), span| {
            (Program { orig, instructions }, span)
        })
}

pub(crate) type File = Vec<WithErrData<Program>>;

fn file(leniency: LeniencyLevel) -> impl Parser<Token, Spanned<Vec<WithErrData<Program>>>, Error = Simple<Token>> {
    comments_and_newlines()
        .ignore_then(
            program(leniency)
                .map(|(p, span)| (Ok(p), span))
                .separated_by(none_of(Token::Opcode(Opcode::Orig)).repeated())
                .allow_trailing()
        )
        .then_ignore(end())
        .map_with_span(|programs, span| (programs, span))
}

pub fn parse(src: &str, tokens: Vec<Spanned<Token>>, leniency: LeniencyLevel) -> (Option<Spanned<File>>, Vec<Simple<Token>>) {
    let len = src.chars().count();
    file(leniency).parse_recovery_verbose(Stream::from_iter(len..len + 1, tokens.into_iter()))
}


#[cfg(test)]
mod tests {
    use super::*;
    use super::Operand::*;
    use super::Reg::*;
    use super::Opcode::*;
    use crate::lexer::lex;

    #[test]
    fn ignore_after_end() {
        let source = ".ORIG x3000\nADD R0, R0, R0\n.END then %some #random junk!";
        let (maybe_tokens, _) = lex(source, LeniencyLevel::Lenient);
        let tokens = maybe_tokens.unwrap();
        let (file, _) = parse(source, tokens, LeniencyLevel::Lenient);

        assert_eq!(Some((vec![(Ok(Program {
            orig: (Ok(Instruction { label: None, opcode: (Ok(Orig), 0..5), operands: (Ok(vec![(Ok(NumberLiteral(LiteralValue::Word(12288))), 6..11)]), 6..11) }), 0..11),
            instructions: vec![
                (Ok(Instruction { label: None, opcode: (Ok(Add), 12..15), operands: (Ok(vec![(Ok(Register(R0)), 16..18), (Ok(Register(R0)), 20..22), (Ok(Register(R0)), 24..26)]), 16..26) }), 12..26)
            ],
        }), 0..31)], 0..56)),
                   file);
    }

    #[test]
    fn operand_error() {
        let source = ".ORIG x3000\nADD R0, R0, #OOPS; <- error\n.END";
        let (maybe_tokens, _) = lex(source, LeniencyLevel::Lenient);
        let tokens = maybe_tokens.unwrap();
        let (file, _) = parse(source, tokens, LeniencyLevel::Lenient);

        assert_eq!(Some((vec![(Ok(Program {
                orig: (Ok(Instruction { label: None, opcode: (Ok(Orig), 0..5), operands: (Ok(vec![(Ok(NumberLiteral(LiteralValue::Word(12288))), 6..11)]), 6..11) }), 0..11),
                instructions: vec![
                    (Ok(Instruction { label: None, opcode: (Ok(Add), 12..15), operands: (Ok(vec![(Ok(Register(R0)), 16..18), (Ok(Register(R0)), 20..22), (Err(()), 24..29)]), 16..29) }), 12..29)
                ],
            }), 0..44)], 0..44)),
            file);
    }

    #[test]
    fn label_error() {
        let source = ".ORIG x3000\nA%DDER ADD R0, R0, #1; <- error\n.END";
        let (maybe_tokens, _) = lex(source, LeniencyLevel::Lenient);
        let tokens = maybe_tokens.unwrap();
        let (file, _) = parse(source, tokens, LeniencyLevel::Lenient);

        assert_eq!(Some((vec![(Ok(Program {
                orig: (Ok(Instruction { label: None, opcode: (Ok(Orig), 0..5), operands: (Ok(vec![(Ok(NumberLiteral(LiteralValue::Word(12288))), 6..11)]), 6..11) }), 0..11),
                instructions: vec![
                       (Ok(Instruction { label: Some((Err(()), 12..18)), opcode: (Ok(Add), 19..22), operands: (Ok(vec![(Ok(Register(R0)), 23..25), (Ok(Register(R0)), 27..29), (Ok(NumberLiteral(LiteralValue::Word(1))), 31..33)]), 23..33) }), 12..33)
                ],
            }), 0..48)], 0..48)),
            file);
    }

}