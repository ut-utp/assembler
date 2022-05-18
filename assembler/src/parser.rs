use std::convert::TryFrom;
use chumsky::prelude::*;
use chumsky::Stream;
use lc3_isa::{Reg, Word};

use crate::Spanned;
use crate::LeniencyLevel;
use crate::lexer::{LiteralValue, Opcode, Token};

pub(crate) type WithErrData<T> = Spanned<Result<T, Simple<Token>>>;

#[derive(Debug)]
pub struct Program {
    pub(crate) orig: WithErrData<Instruction>,
    pub(crate) instructions: Vec<WithErrData<Instruction>>,
    end: WithErrData<Instruction>,
}

#[derive(Debug)]
pub(crate) struct Instruction {
    pub(crate) label: Option<WithErrData<String>>,
    pub(crate) opcode: WithErrData<Opcode>,
    pub(crate) operands: WithErrData<Vec<WithErrData<Operand>>>,
}

#[derive(Clone, Debug)]
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

fn operand() -> impl Parser<Token, Spanned<Operand>, Error = Simple<Token>> {
    let operand = select! {
        Token::Register(reg)                 => Operand::Register(reg),
        Token::UnqualifiedNumberLiteral(val) => Operand::UnqualifiedNumberLiteral(val),
        Token::NumberLiteral(val)            => Operand::NumberLiteral(val),
        Token::StringLiteral(s)              => Operand::StringLiteral(s),
        Token::Label(s)                      => Operand::Label(s),
    };
    operand.map_with_span(|o, span| (o, span))
}

fn any_opcode_but(denied: Opcode) -> impl Parser<Token, Spanned<Opcode>, Error = Simple<Token>> {
    filter_map(move |span, t: Token|
        if let Token::Opcode(o) = t.clone() {
            if o == denied {
                Err(Simple::expected_input_found(span, None, Some(t))) // TODO: improve error, expected
            } else {
                Ok(o)
            }
        } else {
            Err(Simple::expected_input_found(span, None, Some(t))) // TODO: improve error, expected
        })
        .map_with_span(|o, span| (o, span))
}

fn opcode(expected: Opcode) -> impl Parser<Token, Spanned<Opcode>, Error = Simple<Token>> {
    let expected_token = Token::Opcode(expected);
    filter_map(move |span, t|
        if t == expected_token {
            if let Token::Opcode(o) = t {
                Ok(o)
            } else { unreachable!() }
        } else {
            Err(Simple::expected_input_found(span, [Some(expected_token.clone())], Some(t)))
        })
        .map_with_span(|o, span| (o, span))
}

enum OpcodeFilter {
    OnlyOrig,
    AnyButEnd,
    OnlyEnd,
}

fn instruction(oc_filter: OpcodeFilter, leniency: LeniencyLevel) -> impl Parser<Token, Spanned<Instruction>, Error = Simple<Token>> {
    let label =
        select! { Token::Label(s) => s }
            .map_with_span(|s, span| (Ok(s), span))
            .or_not();

    use OpcodeFilter::*;
    let oc: Box<dyn Parser<Token, Spanned<Opcode>, Error = Simple<Token>>> =
        match oc_filter {
            OnlyOrig  => Box::new(opcode(Opcode::Orig)),
            AnyButEnd => Box::new(any_opcode_but(Opcode::End)),
            OnlyEnd   => Box::new(opcode(Opcode::End)),
        };
    let oc_with_err_data = oc.map(|(oc, span)| (Ok(oc), span));

    let operand_separator: Box<dyn Parser<Token, (), Error = Simple<Token>>> =
        match leniency {
            LeniencyLevel::Lenient => Box::new(just(Token::Comma).or_not().ignored()),
            LeniencyLevel::Strict => Box::new(just(Token::Comma).ignored()),
        };

    let operands =
        operand()
            .map(|(o, span)| (Ok(o), span))
            .separated_by(operand_separator)
            .map_with_span(|os, span| (Ok(os), span));

    label
        .then_ignore(just(Token::Newline).repeated())
        .then(oc_with_err_data)
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
    comments_and_newlines()
        .ignore_then(
            instruction(OpcodeFilter::OnlyOrig, leniency)
                .map(|(i, span)| (Ok(i), span)))
        .then(
            instruction(OpcodeFilter::AnyButEnd, leniency)
                .map(|(i, span)| (Ok(i), span))
                .separated_by(comments_and_newlines())
                .allow_leading()
                .allow_trailing()
        )
        .then(
            instruction(OpcodeFilter::OnlyEnd, leniency)
                .map(|(i, span)| (Ok(i), span)))
        .then_ignore(comments_and_newlines())
        .then_ignore(end())
        .map_with_span(|((orig, instructions), end), span| {
            (Program { orig, instructions, end }, span)
        })
}

type File = Vec<WithErrData<Program>>;

fn file(leniency: LeniencyLevel) -> impl Parser<Token, Spanned<Vec<WithErrData<Program>>>, Error = Simple<Token>> {
    program(leniency)
        .map(|(p, span)| (Ok(p), span))
        .separated_by(comments_and_newlines())
        .allow_leading()
        .allow_trailing()
        .map_with_span(|programs, span| (programs, span))
}

pub fn parse(src: &str, tokens: Vec<Spanned<Token>>, leniency: LeniencyLevel) -> (Option<Spanned<File>>, Vec<Simple<Token>>) {
    let len = src.chars().count();
    file(leniency).parse_recovery_verbose(Stream::from_iter(len..len + 1, tokens.into_iter()))
}
