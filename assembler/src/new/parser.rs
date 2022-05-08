use chumsky::prelude::*;
use chumsky::Stream;
use super::{Spanned, Token, Opcode, Reg, LiteralValue, };

type WithErrData<T> = Spanned<Result<T, Simple<Token>>>;

#[derive(Debug)]
pub(crate) struct Program {
    orig: WithErrData<Instruction>,
    instructions: Vec<WithErrData<Instruction>>,
    end: WithErrData<Instruction>,
}

#[derive(Debug)]
struct Instruction {
    label: Option<WithErrData<String>>,
    opcode: WithErrData<Opcode>,
    operands: WithErrData<Vec<WithErrData<Operand>>>,
}

#[derive(Debug)]
enum Operand {
    Register(Reg),
    NumberLiteral(LiteralValue),
    StringLiteral(String),
    Label(String),
}

fn operand() -> impl Parser<Token, Spanned<Operand>, Error = Simple<Token>> {
    let operand = select! {
        Token::Register(reg)      => Operand::Register(reg),
        Token::NumberLiteral(val) => Operand::NumberLiteral(val),
        Token::StringLiteral(s)   => Operand::StringLiteral(s),
        Token::Label(s)           => Operand::Label(s),
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

fn instruction(oc_filter: OpcodeFilter) -> impl Parser<Token, Spanned<Instruction>, Error = Simple<Token>> {
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

    let operands =
        operand()
            .map(|(o, span)| (Ok(o), span))
            .separated_by::<Token, _>(just(Token::Comma))
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

fn program() -> impl Parser<Token, Spanned<Program>, Error = Simple<Token>> {
    comments_and_newlines()
        .ignore_then(
            instruction(OpcodeFilter::OnlyOrig)
                .map(|(i, span)| (Ok(i), span)))
        .then(
            instruction(OpcodeFilter::AnyButEnd)
                .map(|(i, span)| (Ok(i), span))
                .separated_by(comments_and_newlines())
                .allow_leading()
                .allow_trailing()
        )
        .then(
            instruction(OpcodeFilter::OnlyEnd)
                .map(|(i, span)| (Ok(i), span)))
        .then_ignore(comments_and_newlines())
        .then_ignore(end())
        .map_with_span(|((orig, instructions), end), span| {
            (Program { orig, instructions, end }, span)
        })
}

type File = Vec<WithErrData<Program>>;

fn file() -> impl Parser<Token, Spanned<Vec<WithErrData<Program>>>, Error = Simple<Token>> {
    program()
        .map(|(p, span)| (Ok(p), span))
        .separated_by(comments_and_newlines())
        .allow_leading()
        .allow_trailing()
        .map_with_span(|programs, span| (programs, span))
}

pub(crate) fn parse(src: &str, tokens: Vec<Spanned<Token>>) -> (Option<Spanned<File>>, Vec<Simple<Token>>) {
    let len = src.chars().count();
    file().parse_recovery_verbose(Stream::from_iter(len..len + 1, tokens.into_iter()))
}
