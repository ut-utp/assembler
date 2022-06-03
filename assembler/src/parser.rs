use std::convert::{TryFrom, TryInto};
use chumsky::prelude::*;
use chumsky::recovery::SkipUntil;
use chumsky::Stream;
use lc3_isa::{Reg, Word};

use crate::{Span, Spanned};
use crate::LeniencyLevel;
use crate::lexer::{LiteralValue, Opcode, Token};

pub(crate) type WithErrData<T> = Spanned<Result<T, ()>>;

pub(crate) fn get<T>(v: &Vec<WithErrData<T>>, i: usize) -> Option<&T> {
    v.get(i)
        .and_then(|res| get_result(res).as_ref().ok())
}

pub(crate) fn get_result<T>(v: &WithErrData<T>) -> &Result<T, ()> {
    &v.0
}

pub(crate) fn result<T>(v: WithErrData<T>) -> Result<T, ()> {
    v.0
}

pub(crate) fn try_result<T>(maybe_v: Option<WithErrData<T>>) -> Result<T, ()> {
    result(maybe_v.ok_or(())?)
}

pub(crate) fn try_map<T, U, E>(maybe_v: Option<WithErrData<T>>) -> Result<U, ()> where
    U: TryFrom<T, Error=E>
{
    try_result(maybe_v)?
        .try_into()
        .map_err(|_| ())
}

#[derive(Debug, Eq, PartialEq)]
pub struct Program {
    pub(crate) orig: WithErrData<Vec<WithErrData<Operand>>>,
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
    pub(crate) fn get_string(self) -> Option<String> {
        if let Self::StringLiteral(s) = self {
            Some(s)
        } else {
            None
        }
    }

    pub(crate) fn get_label(self) -> Option<String> {
        if let Self::Label(l) = self {
            Some(l)
        } else {
            None
        }
    }

    pub(crate) fn get_unqualified_number_value(self) -> Option<Word> {
        if let Self::UnqualifiedNumberLiteral(w) = self {
            Some(w)
        } else {
            None
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

fn operands(leniency: LeniencyLevel) -> impl Parser<Token, WithErrData<Vec<WithErrData<Operand>>>, Error = Simple<Token>> {
    let operand_separator: Box<dyn Parser<Token, (), Error = Simple<Token>>> =
        match leniency {
            LeniencyLevel::Lenient => Box::new(just(Token::Comma).or_not().ignored()),
            LeniencyLevel::Strict => Box::new(just(Token::Comma).ignored()),
        };

    operand()
        .separated_by(operand_separator)
        .map_with_span(|os, span| (Ok(os), span))
}

fn instruction(leniency: LeniencyLevel) -> impl Parser<Token, WithErrData<Instruction>, Error = Simple<Token>> {
    let label =
        select! {
            Token::Label(s) => Ok(s),
            Token::Invalid => Err(())
        }
        .map_with_span(|l, s| (l, s));

    let opcode =
        filter_map(move |span, t: Token|
            match t.clone() {
                Token::Opcode(o) => Ok(Ok(o)),
                Token::Invalid => Ok(Err(())),
                _ => Err(Simple::expected_input_found(span, None, Some(t))) // TODO: improve error, expected
            })
            .map_with_span(|o, span| (o, span));

    label.or_not()
        .then_ignore(comments_and_newlines().or_not())
        .then(opcode)
        .then(operands(leniency))
        .map_with_span(|((l, o), os), span| {
            let instruction = Instruction {
                label: l,
                opcode: o,
                operands: os,
            };
            println!("{:?}", instruction);
            (Ok(instruction), span)
        })
        // Pseudo-recovery strategy -- take everything until the end of the line. Consider replacing with `recover_via` if merged into `chumsky`
        .or(none_of([Token::End, Token::Comment, Token::Newline]).repeated().at_least(1)
            .map_with_span(|_, span| (Err(()), span)))
}


fn comments_and_newlines() -> impl Parser<Token, (), Error = Simple<Token>> {
    just(Token::Comment).or_not()
        .then(just(Token::Newline).repeated().at_least(1))
        .repeated().at_least(1)
        .ignored()
}

fn program(leniency: LeniencyLevel) -> impl Parser<Token, Spanned<Program>, Error = Simple<Token>> {
    let orig =
        just(Token::Opcode(Opcode::Orig))
            .ignore_then(operands(leniency));

    orig
        .then(
            instruction(leniency)
                .separated_by(comments_and_newlines())
                .allow_leading()
                .allow_trailing()
        )
        .then_ignore(just::<_, Token, _>(Token::End))
        .map_with_span(|(orig, instructions), span| {
            (Program { orig, instructions }, span)
        })
}

#[derive(Debug)]
pub struct File {
    pub(crate) before_first_orig: Spanned<Vec<Token>>, // TODO: check that this only contains newlines and comments (at least if strict)
    pub programs: Vec<WithErrData<Program>>
}

fn file(leniency: LeniencyLevel) -> impl Parser<Token, Spanned<File>, Error = Simple<Token>> {
    none_of(Token::Opcode(Opcode::Orig)).repeated()
        .map_with_span(|toks, span| (toks, span))
        .then(
            program(leniency)
                .map(|(p, span)| (Ok(p), span))
                .separated_by(none_of(Token::Opcode(Opcode::Orig)).repeated())
                .allow_trailing()
        )
        .then_ignore(end())
        .map_with_span(|(before_first_orig, programs), span|
            (File { before_first_orig, programs }, span))
}

pub fn parse(src: &str, tokens: Vec<Spanned<Token>>, leniency: LeniencyLevel) -> (Option<Spanned<File>>, Vec<Simple<Token>>) {
    let len = src.chars().count();
    file(leniency)
        .parse_recovery_verbose(Stream::from_iter(len..len + 1, tokens.into_iter()))
}


#[cfg(test)]
mod tests {
    use super::*;
    use super::Operand::*;
    use super::Reg::*;
    use super::Opcode::*;
    use crate::lexer::lex;

    #[test]
    fn capture_tokens_before_first_orig_separately() {
        let source = "%some #random junk .ORIG x3000\nADD R0, R0, R0\n.END";
        let (maybe_tokens, _) = lex(source, LeniencyLevel::Lenient);
        let tokens = maybe_tokens.unwrap();
        let (file, _) = parse(source, tokens, LeniencyLevel::Lenient);

        assert_eq!((vec![Token::Invalid, Token::Invalid, Token::Label("JUNK".to_string())], 0..18),
                   file.unwrap().0.before_first_orig);
    }

    #[test]
    fn ignore_after_end() {
        let source = ".ORIG x3000\nADD R0, R0, R0\n.END then %some #random junk!";
        let (maybe_tokens, _) = lex(source, LeniencyLevel::Lenient);
        let tokens = maybe_tokens.unwrap();
        let (file, _) = parse(source, tokens, LeniencyLevel::Lenient);


        let f = file.unwrap().0;
        assert_eq!((vec![], 0..5), f.before_first_orig); // TODO: probably doesn't need fixing, but span should probably be 0..0; find source of bug
        assert_eq!(vec![(Ok(Program {
                orig: (Ok(vec![(Ok(NumberLiteral(LiteralValue::Word(12288))), 6..11)]), 6..11),
                instructions: vec![
                    (Ok(Instruction { label: None, opcode: (Ok(Add), 12..15), operands: (Ok(vec![(Ok(Register(R0)), 16..18), (Ok(Register(R0)), 20..22), (Ok(Register(R0)), 24..26)]), 16..26) }), 12..26)
                ],
            }), 0..31)],
           f.programs);
    }

    #[test]
    fn operand_error() {
        let source = ".ORIG x3000\nADD R0, R0, #OOPS; <- error\n.END";
        let (maybe_tokens, _) = lex(source, LeniencyLevel::Lenient);
        let tokens = maybe_tokens.unwrap();
        let (file, _) = parse(source, tokens, LeniencyLevel::Lenient);

        assert_eq!(vec![(Ok(Program {
                orig: (Ok(vec![(Ok(NumberLiteral(LiteralValue::Word(12288))), 6..11)]), 6..11),
                instructions: vec![
                    (Ok(Instruction { label: None, opcode: (Ok(Add), 12..15), operands: (Ok(vec![(Ok(Register(R0)), 16..18), (Ok(Register(R0)), 20..22), (Err(()), 24..29)]), 16..29) }), 12..29)
                ],
            }), 0..44)],
            file.unwrap().0.programs);
    }

    #[test]
    fn label_error() {
        let source = ".ORIG x3000\nA%DDER ADD R0, R0, #1; <- error\n.END";
        let (maybe_tokens, _) = lex(source, LeniencyLevel::Lenient);
        let tokens = maybe_tokens.unwrap();
        let (file, _) = parse(source, tokens, LeniencyLevel::Lenient);

        assert_eq!(vec![(Ok(Program {
                orig: (Ok(vec![(Ok(NumberLiteral(LiteralValue::Word(12288))), 6..11)]), 6..11),
                instructions: vec![
                   (Ok(Instruction { label: Some((Err(()), 12..18)), opcode: (Ok(Add), 19..22), operands: (Ok(vec![(Ok(Register(R0)), 23..25), (Ok(Register(R0)), 27..29), (Ok(NumberLiteral(LiteralValue::Word(1))), 31..33)]), 23..33) }), 12..33)
                ],
            }), 0..48)],
            file.unwrap().0.programs);
    }

}