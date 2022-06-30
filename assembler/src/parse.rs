//! Functions and data structures for parsing LC-3 assembly.
//!
//! Parsing, or syntactic analysis, tries to structure the sequence of tokens produced by [lexing](crate::lex).
//! Tokens between `.ORIG` and `.END` tokens are structured into programs.
//! Within those programs, tokens between newlines are structured into instructions.
//! The result is a [`File`], or syntax tree, corresponding to a single source file.
//! In other words, parsing is where the assembler
//! tries to make sense of the order of the tokens. Here's an example:
//!
//! ```
//! # use lc3_assembler::id;
//! # use lc3_assembler::LeniencyLevel;
//! # use lc3_assembler::lex::lex;
//! # use lc3_assembler::parse::*;
//! # use lc3_assembler::parse::Operand::*;
//! # use lc3_assembler::lex::Opcode::*;
//! # use lc3_assembler::lex::LiteralValue;
//! # use lc3_isa::Reg::*;
//! # use self::*;
//! let id = id(&std::path::PathBuf::from(""));
//! let source = ".ORIG x3000\nADDING ADD R0, R0, #1\n.END";
//! let (tokens, _) = lex(source, LeniencyLevel::Lenient).unwrap();
//! let (file, _) = parse(id, source, tokens, LeniencyLevel::Lenient).unwrap();
//!
//! assert_eq!(file.blocks,
//!     vec![(Ok(ProgramBlock {
//!         orig: (Ok(vec![(Ok(NumberLiteral(LiteralValue::Word(12288))), 6..11)]), 6..11),
//!         instructions: vec![
//!             (Ok(Instruction {
//!                 label: Some((Ok("ADDING".to_string()), 12..18)),
//!                 opcode: (Ok(Add), 19..22),
//!                 operands: (Ok(vec![
//!                     (Ok(Register(R0)), 23..25),
//!                     (Ok(Register(R0)), 27..29),
//!                     (Ok(NumberLiteral(LiteralValue::Word(1))), 31..33)
//!                 ]), 23..33)
//!             }), 12..33)
//!         ],
//!     }), 0..38)]);
//! ```
//!
//! Often times, the order of tokens may be invalid, but mostly correct. For example,
//! the source code may include an invalid token where a label is expected:
//!
//! ```
//! # use lc3_assembler::id;
//! # use lc3_assembler::LeniencyLevel;
//! # use lc3_assembler::lex::lex;
//! # use lc3_assembler::parse::*;
//! # use lc3_assembler::parse::Operand::*;
//! # use lc3_assembler::lex::Opcode::*;
//! # use lc3_assembler::lex::LiteralValue;
//! # use lc3_isa::Reg::*;
//! # use self::*;
//! let id = id(&std::path::PathBuf::from(""));
//! let source = ".ORIG x3000\nA%DDER ADD R0, R0, #1\n.END";
//! let (tokens, _) = lex(source, LeniencyLevel::Lenient).unwrap();
//! let (file, _) = parse(id, source, tokens, LeniencyLevel::Lenient).unwrap();
//!
//! assert_eq!(file.blocks,
//!     vec![(Ok(ProgramBlock {
//!         orig: (Ok(vec![(Ok(NumberLiteral(LiteralValue::Word(12288))), 6..11)]), 6..11),
//!         instructions: vec![
//!             (Ok(Instruction {
//!                 label: Some((Err(()), 12..18)), // <-- Error here!
//!                 opcode: (Ok(Add), 19..22),      // But everything else parses successfully,
//!                 operands: (Ok(vec![             // or at least reasonably.
//!                     (Ok(Register(R0)), 23..25),
//!                     (Ok(Register(R0)), 27..29),
//!                     (Ok(NumberLiteral(LiteralValue::Word(1))), 31..33)
//!                 ]), 23..33)
//!             }), 12..33)
//!         ],
//!     }), 0..38)]);
//! ```
//!
//!
//! [`parse`] is designed to recover when it encounters a token which is out of order. It replaces
//! the smallest possible part of the syntax tree with an error and tries to make
//! a reasonable guess about where to continue. In the example above, it assumes
//! that the invalid token was supposed to be a label, discards it, and checks for an opcode
//! next. In this way, [`parse`] attempts to produce a syntax tree for any input,
//! valid *or invalid*, but the tree will contain location-specific parse errors
//! which [the semantic analysis step](crate::analyze) can try and determine the cause of.
//! By trying to recover, [`parse`] can produce multiple errors instead of
//! failing at a single early error, and semantic analysis can provide clear reasons
//! for some errors.
//!
//! However, including error data for potentially any element
//! in the syntax tree makes the tree more complex. This is why
//! the examples above have so much "noise" in addition to the main data.
//! Most elements of the syntax tree are paired with error data using
//! [`WithErrData`](crate::WithErrData). We use this type to abstract away
//! the error data and make clearer which syntax elements comprise the tree,
//! at least when working with it in code.

use std::convert::TryFrom;
use chumsky::combinator::Repeated;
use chumsky::prelude::*;
use chumsky::primitive::NoneOf;
use chumsky::Stream;
use lc3_isa::{Reg, Word};

use crate::{SourceId, Spanned, WithErrData};
use crate::LeniencyLevel;
use crate::lex::{LiteralValue, Opcode, Token};

/// A representation of a LC-3 assembly file structured based on correct syntax. The root of the syntax tree.
///
/// Produced by [`parse`].
///
/// This assembler allows multiple "program blocks" in the same file,
/// as long as they wouldn't overlap in memory. They must be assembled
/// together and can reference each other's labels. This part of the syntax
/// tree therefore stores a list of program blocks.
#[derive(Debug)]
pub struct File {
    pub(crate) id: SourceId,
    #[allow(dead_code)]
    pub(crate) before_first_orig: Spanned<Vec<Token>>, // TODO: check that this only contains newlines and comments (at least if strict)
    pub blocks: Vec<WithErrData<ProgramBlock>>
}

/// A representation of an LC-3 assembly program block, starting with `.ORIG` and ending with `.END`.
#[derive(Debug, Eq, PartialEq)]
pub struct ProgramBlock {
    pub orig: WithErrData<Vec<WithErrData<Operand>>>,
    pub instructions: Vec<WithErrData<Instruction>>,
}

/// A representation of an LC-3 assembly instruction.
///
/// When produced by [`parse`], may contain any number or types of operands.
/// Operands are just parsed as an arbitrarily long list, no matter what opcode
/// was used. The number and types of operands are validated during [semantic analysis](crate::analyze).
#[derive(Debug, Eq, PartialEq)]
pub struct Instruction {
    pub label: Option<WithErrData<String>>,
    pub opcode: WithErrData<Opcode>,
    pub operands: WithErrData<Vec<WithErrData<Operand>>>,
}

/// An operand of an LC-3 assembly instruction.
///
/// Each variant directly corresponds to a specific [`Token`](crate::lex::Token) variant,
/// noted below. See the [`Token`] documentation for descriptions and examples of each.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Operand {
    /// Corresponds to [`Token::Register`](crate::lex::Token::Register).
    Register(Reg),
    /// Corresponds to [`Token::UnqualifiedNumberLiteral`](crate::lex::Token::UnqualifiedNumberLiteral).
    UnqualifiedNumberLiteral(Word),
    /// Corresponds to [`Token::NumberLiteral`](crate::lex::Token::NumberLiteral).
    NumberLiteral(LiteralValue),
    /// Corresponds to [`Token::StringLiteral`](crate::lex::Token::StringLiteral).
    StringLiteral(String),
    /// Corresponds to [`Token::Label`](crate::lex::Token::Label).
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

    let operand = filter_map(move |span, t: Token|
        match t.clone() {
            Token::Register(reg)                 => Ok(Ok(Operand::Register(reg))),
            Token::UnqualifiedNumberLiteral(val) => Ok(Ok(Operand::UnqualifiedNumberLiteral(val))),
            Token::NumberLiteral(val)            => Ok(Ok(Operand::NumberLiteral(val))),
            Token::StringLiteral(s)              => Ok(Ok(Operand::StringLiteral(s))),
            Token::Label(s)                      => Ok(Ok(Operand::Label(s))),
            Token::Opcode(_)
            | Token::End
            | Token::Invalid    => Ok(Err(())),
            _ => Err(Simple::expected_input_found(span, None, Some(t)))
        }
    );
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

    let terminator =
        just(Token::Comment).or_not()
            .then(just(Token::Newline).ignored().or(end()))
            .ignored();

    label.or_not()
        .then_ignore(comments_and_newlines().or_not())
        .then(opcode)
        .then(operands(leniency))
        .then_ignore(terminator.rewind())
        .map_with_span(|((l, o), os), span| {
            let instruction = Instruction {
                label: l,
                opcode: o,
                operands: os,
            };
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

fn everything_until_orig() -> Repeated<NoneOf<Token, Token, Simple<Token>>> {
    none_of(Token::Opcode(Opcode::Orig)).repeated()
}

fn program_block(leniency: LeniencyLevel) -> impl Parser<Token, WithErrData<ProgramBlock>, Error = Simple<Token>> {
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
            (Ok(ProgramBlock { orig, instructions }), span)
        })
        // Pseudo-recovery strategy -- take everything until next .ORIG
        .or(any().then(everything_until_orig())
            .map_with_span(|_, span| (Err(()), span)))
}

fn file(id: SourceId, leniency: LeniencyLevel) -> impl Parser<Token, Spanned<File>, Error = Simple<Token>> {
    everything_until_orig()
        .map_with_span(|toks, span| (toks, span))
        .then(
            program_block(leniency)
                .separated_by(everything_until_orig())
                .allow_trailing()
        )
        .then_ignore(end())
        .map_with_span(move |(before_first_orig, blocks), span|
            (File { id: id.clone(), before_first_orig, blocks }, span))
}

/// Produce a [`File`] (syntax tree) representative of the given tokens.
///
/// See the [module-level documentation](crate::parse) for general information and examples.
///
/// `tokens` must be the tokens produced by [`lex`](crate::lex::lex)ing `src`.
/// For the best errors, `id` should also correspond to the same source, but if (for example) the source is not
/// from a file, this isn't strictly necessary.
pub fn parse(id: SourceId, src: &str, tokens: Vec<Spanned<Token>>, leniency: LeniencyLevel) -> Result<Spanned<File>, Vec<Simple<Token>>> {
    let len = src.chars().count();
    let (maybe_file, errors) =
        file(id, leniency)
            .parse_recovery_verbose(Stream::from_iter(len..len + 1, tokens.into_iter()));

    maybe_file.ok_or(errors)
}


#[cfg(test)]
mod tests {
    use super::*;
    use super::Operand::*;
    use super::Reg::*;
    use super::Opcode::*;
    use crate::lex::lex;

    #[test]
    fn capture_tokens_before_first_orig_separately() {
        let source = "%some #random junk .ORIG x3000\nADD R0, R0, R0\n.END";
        let (tokens, _) = lex(source, LeniencyLevel::Lenient).unwrap();
        let file = parse("<test>".to_string(), source, tokens, LeniencyLevel::Lenient).unwrap();

        assert_eq!((vec![Token::Invalid, Token::Invalid, Token::Label("JUNK".to_string())], 0..18),
                   file.0.before_first_orig);
    }

    #[test]
    fn ignore_after_end() {
        let source = ".ORIG x3000\nADD R0, R0, R0\n.END then %some #random junk!";
        let (tokens, _) = lex(source, LeniencyLevel::Lenient).unwrap();
        let file = parse("<test>".to_string(), source, tokens, LeniencyLevel::Lenient).unwrap();

        let f = file.0;
        assert_eq!((vec![], 0..5), f.before_first_orig); // TODO: probably doesn't need fixing, but span should probably be 0..0; find source of bug
        assert_eq!(vec![(Ok(ProgramBlock {
                orig: (Ok(vec![(Ok(NumberLiteral(LiteralValue::Word(12288))), 6..11)]), 6..11),
                instructions: vec![
                    (Ok(Instruction { label: None, opcode: (Ok(Add), 12..15), operands: (Ok(vec![(Ok(Register(R0)), 16..18), (Ok(Register(R0)), 20..22), (Ok(Register(R0)), 24..26)]), 16..26) }), 12..26)
                ],
            }), 0..31)],
           f.blocks);
    }

    #[test]
    fn operand_error() {
        let source = ".ORIG x3000\nADD R0, R0, #OOPS; <- error\n.END";
        let (tokens, _) = lex(source, LeniencyLevel::Lenient).unwrap();
        let file = parse("<test>".to_string(), source, tokens, LeniencyLevel::Lenient).unwrap();

        assert_eq!(vec![(Ok(ProgramBlock {
                orig: (Ok(vec![(Ok(NumberLiteral(LiteralValue::Word(12288))), 6..11)]), 6..11),
                instructions: vec![
                    (Ok(Instruction { label: None, opcode: (Ok(Add), 12..15), operands: (Ok(vec![(Ok(Register(R0)), 16..18), (Ok(Register(R0)), 20..22), (Err(()), 24..29)]), 16..29) }), 12..29)
                ],
            }), 0..44)],
            file.0.blocks);
    }

}