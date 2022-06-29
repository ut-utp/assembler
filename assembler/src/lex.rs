//! Functions and data structures for lexing LC-3 assembly.
//!
//! Lexical analysis, or lexing, is the process of splitting a source string into a sequence of meaningful "tokens."
//! Each token is a small data structure which typically represents one "word" or punctuation mark
//! in the source code. Here's an example:
//!
//! ```
//! # use lc3_assembler::LeniencyLevel;
//! # use lc3_assembler::lex::*;
//! # use lc3_assembler::lex::Token::*;
//! # use lc3_assembler::lex::Opcode::*;
//! # use lc3_isa::Reg::*;
//! # use lc3_assembler::lex::LiteralValue::*;
//! let source = "ADD R0, R0, #1; increment counter";
//! let (tokens, _) = lex(source, LeniencyLevel::Lenient).unwrap();
//! assert_eq!(tokens,
//!     vec![
//!         (Opcode(Add),             0.. 3),
//!         (Register(R0),            4.. 6),
//!         (Comma,                   6.. 7),
//!         (Register(R0),            8..10),
//!         (Comma,                  10..11),
//!         (NumberLiteral(Word(1)), 12..14),
//!         (Comment,                14..33),
//!     ]);
//! ```
//!
//! The string is split into seven [`Token`]s. For most of them,
//! each part separated by spaces or punctuation becomes its own token.
//! But really, tokens are based on what parts are significant; notice that the
//! entire comment is represented by one token, and there is no information
//! stored about what the comment said. This is because the content of comments
//! doesn't change the code that needs to be assembled. Maybe more obviously,
//! all of the spaces between the opcode and operands aren't represented in
//! the output tokens at all. They were only important for distinguishing separate tokens.
//!
//! Lexing only splits the string. It doesn't check whether the order of tokens makes sense.
//! For example, the following string is not valid LC-3, but it can be lexed successfully:
//!
//! ```
//! # use lc3_assembler::LeniencyLevel;
//! # use lc3_assembler::lex::*;
//! # use lc3_assembler::lex::Token::*;
//! # use lc3_assembler::lex::Opcode::*;
//! # use lc3_isa::Reg::*;
//! # use lc3_assembler::lex::LiteralValue::*;
//! let source = "hello, world\n";
//! let (tokens, _) = lex(source, LeniencyLevel::Lenient).unwrap();
//! assert_eq!(tokens,
//!     vec![
//!         (Label("HELLO".to_string()),  0.. 5),
//!         (Comma,                       5.. 6),
//!         (Label("WORLD".to_string()),  7..12),
//!         (Newline,                    12..13),
//!     ]);
//! ```
//!
//! [`lex`] also outputs the locations of the tokens in the source string as index ranges.
//! These are to help construct error messages which refer to specific locations in the source.
//!
//!
use chumsky::prelude::*;
use lc3_isa::{Addr, Reg, SignedWord, Word};
use std::convert::{TryFrom, TryInto};
use std::fmt::{Display, Formatter};
use std::num::TryFromIntError;
use chumsky::Stream;

use crate::Spanned;
use crate::LeniencyLevel;

/// A unit representing a string of meaningful text in LC-3 assembly code.
///
/// Produced by [`lex`]ing. See the [module-level documentation](crate::lex) for examples.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Token {
    /// An opcode, pseudo-op (**except `.END`**), or named TRAP routine.
    Opcode(Opcode),
    /// A register reference (e.g., `R0`).
    Register(Reg),
    /// An unqualified positive decimal number. Used as an officially required operand of `.BLKW`.
    ///
    /// # Examples
    /// - `0`
    /// - `10`
    UnqualifiedNumberLiteral(Word),
    /// A number literal, qualified with a base prefix (`#`, `b`, or `x`) and optional negative sign `-`.
    ///
    /// The qualifiers are used to calculate the numeric value during lexing and are not stored.
    ///
    /// # Examples
    /// - `#-1`
    /// - `x3000`
    /// - `b0101`
    NumberLiteral(LiteralValue),
    /// A string literal (e.g., `"Hello, world!"`).
    StringLiteral(String),
    /// A label or label reference.
    ///
    /// Most alphanumeric strings which aren't reserved for other valid tokens
    /// are valid labels, depending on the [`LeniencyLevel`](crate::LeniencyLevel)
    /// used when [`lex`]ing.
    Label(String),

    /// The `.END` pseudo-op.
    ///
    /// Not included as an [`Opcode`] because it denotes
    /// the end of a program block. This makes it
    /// useful for parsing to distinguish between `.END`
    /// and instructions that can occur within a program block.
    End,

    /// A newline.
    ///
    /// Matches line feeds, carriage returns,
    /// and other types of vertical whitespace.
    Newline,
    /// A comma (`,`).
    Comma,
    /// A comment, including the leading semicolon.
    Comment,

    /// Any string of characters which doesn't represent any other type of token.
    Invalid,
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

/// The numeric value represented by a number literal.
///
/// Can be any unsigned or 2's-complement signed number with a width up to 16 bits.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum LiteralValue {
    Word(Word),
    SignedWord(SignedWord),
}

impl TryFrom<LiteralValue> for Addr {
    type Error = TryFromIntError;

    fn try_from(value: LiteralValue) -> Result<Self, Self::Error> {
        match value {
            LiteralValue::Word(word) => Ok(word),
            LiteralValue::SignedWord(signed_word) => signed_word.try_into(),
        }
    }
}

impl TryFrom<LiteralValue> for SignedWord {
    type Error = TryFromIntError;

    fn try_from(value: LiteralValue) -> Result<Self, Self::Error> {
        match value {
            LiteralValue::Word(word) => word.try_into(),
            LiteralValue::SignedWord(signed_word) => Ok(signed_word),
        }
    }
}

impl TryFrom<LiteralValue> for u8 {
    type Error = TryFromIntError;

    fn try_from(value: LiteralValue) -> Result<Self, Self::Error> {
        match value {
            LiteralValue::Word(word) => word.try_into(),
            LiteralValue::SignedWord(signed_word) => signed_word.try_into(),
        }
    }
}

/// The set of condition codes (`n`, `z`, and/or `p`) on which a `BR` opcode is conditioned.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct ConditionCodes {
    pub(crate) n: bool,
    pub(crate) z: bool,
    pub(crate) p: bool,
}

/// A specific LC-3 opcode, pseudo-op, or named TRAP routine.
///
/// Does not include [`.END`](Token::End).
///
/// Represents a *case-insensitive* string in the source code.
/// That is, [`Opcode::Add`] can represent `ADD`, `add`, or `Add`, etc.
/// All are treated as the same `Opcode`. Below, only the all-uppercase
/// option is listed for each `Opcode` variant.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Opcode {
    /// The opcode `ADD`.
    Add,
    /// The opcode `AND`.
    And,
    /// The opcode `BR`, conditioned on any combination of condition codes.
    ///
    /// # Examples
    /// - `BR`
    /// - `BRn`
    /// - `BRzp`
    Br(ConditionCodes),
    /// The opcode `JMP`.
    Jmp,
    /// The opcode `JSR`.
    Jsr,
    /// The opcode `JSRR`.
    Jsrr,
    /// The opcode `LD`.
    Ld,
    /// The opcode `LDI`.
    Ldi,
    /// The opcode `LDR`.
    Ldr,
    /// The opcode `LEA`.
    Lea,
    /// The opcode `NOT`.
    Not,
    /// The opcode `RET`.
    Ret,
    /// The opcode `RTI`.
    Rti,
    /// The opcode `ST`.
    St,
    /// The opcode `STI`.
    Sti,
    /// The opcode `STR`.
    Str,
    /// The opcode `TRAP`.
    Trap,

    // Pseudo-ops
    /// The pseudo-op `.ORIG`.
    Orig,
    /// The pseudo-op `.FILL`.
    Fill,
    /// The pseudo-op `.BLKW`.
    Blkw,
    /// The pseudo-op `.STRINGZ`.
    Stringz,

    // Named TRAP routines
    /// The named TRAP routine `GETC`.
    Getc,
    /// The named TRAP routine `OUT`.
    Out,
    /// The named TRAP routine `PUTS`.
    Puts,
    /// The named TRAP routine `IN`.
    In,
    /// The named TRAP routine `PUTSP`.
    Putsp,
    /// The named TRAP routine `HALT`.
    Halt,
}

#[derive(Debug)]
enum CaseSensitivePassResult {
    CaseInsensitiveSource(String),
    CaseSensitiveToken(Token),
}

fn number_literal_with_base(base: u32, prefix: char, leniency: LeniencyLevel) -> impl Parser<char, LiteralValue, Error=Simple<char>> {
    let strict_literal =
        just(prefix)
        .ignore_then(just('-').ignored().or_not())
        .then(text::digits(base))
        .try_map(move |(maybe_sign, digits): (Option<()>, String), span| {
            let parse_result = if maybe_sign.is_some() {
                SignedWord::from_str_radix(&format!("-{}", digits), base)
                    .map(LiteralValue::SignedWord)
            } else {
                Word::from_str_radix(&digits, base)
                    .map(LiteralValue::Word)
            };
            parse_result.map_err(|e| Simple::custom(span, e.to_string())) // TODO: parse error should only be on overflow or underflow
        });
    let literal: Box<dyn Parser<char, LiteralValue, Error=Simple<char>>> =
        match leniency {
            LeniencyLevel::Lenient => Box::new(just("0").or_not().ignore_then(strict_literal)),
            LeniencyLevel::Strict => Box::new(strict_literal),
        };
    literal
}

fn one_opcode(pattern: &'static str, output_opcode: Opcode) -> impl Parser<char, Opcode, Error=Simple<char>> {
    just_to(pattern, output_opcode)
}

fn one_register(pattern: &'static str, output_reg: Reg) -> impl Parser<char, Reg, Error=Simple<char>> {
    just_to(pattern, output_reg)
}

fn just_to<O: Clone>(pattern: &'static str, output: O) -> impl Parser<char, O, Error=Simple<char>> {
    just(pattern).to(output)
}

fn string_literal() -> impl Parser<char, Token, Error=Simple<char>> {
    // `escape` and `string_literal` are based on JSON parser example
    // https://github.com/zesterer/chumsky/blob/d4102128315d9dbbea901a91dc5eaa0fc9a790f7/examples/json.rs#L39
    let escape = just::<_, _, Simple<char>>('\\').ignore_then(
        just('\\')
            .or(just('"'))
            .or(just('b').to('\x08'))
            .or(just('f').to('\x0C'))
            .or(just('n').to('\n'))
            .or(just('r').to('\r'))
            .or(just('t').to('\t'))
    );

    just('"')
        .ignore_then(filter(|c| *c != '\\' && *c != '"').or(escape).repeated())
        .then_ignore(just('"'))
        .collect::<String>()
        .map(Token::StringLiteral)
}

fn comment() -> impl Parser<char, Token, Error=Simple<char>> {
    just(';')
        .then(filter(|c| !is_newline(c)).repeated())
        .to(Token::Comment)
}

fn tokens(leniency: LeniencyLevel) -> impl Parser<char, Vec<Spanned<Token>>, Error=Simple<char>> {
    let newline = text::newline()
        .to(Token::Newline);

    let comma = just(',')
        .to(Token::Comma);

    let non_newline_whitespace =
        filter(|c: &char| c.is_whitespace() && !is_newline(c)).repeated();

    let terminator =
        filter(|c: &char| c.is_whitespace() || *c == ',' || *c == ';').ignored()
            .or(end().ignored());

    use Opcode::*;
    let branch_opcode =
        just("BR")
            .ignore_then(one_of("NZP").repeated().at_most(3))
            .map::<Opcode, _>(|cond_code_chars| {
                let cond_codes =
                    if cond_code_chars.is_empty() {
                        ConditionCodes { n: true, z: true, p: true }
                    } else {
                        let n = cond_code_chars.contains(&'N');
                        let z = cond_code_chars.contains(&'Z');
                        let p = cond_code_chars.contains(&'P');
                        ConditionCodes { n, z, p }
                    };
                Br(cond_codes)
            });

    // These options are separated by `or` instead of all belonging
    // to one tuple passed to `choice` because `choice` only supports
    // tuples with up to 26 elements.
    // The grouping by 'opcode type' was chosen arbitrarily.
    let opcode = choice((
        one_opcode("ADD", Add),
        one_opcode("AND", And),
        branch_opcode,
        one_opcode("JMP", Jmp),
        one_opcode("JSRR", Jsrr),
        one_opcode("JSR", Jsr),
        one_opcode("LDI", Ldi),
        one_opcode("LDR", Ldr),
        one_opcode("LD", Ld),
        one_opcode("LEA", Lea),
        one_opcode("NOT", Not),
        one_opcode("RET", Ret),
        one_opcode("RTI", Rti),
        one_opcode("STI", Sti),
        one_opcode("STR", Str),
        one_opcode("ST", St),
        one_opcode("TRAP", Trap),
    ))
        .or(choice((
            one_opcode("GETC", Getc),
            one_opcode("OUT", Out),
            one_opcode("PUTSP", Putsp),
            one_opcode("PUTS", Puts),
            one_opcode("IN", In),
            one_opcode("HALT", Halt),
        )))
        .or(choice((
            one_opcode(".ORIG", Orig),
            one_opcode(".FILL", Fill),
            one_opcode(".BLKW", Blkw),
            one_opcode(".STRINGZ", Stringz),
        )))
        .then_ignore(terminator.clone().rewind())
        .map(Token::Opcode);

    let end_pseudo_op = just(".END")
        .then_ignore(terminator.clone().rewind())
        .to(Token::End);

    use Reg::*;
    let register = choice((
        one_register("R0", R0),
        one_register("R1", R1),
        one_register("R2", R2),
        one_register("R3", R3),
        one_register("R4", R4),
        one_register("R5", R5),
        one_register("R6", R6),
        one_register("R7", R7),
    ))
        .then_ignore(terminator.clone().rewind())
        .map(Token::Register);

    let unqualified_number_literal_base = 10;
    let unqualified_number_literal = text::digits(unqualified_number_literal_base)
        .try_map(move |digits: String, span| {
            Word::from_str_radix(&digits, unqualified_number_literal_base)
                .map_err(|e| Simple::custom(span, e.to_string())) // TODO: parse error should only be on overflow or underflow
        })
        .then_ignore(terminator.clone().rewind())
        .map(Token::UnqualifiedNumberLiteral);

    let number_literal = choice((
        number_literal_with_base(2, 'B', leniency),
        number_literal_with_base(10, '#', leniency),
        number_literal_with_base(16, 'X', leniency),
    ))
        .then_ignore(terminator.clone().rewind())
        .map(Token::NumberLiteral);

    let label = text::ident() // C-style identifier. Follows all LC-3 label rules but allows arbitrary length and underscores.
        .then_ignore(terminator.rewind())
        .map(Token::Label); // TODO: validate length, underscores in strict mode

    let token = choice((
        opcode,
        end_pseudo_op,
        register,
        number_literal,
        unqualified_number_literal,
        string_literal(),
        label,
        newline,
        comma,
        comment(),
    ))
        .recover_with(skip_until([',', ';', ' ', '\t', '\n', '\r', '\x0B', '\x0C', '\u{0085}', '\u{2028}', '\u{2029}'], |_| Token::Invalid)); // TODO: improve?

    token
        .map_with_span(|token, span| (token, span))
        .padded_by(non_newline_whitespace)
        .repeated()
        .then_ignore(end())
}

fn is_newline(c: &char) -> bool {
    // All line breaks matched by chumsky::text::newline
    ['\n',
        '\r',
        '\x0B',                   // Vertical tab
        '\x0C',                   // Form feed
        '\u{0085}',               // Next line
        '\u{2028}',               // Line separator
        '\u{2029}', ].contains(c) // Paragraph separator
}

fn case_sensitive_tokens() -> impl Parser<char, Vec<Spanned<CaseSensitivePassResult>>, Error=Simple<char>> {
    let case_sensitive_token =
        choice((
            string_literal(),
            comment()
        ))
        .map(|t| CaseSensitivePassResult::CaseSensitiveToken(t));

    let case_insensitive_source =
        filter(|c| *c != '"' && *c != ';').repeated().at_least(1)
            .collect()
            .map(|s| CaseSensitivePassResult::CaseInsensitiveSource(s));

    case_insensitive_source
        .or(case_sensitive_token)
        .map_with_span(|cspr, s| (cspr, s))
        .repeated()
        .then_ignore(end())
}

fn case_sensitive_pass(source: &str) -> (Option<Vec<Spanned<CaseSensitivePassResult>>>, Vec<Simple<char>>) {
    case_sensitive_tokens().parse_recovery_verbose(source)
}

fn case_insensitive_pass(case_sensitive_pass_results: Vec<Spanned<CaseSensitivePassResult>>, leniency: LeniencyLevel) -> (Vec<Spanned<Token>>, Vec<Simple<char>>) {
    let mut toks: Vec<Spanned<Token>> = Vec::new();
    let mut errors = Vec::new();

    for (cspr, span) in case_sensitive_pass_results {
        match cspr {
            CaseSensitivePassResult::CaseInsensitiveSource(s) => {
                // TODO: profile CPU + memory to see whether this introduces any inefficiencies.
                // This allows chumsky to correctly track spans while parsing this substring.
                let uppercase_s = s.to_uppercase();
                let spanned_char_stream = uppercase_s.chars()
                    .enumerate()
                    .map(|(i, c)| {
                        let pos = span.start + i;
                        (c, pos..(pos + 1))
                    });
                let stream = Stream::from_iter(span.end..(span.end + 1), spanned_char_stream);
                let (maybe_tokens, lex_errors) = tokens(leniency).parse_recovery_verbose(stream);

                // TODO: confirm that fail case is impossible, or decide on how to handle. `recover_with` should prevent failure
                if let Some(ts) = maybe_tokens {
                    toks.extend(ts);
                }
                errors.extend(lex_errors);
            }
            CaseSensitivePassResult::CaseSensitiveToken(t) => {
                toks.push((t, span));
            }
        }
    }

    (toks, errors)
}

/// Analysis data about the [`Token`]s output during [`lex`]ing.
///
/// The result of performing some analysis on the tokens
/// after "lexing proper" is complete. Used to produce some error messages
/// during [semantic analysis](crate::analyze), after the tokens
/// have been consumed during the [`parse`](crate::parse) step.
pub struct LexData {
    pub(crate) no_tokens: bool,
    pub(crate) orig_present: bool,
    pub(crate) end_present: bool,
}

fn contains_token(tokens: &Vec<Spanned<Token>>, token: Token) -> bool {
    tokens.iter().any(|t| t.0 == token)
}

/// Produce a sequence of [`Token`]s representative of the given source string.
///
/// See the [module-level documentation](crate::lex) for general information and examples.
///
/// This function also produces index ranges corresponding to each token's location
/// in the source string. It also analyzes the tokens and produces [`LexData`].
/// Because the tokens are consumed by the [`parse`](crate::parse) step, this data saves the
/// information about the tokens which the [semantic analysis step](crate::analyze)
/// needs to produce some types of error messages.
pub fn lex(source: &str, leniency: LeniencyLevel) -> Result<(Vec<Spanned<Token>>, LexData), Vec<Simple<char>>> {
    let (maybe_csprs, mut errors) = case_sensitive_pass(source);
    let tokens =
        maybe_csprs
            .map(|csprs| {
                let (maybe_tokens, cip_errors) = case_insensitive_pass(csprs, leniency);
                errors.extend(cip_errors);
                maybe_tokens
            });

    match tokens {
        None => Err(errors),
        Some(ts) => {
            let no_tokens = ts.is_empty();
            let orig_present = contains_token(&ts, Token::Opcode(Opcode::Orig));
            let end_present = contains_token(&ts, Token::End);
            let lex_data = LexData { no_tokens, orig_present, end_present };
            Ok((ts, lex_data))
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use Token::*;
    use Reg::*;
    use crate::lex::Opcode::*;

    #[test]
    fn lone_error() {
        let source = "#OOPS";
        let (tokens, _) = lex(source, LeniencyLevel::Lenient).unwrap();
        assert_eq!(
            vec![
                (Invalid, 0..5),
            ],
            tokens);
    }

    #[test]
    fn error_in_context() {
        let source = "ADD R0, R0, #OOPS; <- error";
        let (tokens, _) = lex(source, LeniencyLevel::Lenient).unwrap();
        assert_eq!(
            vec![
                (Opcode(Add),   0.. 3),
                (Register(R0),  4.. 6),
                (Comma,         6.. 7),
                (Register(R0),  8..10),
                (Comma,        10..11),
                (Invalid,      12..17),
                (Comment,      17..27),
            ],
            tokens);
    }
}