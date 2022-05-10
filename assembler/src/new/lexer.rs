use chumsky::prelude::*;
use lc3_isa::{Addr, Reg, SignedWord, Word};
use std::convert::{TryFrom, TryInto};
use super::Spanned;
use std::num::TryFromIntError;
use chumsky::Stream;
use crate::new::LeniencyLevel;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Token {
    Opcode(Opcode),
    Register(Reg),
    UnqualifiedNumberLiteral(Word),
    NumberLiteral(LiteralValue),
    StringLiteral(String),
    Label(String),

    Newline,
    Comma,

    Comment,
}

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

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct ConditionCodes {
    pub(crate) n: bool,
    pub(crate) z: bool,
    pub(crate) p: bool,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Opcode {
    Add,
    And,
    Br(ConditionCodes),
    Jmp,
    Jsr,
    Jsrr,
    Ld,
    Ldi,
    Ldr,
    Lea,
    Not,
    Ret,
    Rti,
    St,
    Sti,
    Str,
    Trap,

    // Pseudo-ops
    Orig,
    Fill,
    Blkw,
    Stringz,
    End,

    // Named TRAP routines
    Getc,
    Out,
    Puts,
    In,
    Putsp,
    Halt,
}

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
            one_opcode(".END", End),
        )))
        .map(Token::Opcode);

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
        .map(Token::Register);

    let unqualified_number_literal_base = 10;
    let unqualified_number_literal = text::digits(unqualified_number_literal_base)
        .try_map(move |digits: String, span| {
            Word::from_str_radix(&digits, unqualified_number_literal_base)
                .map_err(|e| Simple::custom(span, e.to_string())) // TODO: parse error should only be on overflow or underflow
        })
        .map(Token::UnqualifiedNumberLiteral);

    let number_literal = choice((
        number_literal_with_base(2, 'B', leniency),
        number_literal_with_base(10, '#', leniency),
        number_literal_with_base(16, 'X', leniency),
    ))
        .map(Token::NumberLiteral);

    let label = text::ident() // C-style identifier. Follows all LC-3 label rules but allows arbitrary length and underscores.
        .map(Token::Label); // TODO: validate length, underscores in strict mode

    let comma = just(',')
        .to(Token::Comma);

    let token = choice((
        opcode,
        register,
        number_literal,
        unqualified_number_literal,
        string_literal(),
        label,
        newline,
        comma,
        comment(),
    ))
        .recover_with(skip_then_retry_until([])); // TODO: improve?

    let non_newline_whitespace =
        filter(|c: &char| c.is_whitespace() && !is_newline(c)).repeated();

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

fn case_insensitive_pass(case_sensitive_pass_results: Vec<Spanned<CaseSensitivePassResult>>, leniency: LeniencyLevel) -> (Option<Vec<Spanned<Token>>>, Vec<Simple<char>>) {
    let mut toks: Option<Vec<Spanned<Token>>> = None;
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

                if let Some(ts) = maybe_tokens {
                    toks.get_or_insert(Vec::new()).extend(ts);
                }
                errors.extend(lex_errors);
            }
            CaseSensitivePassResult::CaseSensitiveToken(t) => {
                toks.get_or_insert(Vec::new()).push((t, span));
            }
        }
    }

    (toks, errors)
}

pub fn lex(source: &str, leniency: LeniencyLevel) -> (Option<Vec<Spanned<Token>>>, Vec<Simple<char>>) {
    let (maybe_csprs, mut errors) = case_sensitive_pass(source);
    let tokens =
        if let Some(csprs) = maybe_csprs {
            let (maybe_tokens, cip_errors) = case_insensitive_pass(csprs, leniency);
            errors.extend(cip_errors);
            maybe_tokens
        } else {
            None
        };
    (tokens, errors)
}
