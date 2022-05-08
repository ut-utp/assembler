use chumsky::prelude::*;
use chumsky::Stream;
use itertools::Itertools;
use lc3_isa::{Reg, SignedWord, Word};

type Span = std::ops::Range<usize>;
type Spanned<T> = (T, Span);

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
enum Token {
    Opcode(Opcode),
    Register(Reg),
    NumberLiteral(LiteralValue),
    StringLiteral(String),
    Label(String),

    Newline,
    Comma,

    Comment,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
enum LiteralValue {
    Word(Word),
    SignedWord(SignedWord),
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct ConditionCodes {
    n: bool,
    z: bool,
    p: bool,
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

// Lexer

fn number_literal_with_base(base: u32, prefix: char) -> impl Parser<char, LiteralValue, Error=Simple<char>> {
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
        })
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

fn lexer() -> impl Parser<char, Vec<Spanned<Token>>, Error=Simple<char>> {
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

    let string_literal = just('"')
        .ignore_then(filter(|c| *c != '\\' && *c != '"').or(escape).repeated())
        .then_ignore(just('"'))
        .collect::<String>()
        .map(Token::StringLiteral);

    let number_literal = choice((
        number_literal_with_base(2, 'B'),
        number_literal_with_base(10, '#'),
        number_literal_with_base(16, 'X'),
    ))
        .map(Token::NumberLiteral);

    let label = text::ident() // C-style identifier. Follows all LC-3 label rules but allows arbitrary length and underscores.
        .map(Token::Label); // TODO: validate length, underscores in strict mode

    let comment = just(';')
        .then(filter(|c| !is_newline(c)).repeated())
        .to(Token::Comment);

    let comma = just(',')
        .to(Token::Comma);

    let token = choice((
        opcode,
        register,
        number_literal,
        string_literal,
        label,
        newline,
        comma,
        comment,
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

fn lex(source: &str) -> (Option<Vec<Spanned<Token>>>, Vec<Simple<char>>) {
    lexer().parse_recovery(source.to_uppercase())
}


// Parser

type WithErrData<T> = Spanned<Result<T, Simple<Token>>>;

#[derive(Debug)]
struct Program {
    instructions: Vec<WithErrData<Instruction>>,
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
        .ignore_then(instruction(OpcodeFilter::OnlyOrig))
        .then(
            instruction(OpcodeFilter::AnyButEnd)
                .map(|(i, span)| (Ok(i), span))
                .separated_by(comments_and_newlines())
                .allow_leading()
                .allow_trailing()
                )
        .then(instruction(OpcodeFilter::OnlyEnd))
        .then_ignore(comments_and_newlines())
        .then_ignore(end())
        .map_with_span(|((orig, instructions), end), span| {
            (Program { instructions }, span)
        })
}

fn file() -> impl Parser<Token, Spanned<Vec<WithErrData<Program>>>, Error = Simple<Token>> {
    program()
        .map(|(p, span)| (Ok(p), span))
        .separated_by(comments_and_newlines())
        .allow_leading()
        .allow_trailing()
        .map_with_span(|programs, span| (programs, span))
}

fn parse(src: &str, tokens: Vec<Spanned<Token>>) -> (Option<Spanned<Program>>, Vec<Simple<Token>>) {
    let len = src.chars().count();
    program().parse_recovery_verbose(Stream::from_iter(len..len + 1, tokens.into_iter()))
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple() {
        let src = ".ORIG x3000;\nLABEL ADD R0, R0, #7000\n.end";
        let (tokens, lex_errs) = lex(src);
        println!("{:?}", tokens);
        println!("{:?}", lex_errs);

        let parse_results = tokens.map(|ts| parse(src, ts));
        if let Some((program, parse_errs)) = parse_results {
            println!("{:?}", program);
            println!("{:?}", parse_errs);
        }
    }
}