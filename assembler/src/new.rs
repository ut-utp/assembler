use chumsky::prelude::*;
use lc3_isa::{Reg, SignedWord, Word};

pub type Span = std::ops::Range<usize>;

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
enum LiteralValue {
    Word(Word),
    SignedWord(SignedWord),
}

#[derive(Clone, Debug)]
pub struct ConditionCodes {
    n: bool,
    z: bool,
    p: bool,
}

#[derive(Clone, Debug)]
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

fn lexer() -> impl Parser<char, Vec<(Token, Span)>, Error=Simple<char>> {
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple() {
        let src = ".ORIG x3000;\nLABEL ADD R0, R0, #70000\n.end";
        let (tokens, errs) = lexer().parse_recovery(src.to_uppercase());
        println!("{:?}", tokens);
        println!("{:?}", errs);
    }
}