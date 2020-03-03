use crate::lexer::{Token, TokenType, Opcode, Op, NamedTrap, PseudoOp};
use crate::ir1_simple_lines::{SimpleLines, SimpleLine};
use std::iter::Peekable;
use crate::error::ParseError;
use itertools::Itertools;

pub type Lines<'input> = Vec<Line<'input>>;

#[derive(Clone, Debug)]
pub struct Line<'input> {
    pub content: LineContent<'input>,
    pub whitespace: Vec<Token<'input>>, // Only includes whitespace around operation
    pub comment: Option<Token<'input>>,
    pub newline: Option<Token<'input>>,
}

pub type Label<'input> = Token<'input>;

#[derive(Clone, Debug)]
pub enum LineContent<'input> {
    Valid(Option<Label<'input>>, Option<OperationTokens<'input>>),
    Invalid(Vec<Token<'input>>)
}

#[derive(Clone, Debug)]
pub struct OperationTokens<'input> {
    pub operator: Token<'input>,
    pub operands: OperandTokens<'input>,
    pub separators: Vec<Token<'input>>, // To include internal whitespace, but not surrounding
}

#[derive(Clone, Debug)]
pub enum OperandTokens<'input> {
    Add { dr: Token<'input>, sr1: Token<'input>, sr2_or_imm5: Token<'input> },
    And { dr: Token<'input>, sr1: Token<'input>, sr2_or_imm5: Token<'input> },
    Br { nzp: Option<Token<'input>>, label: Label<'input> },
    Jmp { base: Token<'input> },
    Jsr { label: Label<'input> },
    Jsrr { base: Token<'input> },
    Ld { dr: Token<'input>, label: Label<'input>, },
    Ldi { dr: Token<'input>, label: Label<'input>, },
    Ldr { dr: Token<'input>, base: Token<'input>, offset6: Token<'input> },
    Lea { dr: Token<'input>, label: Label<'input> },
    Not { dr: Token<'input>, sr: Token<'input> },
    Ret,
    Rti,
    St { sr: Token<'input>, label: Label<'input> },
    Sti { sr: Token<'input>, label: Label<'input> },
    Str { sr: Token<'input>, base: Token<'input>, offset6: Token<'input> },
    Trap { trap_vec: Token<'input> },

    Getc,
    Out,
    Puts,
    In,
    Putsp,
    Halt,

    Orig { origin: Token<'input> },
    Fill { value: Token<'input> },
    Blkw { size: Token<'input> },
    Stringz { string: Token<'input> },
    End,
}

pub fn parse_lines(simple_lines: SimpleLines) -> Lines {
    simple_lines.into_iter()
        .map(parse_line)
        .collect()
}

fn parse_line(simple_line: SimpleLine) -> Line {
    let SimpleLine { content: old_content, comment, newline } = simple_line;
    let backup = old_content.clone();

    let mut tokens = old_content.into_iter().peekable();
    let mut whitespace = Vec::new();
    skip_and_collect_whitespace(&mut tokens, &mut whitespace);
    let label = parse_ambiguous(&mut tokens).ok();
    skip_and_collect_whitespace(&mut tokens, &mut whitespace);
    let content = parse_operation_tokens(&mut tokens, &mut whitespace).map_or(
        LineContent::Invalid(backup),
        |operation_tokens| { LineContent::Valid(label, operation_tokens) }
    );
    skip_and_collect_whitespace(&mut tokens, &mut whitespace);
    Line { content, whitespace, comment, newline, }
}

fn parse_ambiguous<'input, T>(tokens: &mut Peekable<T>) -> Result<Token<'input>, ParseError>
    where T: Iterator<Item=Token<'input>>
{
    parse_token(tokens, TokenType::Ambiguous)
}

fn parse_string<'input, T>(tokens: &mut Peekable<T>) -> Result<Token<'input>, ParseError>
    where T: Iterator<Item=Token<'input>>
{
    parse_token(tokens, TokenType::String)
}

fn parse_token<'input, T>(tokens: &mut Peekable<T>, target_type: TokenType) -> Result<Token<'input>, ParseError>
    where T: Iterator<Item=Token<'input>>
{
    if let Some(&Token { ty, .. }) = tokens.peek() {
        if ty == target_type {
            return Ok(tokens.next().unwrap());
        }
    }
    Err(ParseError::Misc("Didn't find ambiguous token next.".to_string()))
}

// Expands to the necessary steps to parse operands into a given OperandTokens struct variant.
// Ex: fill_operands! { 3; Add { dr, sr1, sr2_or_imm5, }; tokens, separators }
// expands to:
// let whitespace = parse_whitespace(tokens)?;
// separators.extend(whitespace);
// let mut operand_buffer: [Option<Token<'input>>; 3] = [None; 3];
// parse_operands(tokens, &mut separators, &mut operand_buffer)?;
// OperandTokens::Add {
//     dr: operand_buffer[0].unwrap(),
//     sr1: operand_buffer[1].unwrap(),
//     sr2_or_imm5: operand_buffer[2].unwrap(),
// }

// TODO: put inside parse_operand_tokens to make it so we don't have to pass in references to tokens and separators
macro_rules! fill_operands {
    (@munch ($op_buf:ident) -> { $name:ident, $(($field:ident, $value:expr))* }) => {
        OperandTokens::$name {
            $($field: $value),*
        }
    };

    (@munch ($i:expr, $op_buf:ident, $id:ident,) -> { $($output:tt)* }) => {
        fill_operands! { @munch ($op_buf) -> { $($output)* ($id, $op_buf[$i].unwrap()) } }
    };

    (@munch ($i:expr, $op_buf:ident, $id:ident, $($next:tt)*) -> { $($output:tt)* }) => {
        fill_operands! { @munch ($i+1usize, $op_buf, $($next)*) -> { $($output)* ($id, $op_buf[$i].unwrap()) } }
    };

    ($num:expr; $name:ident { $($input:tt)+ }; $tokens:ident, $separators:ident) => {
        let whitespace = parse_whitespace($tokens)?;
        $separators.extend(whitespace);
        let mut operand_buffer: [Option<Token<'input>>; $num] = [None; $num]; // TODO: write inner macro to munch and get size of array
        parse_operands($tokens, &mut $separators, &mut operand_buffer)?;
        fill_operands! { @munch (0usize, operand_buffer, $($input)+) -> { $name, } }
    };
}


fn parse_operand_tokens<'input, T>(op: Op, tokens: &mut Peekable<T>, mut separators: &mut Vec<Token<'input>>) -> Result<OperandTokens<'input>, ParseError>
    where T: Iterator<Item=Token<'input>>
{
    let operands = match op {
        Op::Opcode(opcode) => match opcode {
            Opcode::Add => { fill_operands! { 3; Add { dr, sr1, sr2_or_imm5, }; tokens, separators } },
            Opcode::And => { fill_operands! { 3; And { dr, sr1, sr2_or_imm5, }; tokens, separators } },
            Opcode::Br => { // Specially handled due to nzp
                let nzp = parse_nzp(tokens)?;
                let whitespace = parse_whitespace(tokens)?;
                separators.extend(whitespace);
                let label = parse_ambiguous(tokens)?;
                OperandTokens::Br { nzp, label }
            },
            Opcode::Jmp  => { fill_operands! { 1; Jmp { base, }; tokens, separators } },
            Opcode::Jsr  => { fill_operands! { 1; Jsr { label, }; tokens, separators } },
            Opcode::Jsrr => { fill_operands! { 1; Jsrr { base, }; tokens, separators } },
            Opcode::Ld   => { fill_operands! { 2; Ld { dr, label, }; tokens, separators } },
            Opcode::Ldi  => { fill_operands! { 2; Ldi { dr, label, }; tokens, separators } },
            Opcode::Ldr  => { fill_operands! { 3; Ldr { dr, base, offset6, }; tokens, separators } },
            Opcode::Lea  => { fill_operands! { 2; Lea { dr, label, }; tokens, separators } },
            Opcode::Not  => { fill_operands! { 2; Not { dr, sr, }; tokens, separators } },
            Opcode::Ret  => OperandTokens::Ret,
            Opcode::Rti  => OperandTokens::Rti,
            Opcode::St   => { fill_operands! { 2; St { sr, label, }; tokens, separators } },
            Opcode::Sti  => { fill_operands! { 2; Sti { sr, label, }; tokens, separators } },
            Opcode::Str  => { fill_operands! { 3; Str { sr, base, offset6, }; tokens, separators } },
            Opcode::Trap => { fill_operands! { 1; Trap { trap_vec, }; tokens, separators } },
        },
        Op::NamedTrap(named_trap) => match named_trap {
            NamedTrap::Getc  => OperandTokens::Getc,
            NamedTrap::Out   => OperandTokens::Out,
            NamedTrap::Puts  => OperandTokens::Puts,
            NamedTrap::In    => OperandTokens::In,
            NamedTrap::Putsp => OperandTokens::Putsp,
            NamedTrap::Halt  => OperandTokens::Halt,
        },
        Op::PseudoOp(pseudo_op) => match pseudo_op {
            PseudoOp::Orig => { fill_operands! { 1; Orig { origin, }; tokens, separators } },
            PseudoOp::Fill => { fill_operands! { 1; Fill { value, }; tokens, separators } },
            PseudoOp::Blkw => { fill_operands! { 1; Blkw { size, }; tokens, separators } },
            PseudoOp::Stringz => {
                let whitespace = parse_whitespace(tokens)?;
                separators.extend(whitespace);
                let string = parse_string(tokens)?;
                OperandTokens::Stringz { string }
            },
            PseudoOp::End => OperandTokens::End,
        },
    };
    Ok(operands)
}

// Return None if no operation but valid line (i.e. only whitespace (optionally))
// ^^^ assumes whitespace has already been skipped.
// Return Err if line doesn't have valid pattern of tokens
fn parse_operation_tokens<'input, T>(mut tokens: &mut Peekable<T>, mut whitespace: &mut Vec<Token<'input>>) -> Result<Option<OperationTokens<'input>>, ParseError>
    where T: Iterator<Item=Token<'input>>
{
    match tokens.next() {
        Some(token) => match token.ty {
            TokenType::Op(op) => {
                let mut separators = Vec::new();
                let operands = parse_operand_tokens(op, tokens, &mut separators)?;
                skip_and_collect_whitespace(&mut tokens, &mut whitespace);
                if tokens.peek().is_some() {
                    Err(ParseError::Misc("Extra tokens at end of line.".to_string()))
                } else {
                    Ok(Some(OperationTokens { operator: token, operands, separators }))
                }
            }
            TokenType::Whitespace => unreachable!("Function was called without first skipping whitespace."),
            _ => Err(ParseError::Misc("Unexpected non-operator token at beginning of 'instruction'".to_string()))
        }
        None => Ok(None),
    }
}

fn parse_nzp<'input, T>(tokens: &mut Peekable<T>) -> Result<Option<Token<'input>>, ParseError>
    where T: Iterator<Item=Token<'input>>
{
    match tokens.peek() {
        Some(&token) => match token.ty {
            TokenType::Ambiguous => {
                tokens.next();
                Ok(Some(token))
            },
            TokenType::Whitespace => Ok(None),
            _ => Err(ParseError::Misc("Found non-nzp token while parsing nzp.".to_string()))
        },
        None => Err(ParseError::Misc("Ran out of tokens while parsing nzp.".to_string())),
    }
}

// Returns Ok if operands parsed correctly and fills operands with Some(token)
// Otherwise, returns Err
fn parse_operands<'input, T>(tokens: &mut Peekable<T>, separators: &mut Vec<Token<'input>>, operands: &mut [Option<Token<'input>>]) -> Result<(), ParseError>
    where T: Iterator<Item=Token<'input>>
{
    for i in 0..operands.len() {
        let operand = parse_ambiguous(tokens)?;
        operands[i] = Some(operand);
        if i < operands.len() - 1 {
            let separator = parse_separator(tokens)?;
            separators.extend(separator);
        }
    }
    Ok(())
}

fn skip_and_collect_whitespace<'input, T>(tokens: &mut Peekable<T>, whitespace: &mut Vec<Token<'input>>)
    where T: Iterator<Item=Token<'input>>
{
    let leading_whitespace = tokens.peeking_take_while(|&Token { ty, .. }| ty == TokenType::Whitespace);
    whitespace.extend(leading_whitespace);
}

fn parse_whitespace<'input, T>(tokens: &mut Peekable<T>) -> Result<Vec<Token<'input>>, ParseError>
    where T: Iterator<Item=Token<'input>>
{
    let whitespace = tokens.peeking_take_while(|&Token { ty, .. }| ty == TokenType::Whitespace)
        .collect::<Vec<_>>();
    if whitespace.is_empty() {
        Err(ParseError::Misc("Missing required whitespace.".to_string()))
    } else {
        Ok(whitespace)
    }
}

fn parse_separator<'input, T>(tokens: &mut Peekable<T>) -> Result<Vec<Token<'input>>, ParseError>
    where T: Iterator<Item=Token<'input>>
{
    let separator = tokens.peeking_take_while(|&Token { ty, .. }| ty == TokenType::Whitespace || ty == TokenType::Comma)
        .collect::<Vec<_>>();
    let num_commas = separator.iter()
        .filter(|&Token { ty, .. }| *ty == TokenType::Comma)
        .count();
    if num_commas > 1 {
        Err(ParseError::Misc("Too many comma separators.".to_string()))
    } else if separator.is_empty() {
        Err(ParseError::Misc("Missing separator.".to_string()))
    } else {
        Ok(separator)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::ir1_simple_lines::parse_simple_lines;

    #[test]
    fn add() {
        let lexer = Lexer::new("ADD R0, R0, R0");
        let simple_lines = parse_simple_lines(lexer);
        let lines = parse_lines(simple_lines);
        let Line { content, .. } = lines.get(0).unwrap();
        let matches = if let LineContent::Valid(None, Some(operation_tokens)) = content {
            if let OperationTokens { operands: OperandTokens::Add { .. }, ..} = operation_tokens {
                true
            } else { false }
        } else { false };
        assert!(matches);
    }

    #[test]
    fn labeled_add() {
        let lexer = Lexer::new("LABEL\n\tADD R0, R1, #1");
        let simple_lines = parse_simple_lines(lexer);
        let lines = parse_lines(simple_lines);

        let Line { content, .. } = lines.get(0).unwrap();
        let line_0_matches = if let LineContent::Valid(Some(_), None) = content { true } else { false };
        assert!(line_0_matches);

        let Line { content, .. } = lines.get(1).unwrap();
        let line_1_matches = if let LineContent::Valid(None, Some(operation_tokens)) = content {
            if let OperationTokens { operands: OperandTokens::Add { .. }, .. } = operation_tokens {
                true
            } else { false }
        } else { false };
        assert!(line_1_matches);
    }

}

