use crate::lexer::{Lexer, Token, TokenType, Op, NamedTrap, PseudoOp};
use crate::ir1_simple_lines::{SimpleLines, SimpleLine};
use std::iter::Peekable;
use itertools::Itertools;
use crate::ir2_lines::{Lines, Line, OperandTokens, LineContent, OperationTokens, Label};
use crate::lexer::Op::Opcode;
use crate::error::ParseError;
//use crate::expanded;
//
//pub fn parse<'input>(lexer: &mut Lexer<'input>) -> expanded::File {
//
//}

fn parse_simple_lines<'input>(lexer: &mut Lexer<'input>) -> SimpleLines<'input> {
    let mut tokens = lexer.peekable();
    let mut simple_lines = Vec::new();
    while tokens.peek().is_some() {
        let simple_line = parse_simple_line(&mut tokens);
        simple_lines.push(simple_line);
    }
    simple_lines
}

fn parse_simple_line<'input>(tokens: &mut Peekable<&mut Lexer<'input>>) -> SimpleLine<'input> {
    let content = tokens.peeking_take_while(|&Token { ty, .. }|
        ty != TokenType::Comment && ty != TokenType::Newline)
        .collect();
    let next = tokens.next();
    let (comment, newline) = match next {
        Some(Token { ty, .. }) => match ty {
            TokenType::Comment => {
                let newline = tokens.next();
                if let Some(Token { ty, .. }) = newline {
                    assert_eq!(ty, TokenType::Newline);
                }
                (next, newline)
            }
            TokenType::Newline => (None, next),
            _ => unreachable!("Found more non-comment, non-newline content after skipping to comment or newline."),
        }
        None => (None, None),
    };
    SimpleLine { content, comment, newline }
}

fn parse_lines<'input>(simple_lines: &SimpleLines<'input>) -> Lines<'input> {
    simple_lines.iter()
        .map(parse_line)
        .collect()
}

fn parse_line<'input>(simple_line: &SimpleLine<'input>) -> Line<'input> {
    let SimpleLine { content: old_content, comment, newline } = simple_line;
    let comment = comment.clone();
    let newline = newline.clone();

    let mut tokens = old_content.iter().peekable();
    let mut whitespace = Vec::new();
    parse_whitespace(&tokens, &whitespace);
    let label = parse_label(&tokens);
    parse_whitespace(&tokens, &whitespace);
    let content = parse_operation_tokens(&tokens).map_or(
        LineContent::Invalid(old_content.clone()),
        |operation_tokens| LineContent::Valid(label, operation_tokens),
    );
    parse_whitespace(&tokens, &whitespace);
    Line { content, whitespace, comment, newline, }
}

fn parse_label<'input, T>(tokens: &mut Peekable<T>) -> Option<Label<'input>>
    where T: Iterator<Token<'input>>
{
    if let Some(Token { ty: TokenType::Word, .. }) = tokens.peek() {
        tokens.next()
    } else {
        None
    }
}

// Return None if no operation but valid line (i.e. only whitespace (optionally))
// ^^^ assumes whitespace has already been skipped.
// Return Err if line doesn't have valid pattern of tokens
fn parse_operation_tokens<'input, T>(tokens: &mut Peekable<T>) -> Result<Option<OperationTokens<'input>>, ParseError>
    where T: Iterator<Token<'input>>
{
    match tokens.next() {
        Some(token) => match token.ty {
            TokenType::Op(op) => {
                let operator = token;
                let mut separators = Vec::new();
                parse_whitespace(tokens, &separators);
                let operands = match op {
                    Op::Opcode(opcode) => match opcode {
                        Opcode::Add => {
                            let mut operand_buffer: [Option<Token<'input>>; 3] = [None; 3];
                            parse_operands(tokens, &separators, &operand_buffer)?;
                            OperandTokens::Add {
                                dr: operand_buffer[0].unwrap(),
                                sr1: operand_buffer[1].unwrap(),
                                sr2_or_imm5: operand_buffer[2].unwrap(),
                            }
                        },
                        Opcode::And => {
                            let mut operand_buffer: [Option<Token<'input>>; 3] = [None; 3];
                            parse_operands(tokens, &separators, &operand_buffer)?;
                            OperandTokens::And {
                                dr: operand_buffer[0].unwrap(),
                                sr1: operand_buffer[1].unwrap(),
                                sr2_or_imm5: operand_buffer[2].unwrap(),
                            }
                        },
                        Opcode::Br => {
                            let mut operand_buffer: [Option<Token<'input>>; 3] = [None; 3];
                            parse_operands(tokens, &separators, &operand_buffer)?;
                            OperandTokens::Br {
                                nzp: operand_buffer[0].unwrap(),
                                sr1: operand_buffer[1].unwrap(),
                                sr2_or_imm5: operand_buffer[2].unwrap(),
                            }
                        },
                        Opcode::Jmp => {

                        },
                        Opcode::Jsr,
                        Opcode::Jsrr,
                        Opcode::Ld,
                        Opcode::Ldi,
                        Opcode::Ldr,
                        Opcode::Lea,
                        Opcode::Not,
                        Opcode::Ret,
                        Opcode::Rti,
                        Opcode::St,
                        Opcode::Sti,
                        Opcode::Str,
                        Opcode::Trap,
                    },
                    Op::NamedTrap(named_trap) => match named_trap {
                        NamedTrap::Getc => {},
                        NamedTrap::Out => {},
                        NamedTrap::Puts => {},
                        NamedTrap::In => {},
                        NamedTrap::Putsp => {},
                        NamedTrap::Halt => {},
                    }
                    Op::PseudoOp(pseudo_op) => match pseudo_op {
                        PseudoOp::Orig => {},
                        PseudoOp::Fill => {},
                        PseudoOp::Blkw => {},
                        PseudoOp::Stringz => {},
                        PseudoOp::End => {},
                    }
                }
                Ok(Some(OperationTokens { operator, operands, separators }))
            }
            _ => Err(ParseError(String::new())) // TODO: handle here or elsewhere? See other empty strings in this function too
        }
        None => Ok(None),
    }
}

// Returns Ok if operands parsed correctly and fills operands with Some(token)
// Otherwise, returns Err
fn parse_operands<'input, T>(tokens: &mut Peekable<T>, separators: &mut Vec<Token<'input>>, operands: &mut [Option<Token<'input>>]) -> Result<(), ParseError>
    where T: Iterator<Token<'input>>
{
    for i in 0..operands.len() {
        if let Some(token) = tokens.next() {

        } else {
            return Err(ParseError(String::new())) // TODO
        }
    }
    Ok(())
}

fn parse_whitespace<'input, T>(tokens: &mut Peekable<T>, whitespace: &mut Vec<Token<'input>>)
    where T: Iterator<Token<'input>>
{
    let leading_whitespace = tokens.peeking_take_while(|&Token { ty, .. }| ty == TokenType::Whitespace);
    whitespace.extend(leading_whitespace);
}

#[cfg(test)]
mod tests {
    use super::*;

    use pretty_assertions::assert_eq;

    #[test]
    fn test_parse_simple_lines_no_newline() {
        let mut lexer = Lexer::new("ADD");
        let simple_lines = parse_simple_lines(&mut lexer);
        let SimpleLine { content, comment, newline } = simple_lines.get(0).unwrap();
        assert_eq!(content.len(), 1);
        assert!(comment.is_none());
        assert!(newline.is_none());
    }

    #[test]
    fn test_parse_simple_lines_two_lines() {
        let mut lexer = Lexer::new("ADD ; test\n.END");
        let simple_lines = parse_simple_lines(&mut lexer);
        let SimpleLine { content, comment, newline } = simple_lines.get(0).unwrap();
        assert_eq!(content.len(), 2);
        assert!(comment.is_some());
        assert!(newline.is_some());

        let SimpleLine { content, comment, newline } = simple_lines.get(1).unwrap();
        assert_eq!(content.len(), 1);
        assert!(comment.is_none());
        assert!(newline.is_none());
    }
}
