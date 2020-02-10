use crate::lexer::{Lexer, Token, TokenType, Op, NamedTrap, PseudoOp, Opcode};
use crate::ir1_simple_lines::{SimpleLines, SimpleLine};
use std::iter::Peekable;
use std::iter::Iterator;
use itertools::Itertools;
use crate::ir2_lines::{Lines, Line, OperandTokens, LineContent, OperationTokens, Label};
use crate::error::ParseError;
use crate::ir3_unvalidated_objects::{UnvalidatedFile, UnvalidatedObject, UnvalidatedLine};
//use crate::expanded;
//
//pub fn parse<'input>(lexer: &mut Lexer<'input>) -> expanded::File {
//
//}

//////////
// IR 1 //
//////////

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

#[cfg(test)]
mod ir1_tests {
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

//////////
// IR 2 //
//////////

fn parse_lines<'input>(simple_lines: &'input SimpleLines<'input>) -> Lines<'input> {
    simple_lines.iter()
        .map(parse_line)
        .collect()
}

fn parse_line<'input>(simple_line: &'input SimpleLine<'input>) -> Line<'input> {
    let SimpleLine { content: old_content, comment, newline } = simple_line;
    let comment = comment.clone();
    let newline = newline.clone();

    let mut tokens = old_content.iter().peekable();
    let mut whitespace = Vec::new();
    skip_and_collect_whitespace(&mut tokens, &mut whitespace);
    let label = parse_ambiguous(&mut tokens).ok();
    skip_and_collect_whitespace(&mut tokens, &mut whitespace);
    let content = parse_operation_tokens(&mut tokens, &mut whitespace).map_or(
        LineContent::Invalid(old_content.clone()),
        |operation_tokens| { LineContent::Valid(label, operation_tokens) }
    );
    skip_and_collect_whitespace(&mut tokens, &mut whitespace);
    Line { content, whitespace, comment, newline, }
}

fn parse_ambiguous<'input, T>(tokens: &mut Peekable<T>) -> Result<Token<'input>, ParseError>
    where T: Iterator<Item=&'input Token<'input>>
{
    parse_token(tokens, TokenType::Ambiguous)
}

fn parse_string<'input, T>(tokens: &mut Peekable<T>) -> Result<Token<'input>, ParseError>
    where T: Iterator<Item=&'input Token<'input>>
{
    parse_token(tokens, TokenType::String)
}

fn parse_token<'input, T>(tokens: &mut Peekable<T>, target_type: TokenType) -> Result<Token<'input>, ParseError>
    where T: Iterator<Item=&'input Token<'input>>
{
    if let Some(Token { ty, .. }) = tokens.peek() {
        if *ty == target_type {
            return Ok(tokens.next().unwrap().clone());
        }
    }
    Err(ParseError("Didn't find ambiguous token next.".to_string()))
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
        let mut operand_buffer: [Option<Token<'input>>; $num] = [None; $num];
        parse_operands($tokens, &mut $separators, &mut operand_buffer)?;
        fill_operands! { @munch (0usize, operand_buffer, $($input)+) -> { $name, } }
    };
}

fn parse_operand_tokens<'input, T>(op: Op, mut tokens: &mut Peekable<T>, mut separators: &mut Vec<Token<'input>>) -> Result<OperandTokens<'input>, ParseError>
    where T: Iterator<Item=&'input Token<'input>>
{
    let operands = match op {
        Op::Opcode(opcode) => match opcode {
            Opcode::Add => { fill_operands! { 3; Add { dr, sr1, sr2_or_imm5, }; tokens, separators } },
            Opcode::And => { fill_operands! { 3; And { dr, sr1, sr2_or_imm5, }; tokens, separators } },
            Opcode::Br => { // Specially handled due to nzp
                let nzp = parse_ambiguous(tokens)?;
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
where T: Iterator<Item=&'input Token<'input>>
{
    match tokens.next() {
        Some(token) => match token.ty {
            TokenType::Op(op) => {
                let operator = token.clone();
                let mut separators = Vec::new();
                let operands = parse_operand_tokens(op, tokens, &mut separators)?;
                skip_and_collect_whitespace(&mut tokens, &mut whitespace);
                if tokens.peek().is_some() {
                    Err(ParseError("Extra tokens at end of line.".to_string()))
                } else {
                    Ok(Some(OperationTokens { operator, operands, separators }))
                }
            }
            TokenType::Whitespace => unreachable!("Function was called without first skipping whitespace."),
            _ => Err(ParseError("Unexpected non-operator token at beginning of 'instruction'".to_string()))
        }
        None => Ok(None),
    }
}

// Returns Ok if operands parsed correctly and fills operands with Some(token)
// Otherwise, returns Err
fn parse_operands<'input, T>(tokens: &mut Peekable<T>, separators: &mut Vec<Token<'input>>, operands: &mut [Option<Token<'input>>]) -> Result<(), ParseError>
    where T: Iterator<Item=&'input Token<'input>>
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
    where T: Iterator<Item=&'input Token<'input>>
{
    let leading_whitespace = tokens.peeking_take_while(|&Token { ty, .. }| *ty == TokenType::Whitespace);
    whitespace.extend(leading_whitespace);
}

fn parse_whitespace<'input, T>(tokens: &mut Peekable<T>) -> Result<Vec<Token<'input>>, ParseError>
    where T: Iterator<Item=&'input Token<'input>>
{
    let whitespace = tokens.peeking_take_while(|&Token { ty, .. }| *ty == TokenType::Whitespace)
        .cloned()
        .collect::<Vec<_>>();
    if whitespace.is_empty() {
        Err(ParseError("Missing required whitespace.".to_string()))
    } else {
        Ok(whitespace)
    }
}

fn parse_separator<'input, T>(tokens: &mut Peekable<T>) -> Result<Vec<Token<'input>>, ParseError>
    where T: Iterator<Item=&'input Token<'input>>
{
    let separator = tokens.peeking_take_while(|&Token { ty, .. }| *ty == TokenType::Whitespace || *ty == TokenType::Comma)
        .cloned()
        .collect::<Vec<_>>();
    let num_commas = separator.iter()
        .filter(|&Token { ty, .. }| *ty == TokenType::Comma)
        .count();
    if num_commas > 1 {
        Err(ParseError("Too many comma separators.".to_string()))
    } else if separator.is_empty() {
        Err(ParseError("Missing separator.".to_string()))
    } else {
        Ok(separator)
    }
}

#[cfg(test)]
mod ir2_tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_parse_lines_add() {
        let mut lexer = Lexer::new("ADD R0, R0, R0");
        let simple_lines = parse_simple_lines(&mut lexer);
        let lines = parse_lines(&simple_lines);
        let Line { content, whitespace, comment, newline } = lines.get(0).unwrap();
        println!("{:?}", content);
        let matches = if let LineContent::Valid(None, Some(operation_tokens)) = content {
            if let OperationTokens { operator, operands, separators } = operation_tokens {
                if let OperandTokens::Add { .. } = operands {
                    true
                } else { false }
            } else { false }
        } else { false };
        assert!(matches);
    }

    #[test]
    fn test_parse_lines_label_add() {
        let mut lexer = Lexer::new("LABEL\n\tADD R0, R1, #1");
        let simple_lines = parse_simple_lines(&mut lexer);
        let lines = parse_lines(&simple_lines);

        let Line { content, whitespace, comment, newline } = lines.get(0).unwrap();
        println!("{:?}", content);
        let line_0_matches = if let LineContent::Valid(Some(_), None) = content { true } else { false };
        assert!(line_0_matches);

        let Line { content, whitespace, comment, newline } = lines.get(1).unwrap();
        println!("{:?}", content);
        let line_1_matches = if let LineContent::Valid(None, Some(operation_tokens)) = content {
            if let OperationTokens { operands, .. } = operation_tokens {
                if let OperandTokens::Add { .. } = operands {
                    true
                } else { false }
            } else { false }
        } else { false };
        assert!(line_1_matches);
    }
    
}

//////////
// IR 3 //
//////////

fn parse_unvalidated_file<'input>(lines: &'input Lines<'input>) -> UnvalidatedFile<'input> {
    let mut objects = Vec::new();
    let mut ignored = Vec::new();
    let mut lines = lines.iter().peekable();
    loop {
        let line = lines.peek();
        match line {
            None => { break; }
            Some( // Ridiculous indentation engage... (We're just matching .ORIG)
                Line {
                    content: LineContent::Valid(_, Some(
                        OperationTokens {
                            operands: OperandTokens::Orig { .. }, // <- This is the important part.
                            ..
                        }
                    )),
                    ..
                }
            ) => { // Re-engaging readability stabilizers...
                let object = parse_unvalidated_object(&mut lines);
                if let Ok(object) = object {
                    objects.push(object);
                }
            },
            Some(_) => { ignored.push(lines.next().unwrap().clone()); },
        }
    }
    UnvalidatedFile { objects, ignored }
}

fn parse_unvalidated_object<'input, T>(lines: &mut Peekable<T>) -> Result<UnvalidatedObject<'input>, ParseError>
    where T: Iterator<Item=&'input Line<'input>>
{
    let mut operations = Vec::new();
    let mut empty_lines = Vec::new();
    let mut hanging_labels = Vec::new();
    let mut invalid_lines = Vec::new();
    
    loop {
        let line = lines.next().ok_or(ParseError("Hit end of file before .END".to_string()))?;
        
        let mut whitespace = Vec::new();
        whitespace.extend(line.whitespace.clone());
        
        let mut comments = Vec::new();
        if let Some(comment) = line.comment {
            comments.push(comment.clone());
        }
        
        let mut newlines = Vec::new();
        if let Some(newline) = line.newline {
            newlines.push(newline.clone());
        }
        
        match &line.content {
            LineContent::Invalid(_) => { invalid_lines.push(line.clone()); }
            LineContent::Valid(None, None) => { empty_lines.push(line.clone()); },
            LineContent::Valid(label, None) => {
                if let Some(Line { 
                    content: LineContent::Valid(None, Some(operation)),
                    whitespace: line_ws, 
                    comment,
                    newline,
                }) = lines.peek() {
                    lines.next();
                    
                    whitespace.extend(line_ws);
                    if let Some(comment) = comment {
                        comments.push(comment.clone());
                    }
                    if let Some(newline) = newline {
                        newlines.push(newline.clone());
                    }
                    let unvalidated_line = UnvalidatedLine {
                        label: label.clone(),
                        operation: operation.clone(),
                        whitespace,
                        comments,
                        newlines,
                    };
                    operations.push(unvalidated_line);
                    if let OperationTokens { operands: OperandTokens::End, .. } = operation {
                        break;
                    }
                } else {
                    hanging_labels.push(line.clone());
                }
            },
            LineContent::Valid(label, Some(operation)) => {
                let unvalidated_line = UnvalidatedLine {
                    label: label.clone(),
                    operation: operation.clone(),
                    whitespace,
                    comments,
                    newlines,
                };
                operations.push(unvalidated_line);
                if let OperationTokens { operands: OperandTokens::End, .. } = operation {
                    break;
                }
            },
        }
    }
    
    Ok(UnvalidatedObject { operations, empty_lines, hanging_labels, invalid_lines })
}