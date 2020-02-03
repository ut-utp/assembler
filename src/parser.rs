use crate::lexer::{Lexer, Token, TokenType};
use crate::ir1_simple_lines::{SimpleLines, SimpleLine};
use std::iter::Peekable;
use itertools::Itertools;
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
        Some(Token { ty: TokenType::Comment, .. }) => {
            let newline = tokens.next();
            if let Some(Token { ty, .. }) = newline {
                assert_eq!(ty, TokenType::Newline); // TODO: find a non-panicky (read: hacky) way to verify this. Seems safe, though.
            }
            (next, newline)
        },
        Some(Token { ty: TokenType::Newline, .. }) => (None, next),
        Some(_) => unreachable!(),
        None => (None, None),
    };
    SimpleLine { content, comment, newline }
}

#[cfg(test)]
mod tests {
    use super::*;

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