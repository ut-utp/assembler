use crate::lexer::{Token, Lexer, TokenType};
use std::iter::Peekable;
use itertools::Itertools;

pub type SimpleLines<'input> = Vec<SimpleLine<'input>>;

pub struct SimpleLine<'input> {
    pub content: Vec<Token<'input>>,
    pub comment: Option<Token<'input>>,
    pub newline: Option<Token<'input>>,
}

pub fn parse_simple_lines(lexer: Lexer) -> SimpleLines {
    let mut tokens = lexer.peekable();
    let mut simple_lines = Vec::new();
    while tokens.peek().is_some() {
        let simple_line = parse_simple_line(&mut tokens);
        simple_lines.push(simple_line);
    }
    simple_lines
}

fn parse_simple_line<'input>(tokens: &mut Peekable<Lexer<'input>>) -> SimpleLine<'input> {
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

