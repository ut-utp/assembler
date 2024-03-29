use crate::lexer::{Token, Lexer, TokenType};
use std::iter::Peekable;
use itertools::Itertools;

pub type SimpleLines<'input> = Vec<SimpleLine<'input>>;

pub struct SimpleLine<'input> {
    pub src: String,
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
        .collect::<Vec<_>>();
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

    let mut all_tokens = vec![];
    all_tokens.extend(content.clone());
    if let Some(token) = comment {
        all_tokens.push(token);
    }
    if let Some(token) = newline {
        all_tokens.push(token);
    }
    let src = reconstruct_src(all_tokens);

    SimpleLine { src, content, comment, newline }
}

fn reconstruct_src<'input>(tokens: impl IntoIterator<Item=Token<'input>>) -> String {
    let mut vec = tokens.into_iter().collect::<Vec<_>>();
    vec.sort_by_key(|token| token.span.0);
    vec.dedup();
    vec.into_iter()
        .map(|token| token.src)
        .join("")
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn no_newline() {
        let lexer = Lexer::new("ADD");
        let simple_lines = parse_simple_lines(lexer);
        let SimpleLine { src, content, comment, newline } = simple_lines.get(0).unwrap();
        assert_eq!(*src, "ADD".to_string());
        assert_eq!(content.len(), 1);
        assert!(comment.is_none());
        assert!(newline.is_none());
    }

    #[test]
    fn two_lines() {
        let lexer = Lexer::new("ADD ; test\n.END");
        let simple_lines = parse_simple_lines(lexer);
        let SimpleLine { src, content, comment, newline } = simple_lines.get(0).unwrap();
        assert_eq!(*src, "ADD ; test\n".to_string());
        assert_eq!(content.len(), 2);
        assert!(comment.is_some());
        assert!(newline.is_some());

        let SimpleLine { src, content, comment, newline } = simple_lines.get(1).unwrap();
        assert_eq!(*src, ".END".to_string());
        assert_eq!(content.len(), 1);
        assert!(comment.is_none());
        assert!(newline.is_none());
    }
}

