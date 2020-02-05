use crate::lexer::Token;

pub type SimpleLines<'input> = Vec<SimpleLine<'input>>;

pub struct SimpleLine<'input> {
    pub content: Vec<Token<'input>>,
    pub comment: Option<Token<'input>>,
    pub newline: Option<Token<'input>>,
}