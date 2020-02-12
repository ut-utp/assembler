use crate::lexer::Token;
use crate::ir2_lines::{OperationTokens, Label, Line};

#[derive(Clone)]
pub struct UnvalidatedFile<'input> {
    pub objects: Vec<UnvalidatedObject<'input>>,
    pub ignored: Vec<Line<'input>>,
}

#[derive(Clone)]
pub struct UnvalidatedObject<'input> {
    pub operations: Vec<UnvalidatedLine<'input>>,
    pub empty_lines: Vec<Line<'input>>,
    pub hanging_labels: Vec<Line<'input>>,
    pub invalid_lines: Vec<Line<'input>>,
}

#[derive(Clone)]
pub struct UnvalidatedLine<'input> {
    pub label: Option<Label<'input>>,
    pub operation: OperationTokens<'input>,
    pub whitespace: Vec<Token<'input>>,
    pub comments: Vec<Token<'input>>,
    pub newlines: Vec<Token<'input>>,
}

