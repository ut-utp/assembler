use crate::lexer::Token;
use crate::ir2_lines::{OperationTokens, Label, Line};

pub struct UnvalidatedFile<'input> {
    objects: Vec<UnvalidatedObject<'input>>,
    ignored: Vec<Line<'input>>,
}

pub struct UnvalidatedObject<'input> {
    operations: Vec<UnvalidatedLine<'input>>,
    empty_lines: Vec<Line<'input>>,
    hanging_labels: Vec<Line<'input>>,
    invalid_lines: Vec<Line<'input>>,
}

pub struct UnvalidatedLine<'input> {
    label: Option<Label<'input>>,
    operation: OperationTokens<'input>,
    whitespace: Vec<Token<'input>>,
    comment: Option<Token<'input>>,
    newline: Option<Token<'input>>,
}