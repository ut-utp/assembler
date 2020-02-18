use crate::lexer::Token;
use crate::ir2_lines::{OperationTokens, Label, Line, Lines, LineContent, OperandTokens};
use std::iter::Peekable;

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

pub fn parse_unvalidated_file<'input>(lines: &'input Lines<'input>) -> UnvalidatedFile<'input> {
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
                match object {
                    Ok(object) => { objects.push(object); },
                    Err(ObjectParseError { lines_seen, ..}) => { ignored.extend(lines_seen) },
                }
            },
            Some(&line) => {
                ignored.push(line.clone());
                lines.next();
            },
        }
    }
    UnvalidatedFile { objects, ignored }
}

struct ObjectParseError<'input> {
    message: String,
    lines_seen: Vec<Line<'input>>,
}

fn parse_unvalidated_object<'input, T>(lines: &mut Peekable<T>) -> Result<UnvalidatedObject<'input>, ObjectParseError<'input>>
    where T: Iterator<Item=&'input Line<'input>>
{
    let mut operations = Vec::new();
    let mut empty_lines = Vec::new();
    let mut hanging_labels = Vec::new();
    let mut invalid_lines = Vec::new();

    let mut lines_seen = Vec::new();

    loop {
        let line = lines.next().ok_or(ObjectParseError {
            message: "Hit end of file before .END".to_string(),
            lines_seen: lines_seen.clone()
        })?;
        lines_seen.push(line.clone());

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

