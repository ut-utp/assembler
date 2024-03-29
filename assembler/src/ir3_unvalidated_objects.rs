use crate::lexer::Token;
use crate::ir2_lines::{OperationTokens, Label, Line, Lines, LineContent, OperandTokens};
use std::iter::Peekable;
use std::mem;

#[derive(Clone)]
pub struct UnvalidatedFile<'input> {
    pub objects: Vec<UnvalidatedObject<'input>>,
    pub ignored: Vec<Line<'input>>,
}

#[derive(Clone)]
pub struct UnvalidatedObject<'input> {
    pub origin_src: UnvalidatedLine<'input>,
    pub origin: Token<'input>,
    pub content: UnvalidatedObjectContent<'input>,
}

#[derive(Clone)]
pub struct UnvalidatedObjectContent<'input> {
    pub operations: Vec<UnvalidatedLine<'input>>,
    pub empty_lines: Vec<Line<'input>>,
    pub hanging_labels: Vec<Line<'input>>,
    pub invalid_lines: Vec<Line<'input>>,
}

#[derive(Clone)]
pub struct UnvalidatedLine<'input> {
    pub src_lines: Vec<String>,
    pub label: Option<Label<'input>>,
    pub operation: OperationTokens<'input>,
    pub whitespace: Vec<Token<'input>>,
    pub comments: Vec<Token<'input>>,
    pub newlines: Vec<Token<'input>>,
}

pub fn parse_unvalidated_file(lines: Lines) -> UnvalidatedFile {
    let mut objects = Vec::new();
    let mut ignored = Vec::new();
    let mut lines = lines.into_iter().peekable();
    loop {
        let maybe_line = lines.next();
        match maybe_line {
            None => { break; },
            Some(line) => {
                let line_backup = line.clone();
                match line {
                    Line {
                        content: LineContent::Valid(label, Some(operation)),
                        whitespace, comment, newline, src
                    } => {
                        if let OperationTokens { operands: OperandTokens::Orig { origin }, .. } = operation {
                            let mut comments = Vec::new();
                            if let Some(comment) = comment {
                                comments.push(comment);
                            }

                            let mut newlines = Vec::new();
                            if let Some(newline) = newline {
                                newlines.push(newline);
                            }
                            let origin_src = UnvalidatedLine { src_lines: vec![src], label, operation, whitespace, comments, newlines };
                            match parse_unvalidated_object_content(&mut lines) {
                                Ok(content) => { objects.push(UnvalidatedObject { origin_src, origin, content }); },
                                Err(ObjectParseError { lines_seen, .. }) => {
                                    ignored.push(line_backup);
                                    ignored.extend(lines_seen);
                                },
                            }
                        } else {
                            ignored.push(line_backup);
                        }
                    },
                    line => {
                        ignored.push(line);
                    }
                }

            }
        }
    }
    UnvalidatedFile { objects, ignored }
}

struct ObjectParseError<'input> {
    lines_seen: Vec<Line<'input>>,
}

fn parse_unvalidated_object_content<'input, T>(lines: &mut Peekable<T>) -> Result<UnvalidatedObjectContent<'input>, ObjectParseError<'input>>
    where T: Iterator<Item=Line<'input>>
{
    let mut operations = Vec::new();
    let mut empty_lines = Vec::new();
    let mut hanging_labels = Vec::new();
    let mut invalid_lines = Vec::new();

    let mut lines_seen = Vec::new();
    let mut found_end = false;

    let mut hanging_label = None;
    let mut src_lines = Vec::new();
    let mut whitespace = Vec::new();
    let mut comments = Vec::new();
    let mut newlines = Vec::new();

    loop {
        let maybe_line = lines.next();
        match maybe_line {
            None => { break; }
            Some(line) => {
                lines_seen.push(line.clone());
                let line_backup = line.clone();

                let Line { content, whitespace: line_whitespace, comment, newline, src } = line;

                if hanging_label.is_some() {
                    if let LineContent::Valid(None, _) = &content {
                    } else {
                        hanging_labels.push(hanging_label.take().unwrap());
                    }
                }

                match content {
                    LineContent::Invalid(_) => { invalid_lines.push(line_backup); }
                    LineContent::Valid(None, None) => { empty_lines.push(line_backup); },
                    LineContent::Valid(Some(_), None) => { hanging_label = Some(line_backup); },
                    LineContent::Valid(label, Some(operation)) => {
                        let label = if hanging_label.is_some() {
                            assert!(label.is_none());
                            let Line { 
                                content: label_content,
                                whitespace: label_whitespace, 
                                comment: label_comment, 
                                newline: label_newline,
                                src
                            } = hanging_label.take().unwrap();
                            
                            whitespace.extend(label_whitespace);
                            src_lines.push(src);
                            if let Some(label_comment) = label_comment { comments.push(label_comment); }
                            if let Some(label_newline) = label_newline { newlines.push(label_newline); }
                            if let LineContent::Valid(label, None) = label_content {
                                label
                            } else {
                                unreachable!("Hanging label wasn't a line with only a label! Contact the maintainers.");
                            }
                        } else {
                            label
                        };
                        
                        whitespace.extend(line_whitespace);
                        src_lines.push(src);
                        if let Some(comment) = comment { comments.push(comment); }
                        if let Some(newline) = newline { newlines.push(newline); }
                        let finished_src_lines = mem::replace(&mut src_lines, Vec::new());
                        let finished_whitespace = mem::replace(&mut whitespace, Vec::new());
                        let finished_comments = mem::replace(&mut comments, Vec::new());
                        let finished_newlines = mem::replace(&mut newlines, Vec::new());
                        if let OperationTokens { operands: OperandTokens::End, .. } = operation {
                            found_end = true;
                        }
                        let unvalidated_line = UnvalidatedLine {
                            label,
                            operation,
                            src_lines: finished_src_lines,
                            whitespace: finished_whitespace,
                            comments: finished_comments,
                            newlines: finished_newlines,
                        };
                        operations.push(unvalidated_line);
                        if found_end {
                            break;
                        }
                    },
                }
            }
        }
    }

    if found_end {
        Ok(UnvalidatedObjectContent { operations, empty_lines, hanging_labels, invalid_lines })
    } else {
        Err(ObjectParseError {
            lines_seen
        })
    }
}

