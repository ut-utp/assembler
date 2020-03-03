extern crate lc3_assembler;

use std::{env, fs};
use std::path::Path;
use lc3_assembler::lexer::Lexer;
use lc3_assembler::parser::parse;
use lc3_assembler::error::ParseError;
use lc3_assembler::cst;
use lc3_assembler::cst::{Object, Operation, ObjectContent, Operands};
use annotate_snippets::formatter::DisplayListFormatter;
use annotate_snippets::display_list::DisplayList;

fn main() {
    let args = env::args().collect::<Vec<_>>();
    for arg in args[1..].iter() {
        let path = Path::new(arg.as_str());
        assert!(path.is_file());

        let string = fs::read_to_string(path).unwrap();
        let str = string.as_str();
        let lexer = Lexer::new(str);
        let cst = parse(lexer);
        
        let dlf = DisplayListFormatter::new(true, false);
        let errors = extract_file_errors(cst);
        if errors.len() > 0 {
            for error in errors {
                let snippet = error.create_snippet(string.clone(), Some(arg.clone()));
                let dl = DisplayList::from(snippet);
                println!("{}", dlf.format(&dl));
            }
        } else {
            println!("{}: found no errors", path.display());
        }

    }
}

fn extract_file_errors(cst: cst::File) -> Vec<ParseError> {
    let mut errors = Vec::new();
    
    let cst::File { objects, .. } = cst;
    if objects.len() == 0 {
        errors.push(ParseError::Misc("File contained no objects.".to_string()));
    }
    
    for object in objects {
        errors.extend(extract_object_errors(object))
    }
    
    errors
}

fn extract_object_errors(object: Object) -> Vec<ParseError> {
    let mut errors = Vec::new();
    
    let Object { origin, content, .. } = object;
    
    origin.extract_error_into(&mut errors);
    errors.extend(extract_object_content_errors(content));
    
    errors
}

fn extract_object_content_errors(object_content: ObjectContent) -> Vec<ParseError> {
    let mut errors = Vec::new();
    
    let ObjectContent { operations, hanging_labels, invalid_lines, .. } = object_content;
    
    for operation in operations {
        errors.extend(extract_operation_errors(operation));
    }
    
    for _ in hanging_labels {
        errors.push(ParseError::Misc("Hanging label.".to_string()));
    }
    
    for _ in invalid_lines {
        errors.push(ParseError::Misc("Invalid line.".to_string()));
    }
    
    errors
}

fn extract_operation_errors(operation: Operation) -> Vec<ParseError> {
    let mut errors = Vec::new();
    
    let Operation { label, operands, .. } = operation;
    
    if let Some(label) = label {
        label.extract_error_into(&mut errors);
    }
    
    errors.extend(extract_operands_errors(operands));
    
    errors
}

fn extract_operands_errors(operands: Operands) -> Vec<ParseError> {
    let mut errors = Vec::new();

    match operands {
        Operands::Add { dr, sr1, sr2_or_imm5 } => {
            dr.extract_error_into(&mut errors);
            sr1.extract_error_into(&mut errors);
            if let Err(error) = sr2_or_imm5 {
                errors.push(error);
            }
        },
        Operands::And { dr, sr1, sr2_or_imm5 } => {
            dr.extract_error_into(&mut errors);
            sr1.extract_error_into(&mut errors);
            if let Err(error) = sr2_or_imm5 {
                errors.push(error);
            }
        },
        Operands::Br { nzp, label, .. } => {
            if let Err(error) = nzp {
                errors.push(error);
            }
            label.extract_error_into(&mut errors);
        },
        Operands::Jmp { base } => {
            base.extract_error_into(&mut errors);
        },
        Operands::Jsr { label } => {
            label.extract_error_into(&mut errors);
        },
        Operands::Jsrr { base } => {
            base.extract_error_into(&mut errors);
        },
        
        Operands::Ld { dr, label } => {
            dr.extract_error_into(&mut errors);
            label.extract_error_into(&mut errors);
        },
        Operands::Ldi { dr, label } => {
            dr.extract_error_into(&mut errors);
            label.extract_error_into(&mut errors);
        },
        Operands::Ldr { dr, base, offset6 } => {
            dr.extract_error_into(&mut errors);
            base.extract_error_into(&mut errors);
            offset6.extract_error_into(&mut errors);
        },
        Operands::Lea { dr, label } => {
            dr.extract_error_into(&mut errors);
            label.extract_error_into(&mut errors);
        },
        Operands::Not { dr, sr } => {
            dr.extract_error_into(&mut errors);
            sr.extract_error_into(&mut errors);
        },
        Operands::St { sr, label } => {
            sr.extract_error_into(&mut errors);
            label.extract_error_into(&mut errors);
        }
        Operands::Sti { sr, label } => {
            sr.extract_error_into(&mut errors);
            label.extract_error_into(&mut errors);
        }
        Operands::Str { sr, base, offset6 } => {
            sr.extract_error_into(&mut errors);
            base.extract_error_into(&mut errors);
            offset6.extract_error_into(&mut errors);
        }
        Operands::Trap { trap_vec } => {
            trap_vec.extract_error_into(&mut errors);
        }
        Operands::Orig { origin } => {
            origin.extract_error_into(&mut errors);
        }
        Operands::Fill { value } => {
            value.extract_error_into(&mut errors);
        }
        Operands::Blkw { size, .. } => { 
            size.extract_error_into(&mut errors);
        }
        Operands::Stringz { .. } => {}

        // Putting these in instead of _ to avoid forgetting to change
        Operands::Ret => {}
        Operands::Rti => {}
        Operands::Getc => {}
        Operands::Out => {}
        Operands::Puts => {}
        Operands::In => {}
        Operands::Putsp => {}
        Operands::Halt => {}
        Operands::End => {}
    };

    errors
}
