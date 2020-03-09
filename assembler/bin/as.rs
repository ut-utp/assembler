extern crate lc3_assembler;

use std::{env, fs};
use std::path::{Path, PathBuf};
use lc3_assembler::lexer::Lexer;
use lc3_assembler::parser::{parse, LeniencyLevel};
use lc3_assembler::assembler::assemble;
use lc3_shims::memory::FileBackedMemoryShim;
use clap::clap_app;
use lc3_assembler::parser::LeniencyLevel::*;
use lc3_assembler::cst;
use lc3_assembler::error::ParseError;
use lc3_assembler::cst::{Object, ObjectContent, Operation, Operands};
use annotate_snippets::formatter::DisplayListFormatter;
use annotate_snippets::display_list::DisplayList;

const MEM_DUMP_FILE_EXTENSION: &'static str = "mem";

fn main() {
    let matches = clap_app!(as =>
        (version: env!("CARGO_PKG_VERSION"))
        (author: env!("CARGO_PKG_AUTHORS"))
        (about: env!("CARGO_PKG_DESCRIPTION"))
        (@arg strict: -s --strict "Enforces all rules of the original LC-3 assembly language when validating the program")
        (@arg check:  -c --check  "Check the correctness of the program without attempting to assemble it")
        (@arg INPUT: +required ... "Paths to the programs to assemble")
    ).get_matches();

    for path_str in matches.values_of("INPUT").unwrap() {
        let path = Path::new(path_str);
        assert!(path.is_file());

        let leniency = if matches.is_present("strict") { Strict } else { Lenient };

        let string = fs::read_to_string(path).unwrap();
        let lexer = Lexer::new(string.as_str());
        let cst = parse(lexer, leniency);


        let dlf = DisplayListFormatter::new(true, false);
        let errors = extract_file_errors(cst.clone());
        if errors.len() > 0 {
            for error in errors {
                let snippet = error.create_snippet(string.clone(), Some(path_str.to_string()));
                let dl = DisplayList::from(snippet);
                println!("{}", dlf.format(&dl));
            }
            break;
        }

        if matches.is_present("check") {
            println!("{}: No errors found.", path_str);
        } else {
            let mem = assemble(cst.objects);

            let mut output_path = PathBuf::from(path_str);
            output_path.set_extension(MEM_DUMP_FILE_EXTENSION);
            let mut file_backed_mem = FileBackedMemoryShim::with_initialized_memory(output_path, mem);
            file_backed_mem.flush().unwrap();
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
