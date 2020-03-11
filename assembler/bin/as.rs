extern crate lc3_assembler;

use std::{env, fs};
use std::path::{Path, PathBuf};
use lc3_assembler::lexer::Lexer;
use lc3_assembler::parser::parse;
use lc3_assembler::assembler::assemble;
use lc3_shims::memory::FileBackedMemoryShim;
use clap::clap_app;
use lc3_assembler::parser::LeniencyLevel::*;
use lc3_assembler::error::extract_file_errors;
use annotate_snippets::formatter::DisplayListFormatter;
use annotate_snippets::display_list::DisplayList;

const MEM_DUMP_FILE_EXTENSION: &'static str = "mem";

fn main() {
    let matches = clap_app!(assemble_lc3 =>
        (version: env!("CARGO_PKG_VERSION"))
        (author: env!("CARGO_PKG_AUTHORS"))
        (about: env!("CARGO_PKG_DESCRIPTION"))
        (@arg strict: -s --strict "Enforces all rules of the original LC-3 assembly language when validating the program")
        (@arg check:  -c --check  "Checks the correctness of the program without attempting to assemble it")
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
