extern crate lc3_assembler;

use std::{env, fs};
use std::path::{Path, PathBuf};
use std::process::exit;
use ariadne::Source;
use lc3_assembler::parser::parse;
use lc3_shims::memory::FileBackedMemoryShim;
use clap::clap_app;
use lc3_assembler::analysis::{report, validate};
use lc3_assembler::assembler::assemble;
use lc3_assembler::LeniencyLevel;
use lc3_assembler::lexer::lex;
use lc3_assembler::linker::link;

const MEM_DUMP_FILE_EXTENSION: &'static str = "mem";

fn main() {
    std::thread::Builder::new()
        .name("main_greater_stack_size".to_string())
        .stack_size(8*1024*1024)
        .spawn(as_).unwrap()
        .join().unwrap();
}

fn as_() {
    let matches = clap_app!(assemble_lc3 =>
        (version: env!("CARGO_PKG_VERSION"))
        (author: env!("CARGO_PKG_AUTHORS"))
        (about: env!("CARGO_PKG_DESCRIPTION"))
        (@arg  strict: -s --strict   "Enforces all rules of the original LC-3 assembly language when validating the program")
        (@arg   check: -c --check    "Checks the correctness of the program without attempting to assemble it")
        (@arg with_os: -o --with_os  "Overlays the program onto an image of the OS from lc3-os")
        (@arg   INPUT: +required ... "Paths to the programs to assemble")
    ).get_matches();

    for path_str in matches.values_of("INPUT").unwrap() {
        let path = Path::new(path_str);
        assert!(path.is_file());

        let leniency = if matches.is_present("strict") { LeniencyLevel::Strict } else { LeniencyLevel::Lenient };

        let string = fs::read_to_string(path).unwrap();
        let src = string.as_str();

        let (maybe_tokens, lex_errs) = lex(src, leniency);
        let tokens = maybe_tokens.expect("lexing failed");

        let (maybe_file, parse_errs) = parse(src, tokens, leniency);
        let (mut file, span) = maybe_file.expect("parsing failed");

        println!("{:?}", file);
        let errors = validate(&file);

        if !errors.is_empty() {
            for error in errors {
                let report = report(error);
                report.eprint(Source::from(src));
            }
            continue;
        }

        if matches.is_present("check") {
            println!("{}: No errors found.", path_str);
        } else {
            let background = if matches.is_present("with_os") { Some(lc3_os::OS_IMAGE.clone()) } else { None };

            let objects =
                file.into_iter()
                    .map(|program| assemble(program.0.unwrap()));

            let mem = link(objects, background);

            let mut output_path = PathBuf::from(path_str);
            output_path.set_extension(MEM_DUMP_FILE_EXTENSION);
            let mut file_backed_mem = FileBackedMemoryShim::with_initialized_memory(output_path, mem);
            file_backed_mem.flush_all_changes().unwrap();
        }
    }
}
