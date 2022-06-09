extern crate lc3_assembler;

use std::{env, fs};
use std::path::{Path, PathBuf};
use std::process::exit;
use ariadne::Source;
use lc3_assembler::parser::parse;
use lc3_shims::memory::FileBackedMemoryShim;
use clap::{Parser};
use lc3_assembler::analysis::{report, validate};
use lc3_assembler::assembler::assemble;
use lc3_assembler::LeniencyLevel;
use lc3_assembler::lexer::lex;
use lc3_assembler::linker::link;

const MEM_DUMP_FILE_EXTENSION: &'static str = "mem";

#[derive(Parser)]
#[clap(author, version, about,
    long_about = "Analyzes, assembles, and/or links LC-3 assembly and object files. \
                  Each given assembly file is assembled to a single object file, \
                  then all assembled or given object files are linked into a single executable image \
                  of LC-3 machine code."
      )]
struct Args {
    /// Input file paths
    #[clap(required = true, parse(from_os_str), value_name = "INPUT_FILE")]
    input: Vec<PathBuf>,

    /// Enforce all rules of the original LC-3 assembly language
    ///
    /// By default, the assembler is lenient about restrictions such as label length.
    /// This option enforces restrictions specified in Patt and Patel's Introduction to Computing Systems, 3rd edition.
    // TODO: provide full list of restrictions
    #[clap(long, short)]
    strict: bool,

    /// Check the correctness of the program without assembling
    #[clap(long, short)]
    check: bool,

    /// Link executable image without OS
    ///
    /// If not specified, the program is overlaid onto an image of the OS from lc3-os at link time.
    #[clap(long, short)]
    no_os: bool,
}

fn main() {
    std::thread::Builder::new()
        .name("main_greater_stack_size".to_string())
        .stack_size(8*1024*1024)
        .spawn(as_).unwrap()
        .join().unwrap();
}

fn as_() {
    let args = Args::parse();

    for path in args.input {
        assert!(path.is_file());

        let leniency = if args.strict { LeniencyLevel::Strict } else { LeniencyLevel::Lenient };

        let string = fs::read_to_string(path.clone()).expect(&format!("Could not read file at: {:?}", path));
        let src = string.as_str();

        let (maybe_tokens, lex_data, lex_errs) = lex(src, leniency);
        if let None = maybe_tokens {
            for lex_err in lex_errs {
                println!("Lex error: {}", lex_err);
            }
            continue;
        }
        let tokens = maybe_tokens.expect("Lexing failed, but produced no errors.");

        let (maybe_file, parse_errs) = parse(src, tokens, leniency);
        if let None = maybe_file {
            for parse_err in parse_errs {
                println!("{}", parse_err);
            }
            continue;
        }
        let spanned_file = maybe_file.expect("Parsing failed, but produced no errors.");

        let errors = validate(&lex_data, &spanned_file);

        if !errors.is_empty() {
            for error in errors {
                let report = report(error);
                report.eprint(Source::from(src));
            }
            continue;
        }

        if args.check {
            println!("{}: No errors found.", path.to_str().unwrap());
        } else {
            let mut file = spanned_file.0;
            let objects =
                file.programs.into_iter()
                    .map(|program| assemble(program.0.expect("Found invalid object.")).expect("Failed to assemble object."));

            let mem = link(objects, !args.no_os).expect("linking failed");

            let mut output_path = path.clone();
            output_path.set_extension(MEM_DUMP_FILE_EXTENSION);
            let mut file_backed_mem = FileBackedMemoryShim::with_initialized_memory(output_path, mem);
            file_backed_mem.flush_all_changes().unwrap();
        }
    }
}
