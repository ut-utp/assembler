extern crate lc3_assembler;

use std::{env, fs};
use std::path::{Path, PathBuf};
use std::process::exit;
use ariadne::Source;
use lc3_assembler::parse::{File, parse};
use lc3_shims::memory::FileBackedMemoryShim;
use clap::{Parser};
use lc3_isa::util::MemoryDump;
use lc3_assembler::{assemble, assemble_file, LeniencyLevel, parse_and_analyze, parse_and_analyze_file};

const MEM_DUMP_FILE_EXTENSION: &'static str = "mem";

#[derive(Parser)]
#[clap(author, version, about,
    long_about = "Analyzes, assembles, and/or links LC-3 assembly and object files. \
                  Each given assembly file is assembled to a single object file, \
                  then all assembled or given object files are linked into a single executable image \
                  of LC-3 machine code."
      )]
struct Args {
    /// Input file path
    #[clap(required = true, parse(from_os_str), value_name = "INPUT_FILE")]
    input: PathBuf,

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

enum Error {
    Io(std::io::Error),
    MemoryShim(lc3_shims::memory::error::MemoryShimError),
    Assembler
}

impl From<std::io::Error> for Error {
    fn from(e: std::io::Error) -> Self {
        Error::Io(e)
    }
}

impl From<lc3_shims::memory::error::MemoryShimError> for Error {
    fn from(e: lc3_shims::memory::error::MemoryShimError) -> Self {
        Error::MemoryShim(e)
    }
}

fn as_() -> Result<(), Error> {
    let args = Args::parse();

    let leniency = if args.strict { LeniencyLevel::Strict } else { LeniencyLevel::Lenient };

    let src = fs::read_to_string(args.input.clone())?;

    if args.check {
        match parse_and_analyze(&src, leniency) {
            Ok(_) => {
                println!("{}: No errors found.", args.input.display());
                Ok(())
            }
            Err(error) => print_errors(error, &src)
        }
    } else {
        match assemble(&src, leniency, args.no_os) {
            Ok(mem) => {
                let mut output_path = args.input.clone();
                output_path.set_extension(MEM_DUMP_FILE_EXTENSION);
                let mut file_backed_mem = FileBackedMemoryShim::with_initialized_memory(output_path, mem);
                file_backed_mem.flush_all_changes()?;

                Ok(())
            }
            Err(error) => print_errors(error, &src)
        }
    }
}

fn print_errors(error: lc3_assembler::error::Error, src: &String) -> Result<(), Error> {
    let print_results =
        error.report().into_iter()
            .map(|report| report.eprint(Source::from(src)))
            .collect::<Vec<_>>();

    for print_result in print_results {
        print_result?
    }
    Err(Error::Assembler)
}
