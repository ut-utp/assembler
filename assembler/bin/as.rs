extern crate lc3_assembler;

use std::{env, fs};
use std::path::{Path, PathBuf};
use lc3_assembler::lexer::Lexer;
use lc3_assembler::parser::parse;
use lc3_shims::memory::FileBackedMemoryShim;
use clap::clap_app;
use lc3_assembler::parser::LeniencyLevel::*;
use lc3_assembler::error::{extract_file_errors, ParseError};
use annotate_snippets::display_list::{DisplayList, FormatOptions};
use annotate_snippets::snippet::{Snippet, Annotation, Slice, AnnotationType, SourceAnnotation};

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
        (@arg strict: -s --strict "Enforces all rules of the original LC-3 assembly language when validating the program")
        (@arg check:  -c --check  "Checks the correctness of the program without attempting to assemble it")
        (@arg with_os:  -o --with_os  "Overlays the program onto an image of the OS from lc3-os")
        (@arg INPUT: +required ... "Paths to the programs to assemble")
    ).get_matches();

    for path_str in matches.values_of("INPUT").unwrap() {
        let path = Path::new(path_str);
        assert!(path.is_file());

        let leniency = if matches.is_present("strict") { Strict } else { Lenient };

        let string = fs::read_to_string(path).unwrap();
        let src = string.as_str();
        let lexer = Lexer::new(src);
        let cst = parse(lexer, leniency);

        let errors = extract_file_errors(cst.clone());
        if errors.len() > 0 {
            for error in errors {
                let label_string = error.message();
                let label = label_string.as_str();
                let annotations = error.annotations();
                let slices = slices(annotations, src, Some(path_str));
                let snippet = create_snippet(label, slices);
                let dl = DisplayList::from(snippet);
                println!("{}", dl);
            }
            break;
        }

        if matches.is_present("check") {
            println!("{}: No errors found.", path_str);
        } else {
            let background = if matches.is_present("with_os") { Some(lc3_os::OS_IMAGE.clone()) } else { None };
            let mem = assemble(cst.objects, background);

            let mut output_path = PathBuf::from(path_str);
            output_path.set_extension(MEM_DUMP_FILE_EXTENSION);
            let mut file_backed_mem = FileBackedMemoryShim::with_initialized_memory(output_path, mem);
            file_backed_mem.flush_all_changes().unwrap();
        }
    }
}

fn create_snippet<'input>(label: &'input str, slices: Vec<Slice<'input>>) -> Snippet<'input> {
    Snippet {
        title: Some(Annotation {
            label: Some(label),
            id: None,
            annotation_type: AnnotationType::Error
        }),
        footer: vec![],
        slices,
        opt: FormatOptions { color: true, anonymized_line_numbers: false }
    }
}

pub fn slices<'input>(annotations: Vec<SourceAnnotation<'input>>, source: &'input str, origin: Option<&'input str>) -> Vec<Slice<'input>> {
    let mut slices = Vec::new();
    if !annotations.is_empty() {
        slices.push(
            Slice {
                source,
                origin,
                line_start: 1,
                fold: true,
                annotations,
            }
        );
    }
    slices
}
