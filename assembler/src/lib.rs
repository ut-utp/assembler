
// TODO: docs
// TODO: denys
// TODO: docs URL

extern crate core;

use std::fs;
use std::path::PathBuf;

pub mod lexer;
pub mod parser;
pub mod assembler;
pub mod linker;
pub mod analysis;
pub mod error;
mod util;

type Span = std::ops::Range<usize>;
type Spanned<T> = (T, Span);

#[derive(Copy, Clone)]
pub enum LeniencyLevel {
    Lenient,
    Strict
}

pub fn parse_and_analyze_file(input: PathBuf, leniency: LeniencyLevel) -> Result<parser::File, error::Error> {
    let src = fs::read_to_string(input)?;
    parse_and_analyze(&src, leniency)
}

pub fn parse_and_analyze(src: &String, leniency: LeniencyLevel) -> Result<parser::File, error::Error> {
    let (tokens, lex_data) = lexer::lex(src, leniency)?;
    let file_spanned = parser::parse(src, tokens, leniency)?;
    let errors = analysis::validate(&lex_data, &file_spanned);
    if !errors.is_empty() {
        return Err(errors.into());
    }
    let (file, _) = file_spanned;
    Ok(file)
}

pub fn assemble_file(input: PathBuf, leniency: LeniencyLevel, no_os: bool) -> Result<lc3_isa::util::MemoryDump, error::Error> {
    let src = fs::read_to_string(input)?;
    assemble(&src, leniency, no_os)
}

pub fn assemble(src: &String, leniency: LeniencyLevel, no_os: bool) -> Result<lc3_isa::util::MemoryDump, error::Error> {
    let file = parse_and_analyze(src, leniency)?;
    let object = assembler::assemble(file).map_err(|_| error::SingleError::Assemble)?;
    let mem = linker::link([object], !no_os)?;
    Ok(mem)
}
