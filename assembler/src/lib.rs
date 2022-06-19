
// TODO: docs
// TODO: denys
// TODO: docs URL

extern crate core;

use std::fs;
use std::path::PathBuf;

mod util;

pub mod error;

pub mod lex;
pub mod parse;
pub mod analyze;
pub mod assemble;
pub mod link;
pub mod layer;

type Span = std::ops::Range<usize>;
type Spanned<T> = (T, Span);

#[derive(Copy, Clone)]
pub enum LeniencyLevel {
    Lenient,
    Strict
}

pub fn parse_and_analyze_file(input: PathBuf, leniency: LeniencyLevel) -> Result<parse::File, error::Error> {
    let src = fs::read_to_string(input)?;
    parse_and_analyze(&src, leniency)
}

pub fn parse_and_analyze(src: &String, leniency: LeniencyLevel) -> Result<parse::File, error::Error> {
    let (tokens, lex_data) = lex::lex(src, leniency)?;
    let file_spanned = parse::parse(src, tokens, leniency)?;
    let errors = analyze::validate(&lex_data, &file_spanned);
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
    let assemble::Object { symbol_table, regions } = assemble::assemble(file).map_err(|_| error::SingleError::Assemble)?;
    let linked_regions = link::link_regions(&symbol_table, regions)?;
    let mem = layer::layer(linked_regions, !no_os)?;
    Ok(mem)
}
