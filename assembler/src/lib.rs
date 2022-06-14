
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

type Span = std::ops::Range<usize>;
type Spanned<T> = (T, Span);

#[derive(Copy, Clone)]
pub enum LeniencyLevel {
    Lenient,
    Strict
}

const MEM_DUMP_FILE_EXTENSION: &'static str = "mem";

impl From<std::io::Error> for analysis::SingleError {
    fn from(error: std::io::Error) -> Self {
        analysis::SingleError::Io(error)
    }
}

impl<E> From<Vec<E>> for analysis::Error
    where E: Into<analysis::Error>
{
    fn from(errors: Vec<E>) -> Self {
        let es = errors.into_iter()
            .map(|e| e.into())
            .collect();
        analysis::Error::Multiple(es)
    }
}

impl<E> From<E> for analysis::Error
    where E: Into<analysis::SingleError>
{
    fn from(error: E) -> Self {
        analysis::Error::Single(error.into())
    }
}

impl From<chumsky::error::Simple<char>> for analysis::SingleError {
    fn from(error: chumsky::error::Simple<char>) -> Self {
        analysis::SingleError::Lex(error)
    }
}

impl From<chumsky::error::Simple<lexer::Token>> for analysis::SingleError {
    fn from(error: chumsky::error::Simple<lexer::Token>) -> Self {
        analysis::SingleError::Parse(error)
    }
}

pub fn parse_and_analyze_file(input: PathBuf, leniency: LeniencyLevel) -> Result<parser::File, analysis::Error> {
    let src = fs::read_to_string(input)?;
    parse_and_analyze(&src, leniency)
}

pub fn parse_and_analyze(src: &String, leniency: LeniencyLevel) -> Result<parser::File, analysis::Error> {
    let (tokens, lex_data) = lexer::lex(src, leniency)?;
    let file_spanned = parser::parse(src, tokens, leniency)?;
    let errors = analysis::validate(&lex_data, &file_spanned);
    if !errors.is_empty() {
        return Err(errors.into());
    }
    let (file, _) = file_spanned;
    Ok(file)
}

pub fn assemble_file(input: PathBuf, leniency: LeniencyLevel, no_os: bool) -> Result<lc3_isa::util::MemoryDump, analysis::Error> {
    let src = fs::read_to_string(input)?;
    assemble(&src, leniency, no_os)
}

pub fn assemble(src: &String, leniency: LeniencyLevel, no_os: bool) -> Result<lc3_isa::util::MemoryDump, analysis::Error> {
    let file = parse_and_analyze(src, leniency)?;
    let object = assembler::assemble(file).map_err(|_| analysis::SingleError::Assemble)?;
    let mem = linker::link([object], !no_os)?;
    Ok(mem)
}
