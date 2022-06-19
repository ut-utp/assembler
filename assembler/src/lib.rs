// TODO: docs
// TODO: docs URL

// TODO: add more lints?
#![deny(unused)]

use std::convert::{TryFrom, TryInto};
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
type WithErrData<T> = Spanned<Result<T, ()>>;

fn get<T>(v: &Vec<WithErrData<T>>, i: usize) -> Option<&T> {
    v.get(i)
        .and_then(|res| get_result(res).as_ref().ok())
}

fn get_result<T>(v: &WithErrData<T>) -> &Result<T, ()> {
    &v.0
}

fn result<T>(v: WithErrData<T>) -> Result<T, ()> {
    v.0
}

fn try_result<T>(maybe_v: Option<WithErrData<T>>) -> Result<T, ()> {
    result(maybe_v.ok_or(())?)
}

fn try_map<T, U, E>(maybe_v: Option<WithErrData<T>>) -> Result<U, ()> where
    U: TryFrom<T, Error=E>
{
    try_result(maybe_v)?
        .try_into()
        .map_err(|_| ())
}

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
