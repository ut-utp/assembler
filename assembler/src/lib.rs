// TODO: docs
// TODO: docs URL

// TODO: add more lints?
#![deny(unused)]

use std::convert::{TryFrom, TryInto};
use std::fmt::Debug;
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

pub type SourceId = String;

#[derive(Debug, Clone)]
pub struct SpanWithSource {
    id: SourceId,
    span: Span,
}

impl From<(SourceId, Span)> for SpanWithSource {
    fn from((id, span): (SourceId, Span)) -> Self {
        Self { id, span }
    }
}

impl ariadne::Span for SpanWithSource {
    type SourceId = SourceId;

    fn source(&self) -> &Self::SourceId { &self.id }
    fn start(&self) -> usize { self.span.start }
    fn end(&self) -> usize { self.span.end }
}

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

pub fn sources(iter: impl IntoIterator<Item=PathBuf>) -> Result<impl ariadne::Cache<SourceId>, std::io::Error> {
    let sources = iter.into_iter()
        .map(|input| Ok((id(&input), read(&input)?)))
        .collect::<Result<Vec<_>, std::io::Error>>()?;
     Ok(ariadne::sources(sources))
}

pub fn read(input: &PathBuf) -> Result<String, std::io::Error> {
    fs::read_to_string(input.clone())
}

pub fn id(input: &PathBuf) -> SourceId {
    input.to_string_lossy().to_string()
}

pub fn parse_and_analyze_file(input: &PathBuf, leniency: LeniencyLevel) -> Result<parse::File, error::Error> {
    let id = id(&input);
    let src = read(input).map_err(|e| (id.clone(), e))?;
    parse_and_analyze(&id, &src, leniency)
}

pub fn parse_and_analyze(id: &SourceId, src: &String, leniency: LeniencyLevel) -> Result<parse::File, error::Error> {
    let (tokens, lex_data) = lex::lex(src, leniency).map_err(|es| error::into_multiple(id.clone(), es))?;
    let file_spanned = parse::parse(id.clone(), src, tokens, leniency).map_err(|es| error::into_multiple(id.clone(), es))?;
    let errors = analyze::validate(&lex_data, &file_spanned);
    if !errors.is_empty() {
        return Err(errors.into());
    }
    let (file, _) = file_spanned;
    Ok(file)
}

pub fn assemble_file(input: &PathBuf, leniency: LeniencyLevel, no_os: bool) -> Result<lc3_isa::util::MemoryDump, error::Error> {
    let id = id(&input);
    let src = read(input).map_err(|e| (id.clone(), e))?;
    assemble(&id, &src, leniency, no_os)
}

pub fn assemble(id: &SourceId, src: &String, leniency: LeniencyLevel, no_os: bool) -> Result<lc3_isa::util::MemoryDump, error::Error> {
    let file = parse_and_analyze(id, src, leniency)?;
    let assemble::Object { symbol_table, regions } = assemble::assemble(file).map_err(|_| (id.clone(), error::SingleError::Assemble))?;
    let linked_regions = link::link_regions(&symbol_table, regions).map_err(|e| (id.clone(), e))?;
    let mem = layer::layer(linked_regions, !no_os).map_err(|e| (id.clone(), e))?;
    Ok(mem)
}
