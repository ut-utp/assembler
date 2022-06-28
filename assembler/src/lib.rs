// TODO: docs
// TODO: docs URL
#![doc = include_str!("../README.md")]

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

/// An identifier for a unique source file. Produced by [`id`].
///
/// Used in error messages to indicate
/// which source file the text in the error is from.
pub type SourceId = String;


/// Data indicating a substring in a specific source file.
///
/// Primarily used to identify the exact source code which caused an error.
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

/// The level of leniency to be used when parsing and identifying errors.
///
/// Officially, LC-3 assembly follows strict syntax rules which can be inconvenient.
/// For example, labels officially cannot exceed 20 characters.
/// To enforce these rules, use [`LeniencyLevel::Strict`].
///
/// [`LeniencyLevel::Lenient`] allows the following:
/// (TODO)
#[derive(Copy, Clone)]
pub enum LeniencyLevel {
    /// Indicates that all convenience features (described under [`LeniencyLevel`]) are to be allowed.
    Lenient,

    /// Indicates that all official rules of the LC-3 assembly language
    /// are to be followed, as described in *Introduction to Computing Systems: from Bits & Gates to C/C++ & Beyond*,
    /// by Patt and Patel.
    Strict
}


/// Read and cache the given source files for use in printing error messages.
///
/// To print error messages correctly, this function must
/// be given the same paths to the source files which were
/// input to the function which returned the errors,
/// then the resulting cache must be passed to the error printing function.
///
/// This function reads all the given source files to memory,
/// so be aware that large files may cause significant memory usage.
pub fn sources(iter: impl IntoIterator<Item=PathBuf>) -> Result<impl ariadne::Cache<SourceId>, std::io::Error> {
    let sources = iter.into_iter()
        .map(|input| Ok((id(&input), read(&input)?)))
        .collect::<Result<Vec<_>, std::io::Error>>()?;
     Ok(ariadne::sources(sources))
}


/// Read all of the given file.
pub fn read(input: &PathBuf) -> Result<String, std::io::Error> {
    fs::read_to_string(input.clone())
}


/// Get a [`SourceId`] for the given source file.
pub fn id(input: &PathBuf) -> SourceId {
    input.to_string_lossy().to_string()
}


/// Check whether the given file contains valid LC-3 assembly code.
///
/// Reads the given file, then parses and analyzes its contents for errors,
/// returning a syntax tree if successful,
/// and otherwise, the errors that were found.
///
/// # Examples
/// ## Success
/// `add.asm`:
/// ```asm
#[doc = include_str!("../docs/tests/add.asm")]
/// ```
/// ```ignore
/// # use lc3_assembler::*;
/// let src_path = std::path::PathBuf::from("../docs/tests/add.asm");
/// let result = parse_and_analyze_file(&src_path, LeniencyLevel::Lenient);
/// assert!(result.is_ok());
/// ```
///
/// ## Error
/// `bad_operand.asm`:
/// ```asm
#[doc = include_str!("../docs/tests/bad_operand.asm")]
/// ```
/// ```ignore
/// # use lc3_assembler::*;
/// use assert_matches::assert_matches;
/// let src_path = std::path::PathBuf::from("../docs/tests/bad_operand.asm");
/// let error = parse_and_analyze_file(&src_path, LeniencyLevel::Lenient).unwrap_err();
/// let first_error = error.get_first_single_error().unwrap();
/// assert_matches!(first_error, error::SingleError::BadOperand);
/// ```
pub fn parse_and_analyze_file(input: &PathBuf, leniency: LeniencyLevel) -> Result<parse::File, error::Error> {
    let id = id(&input);
    let src = read(input).map_err(|e| (id.clone(), e))?;
    parse_and_analyze(&id, &src, leniency)
}


/// Check whether the given `String` is valid LC-3 assembly code.
///
/// Parses, then analyzes the given `String` for errors,
/// returning a syntax tree if successful,
/// and otherwise, the errors that were found.
///
#[doc = include_str!("../docs/id_arg.md")]
///
/// # Examples
/// ## Success
/// `add.asm`:
/// ```asm
#[doc = include_str!("../docs/tests/add.asm")]
/// ```
/// ```
/// # use lc3_assembler::*;
/// let src = include_str!("../docs/tests/add.asm").to_string();
/// let src_id = id(&std::path::PathBuf::from("../docs/tests/add.asm"));
/// let result = parse_and_analyze(&src_id, &src, LeniencyLevel::Lenient);
/// assert!(result.is_ok());
/// ```
///
/// ## Error
/// `bad_operand.asm`:
/// ```asm
#[doc = include_str!("../docs/tests/bad_operand.asm")]
/// ```
/// ```
/// # use lc3_assembler::*;
/// use assert_matches::assert_matches;
/// let src = include_str!("../docs/tests/bad_operand.asm").to_string();
/// let src_id = id(&std::path::PathBuf::from("../docs/tests/bad_operand.asm"));
/// let error = parse_and_analyze(&src_id, &src, LeniencyLevel::Lenient).unwrap_err();
/// let first_error = error.get_first_single_error().unwrap();
/// assert_matches!(first_error, error::SingleError::BadOperand);
/// ```
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


/// Fully assemble the contents of the given file.
///
/// Reads the given file, then parses, analyzes, assembles, and links its contents,
/// returning an LC-3 executable image if successful,
/// and otherwise, the error(s) that were found.
///
#[doc = include_str!("../docs/no_os_arg.md")]
///
/// # Examples
/// `add.asm`:
/// ```asm
#[doc = include_str!("../docs/tests/add.asm")]
/// ```
/// ```ignore
/// # use lc3_assembler::*;
/// # fn main() -> Result<(), error::Error> {
/// let src_path = std::path::PathBuf::from("../docs/tests/add.asm");
/// let mem = assemble_file(&src_path, LeniencyLevel::Lenient, false)?;
/// assert_eq!(mem[0x3000], 0x1000);
/// # Ok(())
/// # }
/// ```
pub fn assemble_file(input: &PathBuf, leniency: LeniencyLevel, no_os: bool) -> Result<lc3_isa::util::MemoryDump, error::Error> {
    let id = id(&input);
    let src = read(input).map_err(|e| (id.clone(), e))?;
    assemble(&id, &src, leniency, no_os)
}


/// Fully assemble the given `String`.
///
/// Parses, analyzes, assembles, then links the given `String`,
/// returning an LC-3 executable image if successful,
/// and otherwise, the error(s) that were found.
///
#[doc = include_str!("../docs/id_arg.md")]
///
#[doc = include_str!("../docs/no_os_arg.md")]
///
/// # Examples
/// `add.asm`:
/// ```asm
#[doc = include_str!("../docs/tests/add.asm")]
/// ```
/// ```
/// # use lc3_assembler::*;
/// # fn main() -> Result<(), error::Error> {
/// let src = include_str!("../docs/tests/add.asm").to_string();
/// let src_id = id(&std::path::PathBuf::from("../docs/tests/add.asm"));
/// let mem = assemble(&src_id, &src, LeniencyLevel::Lenient, false)?;
/// assert_eq!(mem[0x3000], 0x1000);
/// # Ok(())
/// # }
/// ```
pub fn assemble(id: &SourceId, src: &String, leniency: LeniencyLevel, no_os: bool) -> Result<lc3_isa::util::MemoryDump, error::Error> {
    let file = parse_and_analyze(id, src, leniency)?;
    let assemble::Object { symbol_table, regions } = assemble::assemble(file).map_err(|_| (id.clone(), error::SingleError::Assemble))?;
    let linked_regions = link::link_regions(&symbol_table, regions).map_err(|e| (id.clone(), e))?;
    let mem = layer::layer(linked_regions, !no_os).map_err(|e| (id.clone(), e))?;
    Ok(mem)
}
