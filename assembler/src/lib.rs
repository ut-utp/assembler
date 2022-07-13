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

/// A parsed syntax element, or an error if it was skipped,
/// along with any other data necessary to produce an error indicating this syntax element.
pub type WithErrData<T> = Spanned<Result<T, ()>>;

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
    pub span: Span,
}

impl SpanWithSource {
    pub fn with_dummy_source(span: Span) -> Self {
        SpanWithSource { 
            id: dummy_id(),
            span
        }
    }
    pub fn start(&self) -> usize { self.span.start }
    pub fn end(&self) -> usize { self.span.end }
}

impl From<(SourceId, Span)> for SpanWithSource {
    fn from((id, span): (SourceId, Span)) -> Self {
        Self { id, span }
    }
}

impl ariadne::Span for SpanWithSource {
    type SourceId = SourceId;

    fn source(&self) -> &Self::SourceId { &self.id }
    fn start(&self) -> usize { self.start() }
    fn end(&self) -> usize { self.end() }
}

/// The level of leniency to be used when parsing and identifying errors.
///
/// Officially, LC-3 assembly follows strict syntax rules which can be inconvenient.
/// For example, labels officially cannot exceed 20 characters.
/// To enforce these rules, use [`LeniencyLevel::Strict`].
///
/// [`LeniencyLevel::Strict`] enforces the following:
/// - Labels cannot contain underscores
/// - Labels cannot exceed 20 characters in length
/// - Labels must be defined on the same line as an instruction, not separately on a previous line
/// - Qualified number literals cannot be prefixed with `0` (i.e., `0x3000` is not allowed, only `x3000`)
/// - Operands must be separated with commas (`,`), not just whitespace.
/// - Condition codes for BR instructions *must* be listed in the following order: `n`, `z`, then `p`.
// NOTE TO DEVS (THIS SHOULD NOT BE IN THE DOCS):
// When updating this list, remember to update the command line app's list.
#[derive(Copy, Clone)]
pub enum LeniencyLevel {
    /// Indicates that all convenience features are to be allowed.
    Lenient,

    /// Indicates that all official rules of the LC-3 assembly language
    /// are to be followed, as described in *Introduction to Computing Systems: from Bits & Gates to C/C++ & Beyond (3rd ed.)*,
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
///
/// If working with source code that isn't from a file,
/// you may use the id of an arbitrary path (even `""`) for
/// functions requiring a [`SourceId`], or use [`dummy_id`].
pub fn id(input: &PathBuf) -> SourceId {
    input.to_string_lossy().to_string()
}

/// Get a [`SourceId`] representing no source file.
pub fn dummy_id() -> SourceId {
    id(&PathBuf::from(""))
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
pub fn parse_and_analyze_file(input: &PathBuf, leniency: LeniencyLevel) -> Result<Spanned<parse::File>, error::Error> {
    Assembly::<&PathBuf>::new(input, leniency)
        .read()?
        .parse_and_analyze()
        .map(|a| a.file())
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
/// let result = parse_and_analyze(Some(src_id), src, LeniencyLevel::Lenient);
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
/// let error = parse_and_analyze(Some(src_id), src, LeniencyLevel::Lenient).unwrap_err();
/// let first_error = error.get_first_single_error().unwrap();
/// assert_matches!(first_error, error::SingleError::BadOperand);
/// ```
pub fn parse_and_analyze(id: Option<SourceId>, source: String, leniency: LeniencyLevel) -> Result<Spanned<parse::File>, error::Error> {
    Assembly::<String>::new(id, source, leniency)
        .parse_and_analyze()
        .map(|a| a.file())
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
    Assembly::<&PathBuf>::new(input, leniency)
        .read()?
        .assemble(!no_os)
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
/// let mem = assemble(Some(src_id), src, LeniencyLevel::Lenient, false)?;
/// assert_eq!(mem[0x3000], 0x1000);
/// # Ok(())
/// # }
/// ```
pub fn assemble(id: Option<SourceId>, source: String, leniency: LeniencyLevel, no_os: bool) -> Result<lc3_isa::util::MemoryDump, error::Error> {
    Assembly::<String>::new(id, source, leniency)
        .assemble(!no_os)
}


pub struct Assembly<S> {
    state: S,
    id: SourceId,
    leniency: LeniencyLevel,
}

impl<S> Assembly<S> {
    fn map_state<F, T>(self, f: F) -> Assembly<T>
        where F: FnOnce(S) -> T
    {
        Assembly {
            state: f(self.state),
            id: self.id,
            leniency: self.leniency,
        }
    }
}

impl<'a> Assembly<&'a PathBuf> {
    pub fn new(path: &PathBuf, leniency: LeniencyLevel) -> Assembly<&PathBuf> {
        let id = id(path);
        Assembly {
            state: path,
            id,
            leniency,
        }
    }

    pub fn read(self) -> Result<Assembly<String>, error::Error> {
        match read(self.state) {
            Ok(source) => Ok(self.map_state(|_| source)),
            Err(e) => Err((self.id, e).into())
        }
    }
}

impl Assembly<String> {
    pub fn new(id: Option<SourceId>, source: String, leniency: LeniencyLevel) -> Assembly<String> {
        let id = id.unwrap_or(dummy_id());
        Assembly {
            state: source,
            id,
            leniency,
        }
    }

    pub fn assemble(self, with_os: bool) -> Result<lc3_isa::util::MemoryDump, error::Error> {
        self.parse_and_analyze()?
            .assemble()?
            .link()?
            .layer(with_os)
    }

    pub fn parse_and_analyze(self) -> Result<Assembly<Validated>, error::Error> {
        self.lex()?
            .parse()?
            .validate()
            .map_err(|(_, errs)| error::Error::Multiple(errs))
    }

    pub fn lex(self) -> Result<Assembly<Lexed>, error::Error> {
        match lex::lex(&self.state, self.leniency) {
            Ok((tokens, lex_data)) =>
                Ok(self.map_state(|source|
                    Lexed {
                        source,
                        tokens,
                        lex_data
                    })),
            Err(es) => Err(error::into_multiple(self.id.clone(), es)),
        }
    }
}

pub struct Lexed {
    source: String,
    tokens: Vec<Spanned<lex::Token>>,
    lex_data: lex::LexData,
}
impl Assembly<Lexed> {
    pub fn parse(self) -> Result<Assembly<Parsed>, error::Error> {
        match parse::parse(self.id.clone(), &self.state.source, self.state.tokens, self.leniency) {
            Ok(file) =>
                Ok(Assembly {
                    state: Parsed {
                        lex_data: self.state.lex_data,
                        file
                    },
                    id: self.id,
                    leniency: self.leniency
                }),
            Err(es) => Err(error::into_multiple(self.id.clone(), es)),
        }
    }
}

pub struct Parsed {
    lex_data: lex::LexData,
    file: Spanned<parse::File>,
}
impl Parsed {
    pub fn file(self) -> Spanned<parse::File> {
        self.file
    }
}
impl Assembly<Parsed> {
    pub fn validate(self) -> Result<Assembly<Validated>,
                                    (Parsed, Vec<error::Error>)> {
        let errors = analyze::validate(&self.state.lex_data, &self.state.file, self.leniency);
        if errors.is_empty() {
            Ok(self.map_state(|p| Validated { file: p.file }))
        } else {
            Err((self.state, errors))
        }
    }

    pub fn file_and_errors(self) -> (Spanned<parse::File>, error::Error) {
        let errors = analyze::validate(&self.state.lex_data, &self.state.file, self.leniency);
        (self.state.file, error::Error::Multiple(errors))
    }
}

pub struct Validated {
    file: Spanned<parse::File>,
}
impl Assembly<Validated> {
    pub fn assemble(self) -> Result<Assembly<assemble::Object>, error::Error> {
        let (f, _) = self.state.file;
        match assemble::assemble(f) {
            Ok(object) =>
                Ok(Assembly {
                    state: object,
                    id: self.id,
                    leniency: self.leniency
                }),
            Err(_) => Err((self.id.clone(), error::SingleError::Assemble).into())
        }
    }

    pub fn file(self) -> Spanned<parse::File> {
        self.state.file
    }
}

impl Assembly<assemble::Object> {
    pub fn link(self) -> Result<Assembly<Vec<link::Block>>, error::Error> {
        let assemble::Object { symbol_table, blocks } = self.state;
        match link::link_object_blocks(&symbol_table, blocks) {
            Ok(blocks) =>
                Ok(Assembly {
                    state: blocks,
                    id: self.id,
                    leniency: self.leniency,
                }),
            Err(_) => Err((self.id.clone(), error::SingleError::Link).into())
        }
    }
}

impl Assembly<Vec<link::Block>> {
    pub fn layer(self, with_os: bool) -> Result<lc3_isa::util::MemoryDump, error::Error> {
        layer::layer(self.state, with_os)
            .map_err(|_| (self.id.clone(), error::SingleError::Layer).into())
    }
}
