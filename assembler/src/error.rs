//! Error types and associated functions.

use ariadne::{Label, Report, ReportBuilder, ReportKind};
use std::cmp::max;
use lc3_isa::SignedWord;
use std::fmt::{Display, Formatter};
use crate::{SourceId, Span, SpanWithSource, util};
use crate::lex;
use crate::lex::{LiteralValue};
use crate::parse::Operand;
use std::ops::Range;


/// This crate's primary error type. Can represent multiple errors from the entire assembly process.
#[derive(Debug)]
pub enum Error {
    /// A single error and the ID of the source file which caused it.
    Single(SourceId, SingleError),
    /// A single error and a span indicating the main substring of source code which caused it.
    Spanned(SpanWithSource, SingleError),
    /// A set of errors.
    Multiple(Vec<Error>),
}

pub(crate) fn into_multiple<E>(id: SourceId, es: Vec<E>) -> Error
    where (SourceId, E): Into<Error>
{
    let errors =
        es.into_iter()
            .map(|e| (id.clone(), e).into())
            .collect();
    Error::Multiple(errors)
}

impl From<Vec<Error>> for Error
{
    fn from(es: Vec<Self>) -> Self {
        Error::Multiple(es)
    }
}

impl<E> From<(SourceId, E)> for Error
    where E: Into<SingleError>
{
    fn from((id, e): (SourceId, E)) -> Self {
        Error::Single(id, e.into())
    }
}

impl From<std::io::Error> for SingleError {
    fn from(e: std::io::Error) -> Self { Io(e) }
}

impl From<(SourceId, chumsky::error::Simple<char>)> for Error {
    fn from((id, e): (SourceId, chumsky::error::Simple<char>)) -> Self {
        let span = SpanWithSource { id, span: e.span() };
        Error::Spanned(span, Lex(e))
    }
}

impl From<(SourceId, chumsky::error::Simple<lex::Token>)> for Error {
    fn from((id, e): (SourceId, chumsky::error::Simple<lex::Token>)) -> Self {
        let span = SpanWithSource { id, span: e.span() };
        Error::Spanned(span, Parse(e))
    }
}

impl Error {
    /// Produce a set of error reports for this [`Error`], which can then be printed.
    ///
    /// One report will be produced for each [`SingleError`] in the [`Error`].
    /// Each report for an [`Error::Spanned`] will annotate the substring
    /// which caused the error.
    ///
    /// To print the reports, you will need an appropriate [`ariadne::Cache`];
    /// use [`sources`](crate::sources).
    pub fn report(self) -> Vec<Report<SpanWithSource>> {
        use Error::*;
        match self {
            Single(id, error) => vec![report_single(id, None, error).finish()],
            Spanned(span, error) => {
                let SpanWithSource { id, span: s } = span.clone();
                vec![
                    report_single(id, Some(s), error)
                        .with_label(Label::new(span).with_message("here"))
                        .finish()
                ]
            }
            Multiple(errors) =>
                errors.into_iter()
                    .flat_map(|e| e.report())
                    .collect()
        }
    }

    /// Produce a `String` containing error messages for this [`Error`].
    ///
    /// To create an appropriate `cache`, use [`sources`](crate::sources).
    pub fn report_to_string(self, mut cache: impl ariadne::Cache<SourceId>) -> Result<String, std::io::Error> {
        let mut s = Vec::new();
        for report in self.report() {
            report.write(&mut cache, &mut s)?;
        }
        Ok(String::from_utf8_lossy(&s).to_string())
    }

    /// Return the first [`SingleError`] in this [`Error`], if it contains any, otherwise `None`.
    ///
    /// Can be used to present only one error in a set,
    /// or to get the only error in an [`Error`]
    /// that is known to only contain one.
    pub fn get_first_single_error(&self) -> Option<&SingleError> {
        use Error::*;
        match self {
            Single(_, error) => Some(error),
            Spanned(_, error) => Some(error),
            Multiple(errors) =>
                match errors.get(0) {
                    Some(e) => e.get_first_single_error(),
                    None => None,
                },
        }
    }
}

pub(crate) type RoughAddr = i32;

use SingleError::*;

/// An independent error without associated location data.
#[derive(Debug)]
pub enum SingleError {
    /// A `std::io::Error`.
    Io(std::io::Error),
    /// An error which occurred during lexing.
    ///
    /// Lexing attempts to be error-tolerant,
    /// successfully producing invalid tokens for invalid input,
    /// so this error indicates a bug in [`lex`](crate::lex).
    Lex(chumsky::error::Simple<char>),
    /// An error which occurred during parsing.
    ///
    /// Parsing attempts to be error-tolerant,
    /// successfully producing a syntax tree even for invalid input,
    /// so this error indicates a bug in [`parse`](crate::parse).
    Parse(chumsky::error::Simple<lex::Token>),
    /// An error which occurred during assembly.
    ///
    /// May indicate that the input was invalid or that
    /// there is a bug in [`assemble`](mod@crate::assemble).
    Assemble,
    /// An error which occurred during linking.
    ///
    /// May indicate that the inputs were invalid or that
    /// there is a bug in [`link`](crate::link).
    Link,
    /// An error which occurred during layering due to invalid input.
    Layer,

    /// More inputs were provided than could be assigned [`SourceId`](crate::SourceId)s.
    /// Should never occur in reasonable use cases.
    TooManyInputs,

    /// Source assumed to be a program block could not be parsed.
    BadProgramBlock,
    /// Source assumed to be an instruction could not be parsed.
    BadInstruction,
    /// Source assumed to be a label could not be parsed.
    BadLabel,
    /// Source assumed to be an opcode could not be parsed.
    BadOpcode,
    /// Source assumed to be a list of operands could not be parsed.
    BadOperands,
    /// Source assumed to be an operand could not be parsed.
    BadOperand,
    /// An operand list contained the wrong number of operands for an instruction.
    WrongNumberOfOperands {
        /// The correct number of operands for the instruction, given the opcode.
        expected: usize,
        /// The number of operands found in the operand list.
        actual: usize
    },
    /// The wrong type of operand was given for an instruction.
    OperandTypeMismatch {
        /// The correct type of operand for the instruction.
        expected: OperandType,
        /// The given operand's type.
        actual: OperandType
    },
    /// The same label was defined at two or more addresses.
    DuplicateLabel {
        /// The label.
        label: String,
        /// The set of occurrences of the label in the source code.
        occurrences: Vec<SpanWithSource>,
    },
    /// An instruction can't be assembled due to a label reference given as an operand.
    InvalidLabelReference {
        /// The label.
        label: String,
        /// The specific reason the instruction can't be assembled.
        reason: InvalidReferenceReason
    },
    /// A label does not follow the strict LC-3 requirements.
    StrictlyInvalidLabel {
        /// The label.
        label: String,
        /// The specific reason the label doesn't meet strict LC-3 requirements.
        reason: StrictlyInvalidLabelReason
    },
    /// Two program blocks span at least one common memory location.
    ProgramBlocksOverlap { placement1: ProgramBlockPlacement, placement2: ProgramBlockPlacement },
    /// The lexer produced no tokens; probably indicates no content in the source file.
    NoTokens,
    /// The lexer produced no token for `.ORIG`; this will likely result in no valid program blocks being parsed.
    NoOrig,
    /// The lexer produced no token for `.END`; this will likely result in no valid program blocks being parsed.
    NoEnd,
}

/// A reason that a label doesn't meet strict LC-3 requirements.
#[derive(Debug)]
pub enum StrictlyInvalidLabelReason {
    /// The label contains underscores.
    ContainsUnderscores,
    /// The label is over 20 characters.
    TooLong,
    /// The label is over 20 characters and contains underscores.
    ContainsUnderscoresAndTooLong,
}

/// A reason that an instruction cannot be assembled due to a label reference operand.
#[derive(Debug)]
pub enum InvalidReferenceReason {
    /// The label is not defined in the file.
    Undefined,
    /// The label is defined at more than one address in the file.
    Duplicated,
    /// The label is defined at an invalid address.
    OutOfBounds,
    /// The label is so far from the reference that the required offset would overflow the available bits.
    TooDistant { width: u8, est_ref_pos: RoughAddr, est_label_pos: RoughAddr, offset: SignedWord },
}

impl SingleError {
    pub(crate) fn program_blocks_overlap(p1: ProgramBlockPlacement, p2: ProgramBlockPlacement) -> Self {
        let (placement1, placement2) =
            if p1.span_in_memory.start <= p2.span_in_memory.start {
                (p1, p2)
            } else {
                (p2, p1)
            };
        ProgramBlocksOverlap { placement1, placement2 }
    }

    fn message(&self) -> String {
        match self {
            BadProgramBlock => String::from("invalid program block"),
            BadInstruction => String::from("invalid instruction"),
            BadLabel => String::from("invalid label"),
            BadOpcode => String::from("invalid opcode"),
            BadOperands => String::from("invalid operand list"),
            BadOperand => String::from("invalid operand"),
            WrongNumberOfOperands { expected, actual } =>
                format!("wrong number of operands; expected {}, found: {}", expected, actual),
            OperandTypeMismatch { expected, actual } =>
                format!("wrong operand type; expected {}, found: {}", expected, actual),
            DuplicateLabel { label, .. } =>
                format!("same label used for multiple locations: {}", label),
            InvalidLabelReference { label, reason } => {
                let reason_str = match reason {
                    InvalidReferenceReason::Undefined => "not previously defined".to_string(),
                    InvalidReferenceReason::Duplicated => "defined in multiple locations".to_string(),
                    InvalidReferenceReason::OutOfBounds => "defined at invalid address".to_string(),
                    InvalidReferenceReason::TooDistant { width, est_ref_pos, est_label_pos, offset  } =>
                        format!("label {} at {:#0label_pos_width$X} referenced at {:#0ref_pos_width$X}; too distant, cannot represent offset of {} in available bits: {}",
                                label, est_label_pos, est_ref_pos, offset, width,
                                // TODO: Rust '#X' formatter automatically fixes width to multiple of 4... find or implement workaround to control sign-extension; for example, for 9-bit signed offsets, we would want to display 0x2FF, not 0xFEFF. Showing as decimal for now.
                                label_pos_width = max(4, min_signed_hex_digits_required(*est_ref_pos) as usize),
                                ref_pos_width = max(4, min_signed_hex_digits_required(*est_label_pos) as usize),)
                };
                format!("reference to label {} invalid: {}", label, reason_str)
            }
            ProgramBlocksOverlap { placement1, placement2 } => {
                format!("program block {} in file occupying [{:#0o1s_width$X}, {:#0o1e_width$X}) overlaps program block {} occupying [{:#0o2s_width$X}, {:#0o2e_width$X})",
                    placement1.position_in_file,
                    placement1.span_in_memory.start,
                    placement1.span_in_memory.end,
                    placement2.position_in_file,
                    placement2.span_in_memory.start,
                    placement2.span_in_memory.end,
                    o1s_width = max(4, min_signed_hex_digits_required(placement1.span_in_memory.start) as usize),
                    o1e_width = max(4, min_signed_hex_digits_required(placement1.span_in_memory.end) as usize),
                    o2s_width = max(4, min_signed_hex_digits_required(placement2.span_in_memory.start) as usize),
                    o2e_width = max(4, min_signed_hex_digits_required(placement2.span_in_memory.end) as usize),
                )
            }
            NoTokens => "no LC-3 assembly in file".to_string(),
            NoOrig => "no .ORIG pseudo-op in file".to_string(),
            NoEnd => "no .END pseudo-op in file".to_string(),
            Io(ioe) => ioe.to_string(),
            Lex(le) => le.to_string(),
            Parse(pe) => pe.to_string(),
            Assemble => "unexpected assembly error".to_string(),
            Link => "unexpected link error".to_string(),
            Layer => "unexpected layering error".to_string(),
            TooManyInputs => "too many input files provided".to_string(),
            StrictlyInvalidLabel { label, reason } => {
                use StrictlyInvalidLabelReason::*;
                let reason_str = 
                    match reason {
                        ContainsUnderscores => "contains underscores",
                        TooLong => "over 20 characters long",
                        ContainsUnderscoresAndTooLong => "contains underscores and over 20 characters long"
                    };
                format!("label {} invalid: {}", label, reason_str)
            }
        }
    }
}

fn min_signed_hex_digits_required(n: i32) -> u8 {
    let bin_digits = util::min_signed_width(n);
    let extra = if bin_digits % 4 == 0 { 0 } else { 1 };
    bin_digits / 4 + extra
}


fn report_single(id: SourceId, span: Option<Span>, error: SingleError) -> ReportBuilder<SpanWithSource> {
    let mut r: ReportBuilder<SpanWithSource> =
        Report::build(ReportKind::Error, id, span.map(|s| s.start).unwrap_or(0))
        .with_message(error.message());
    match error {
        DuplicateLabel { occurrences, .. } => {
            let mut first_declaration_labeled = false;
            for occurrence in occurrences {
                let label_message = if !first_declaration_labeled {
                    first_declaration_labeled = true;
                    "first used here"
                } else {
                    "also used here"
                };
                r = r.with_label(Label::new(occurrence).with_message(label_message))
            }
        }
        ProgramBlocksOverlap { placement1, placement2 } => {
            let (first, first_pos_text, second, second_pos_text) =
                if placement1.position_in_file < placement2.position_in_file {
                    (placement1, "end", placement2, "start")
                } else {
                    (placement2, "start", placement1, "end")
                };
            r = r.with_label(Label::new(first.span_in_file)
                .with_message(format!("{} of this object overlaps the other", first_pos_text)))
                .with_label(Label::new(second.span_in_file)
                    .with_message(format!("{} of this object overlaps the other", second_pos_text)));
        }
        _ => {}
    }
    r
}


/// A type of operand, including number width constraints.
#[derive(Clone, Debug)]
pub enum OperandType {
    /// A register reference.
    Register,
    /// An unqualified number for use with `.BLKW`.
    UnqualifiedNumber,
    /// A number with a specific sign and width.
    Number { signed: bool, width: u8 },
    /// A string of characters.
    String,
    /// A label reference.
    Label,
    /// A type of operand that includes multiple other types.
    /// An operand of this type can be either of the contained types.
    ///
    /// Used for operands like PC offsets; the type of a PC offset
    /// is a label OR a signed number.
    Or(Box<OperandType>, Box<OperandType>)
}

impl Display for OperandType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use OperandType::*;

        match self {
            Register => write!(f, "Register"),
            UnqualifiedNumber => write!(f, "Unqualified Number"),
            Number { signed, width } => write!(f, "Number ({}-bit, {})", width, (if *signed { "signed" } else { "unsigned" })),
            String => write!(f, "String"),
            Label => write!(f, "Label"),
            Or(t1, t2) => write!(f, "{} or {}", t1, t2),
        }
    }
}

pub(crate) enum AcceptedNumberSigns {
    Signed,
    Unsigned,
    None,
    Any
}

impl AcceptedNumberSigns {
    pub(crate) fn or(&self, other: &Self) -> Self {
        use AcceptedNumberSigns::*;

        match (self, other) {
            (Unsigned, Signed)
            | (Signed, Unsigned)
            | (Any, _)
            | (_, Any)           => Any,
            (Signed, _)
            | (_, Signed)        => Signed,
            (Unsigned, _)
            | (_, Unsigned)      => Unsigned,
            (None, None)         => None
        }
    }
}

impl OperandType {

    pub(crate) fn accepted_number_signs(&self) -> AcceptedNumberSigns {
        use AcceptedNumberSigns::*;
        use OperandType::*;

        match self {
            Number { signed, .. } => if *signed { Signed } else { Unsigned },
            Or(t1, t2) => t1.accepted_number_signs().or(&t2.accepted_number_signs()),
            _ => None
        }
    }
    pub(crate) fn signed_or_unsigned_number(width: u8) -> Self {
        use OperandType::*;

        Or(Box::new(Number { signed: false, width }),
           Box::new(Number { signed: true,  width }))
    }

    pub(crate) fn reg_or_imm5() -> Self {
        use OperandType::*;

        Or(Box::new(Register), Box::new(Number { signed: true, width: 5 }))
    }

    pub(crate) fn pc_offset(width: u8) -> Self {
        use OperandType::*;

        Or(Box::new(Label), Box::new(Number { signed: true, width }))
    }

    pub(crate) fn check(&self, operand: &Operand) -> bool {
        use OperandType::*;

        match self {
            Register => matches!(operand, Operand::Register(_)),
            UnqualifiedNumber => matches!(operand, Operand::UnqualifiedNumberLiteral(_)),
            Number { signed: expected_signed, width: expected_width } => {
                if let Number { signed, width } = OperandType::of(operand) {
                    match (signed, expected_signed) {
                        (true, false) => {
                            if let Operand::NumberLiteral(LiteralValue::SignedWord(sw)) = operand {
                                *sw >= 0 && (width - 1) <= *expected_width
                            } else {
                                // TODO: find way to couple OperandType::of and value extraction to avoid this case
                                unreachable!("Detected operand as signed type but could not extract signed value");
                            }
                        }
                        (false, true) => width <= (expected_width - 1),
                        _ => width <= *expected_width
                    }

                } else {
                    false
                }
            }
            String => matches!(operand, Operand::StringLiteral(_)),
            Label => matches!(operand, Operand::Label(_)),
            Or(t1, t2)               => t1.check(operand) || t2.check(operand),
        }
    }

    pub(crate) fn of(operand: &Operand) -> Self {
        use OperandType::*;

        match operand {
            Operand::Register(_)                 => Register,
            Operand::UnqualifiedNumberLiteral(_) => UnqualifiedNumber,
            Operand::NumberLiteral(lv)           => OperandType::of_number_literal(lv, None),
            Operand::StringLiteral(_)            => String,
            Operand::Label(_)                    => Label,
        }
    }

    pub(crate) fn of_number_literal(literal_value: &LiteralValue, interpret_as: Option<AcceptedNumberSigns>) -> Self {
        use AcceptedNumberSigns::*;
        use OperandType::*;

        let value =
            match literal_value {
                LiteralValue::Word(value)       => *value as i32,
                LiteralValue::SignedWord(value) => *value as i32,
            };
        let unsigned_interpretation = Number { signed: false, width: util::min_unsigned_width(value) };
        let signed_interpretation   = Number { signed: true,  width: util::min_signed_width(value)   };
        match interpret_as {
            Option::None | Some(None) => match literal_value {
                LiteralValue::Word(_)       => unsigned_interpretation,
                LiteralValue::SignedWord(_) => signed_interpretation,
            }
            Some(Signed)   => signed_interpretation,
            Some(Unsigned) => unsigned_interpretation,
            Some(Any)      => Or(Box::new(signed_interpretation),
                                 Box::new(unsigned_interpretation)),
        }
    }
}

/// Data indicating the source string indices and memory addresses spanned by a program block.
#[derive(Clone, Debug)]
pub struct ProgramBlockPlacement {
    pub position_in_file: usize,
    pub span_in_file: SpanWithSource,
    pub span_in_memory: Range<RoughAddr>,
}

