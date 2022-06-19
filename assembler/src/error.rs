use ariadne::{Label, Report, ReportBuilder, ReportKind};
use std::cmp::max;
use lc3_isa::SignedWord;
use std::fmt::{Display, Formatter};
use crate::{analyze, Span, util};
use crate::lex;
use crate::lex::LiteralValue;
use crate::parse::Operand;
use std::ops::Range;


impl From<std::io::Error> for SingleError {
    fn from(error: std::io::Error) -> Self {
        Io(error)
    }
}

impl<E> From<Vec<E>> for Error
    where E: Into<Error>
{
    fn from(errors: Vec<E>) -> Self {
        let es = errors.into_iter()
            .map(|e| e.into())
            .collect();
        Error::Multiple(es)
    }
}

impl<E> From<E> for Error
    where E: Into<SingleError>
{
    fn from(error: E) -> Self {
        Error::Single(error.into())
    }
}

impl From<chumsky::error::Simple<char>> for SingleError {
    fn from(error: chumsky::error::Simple<char>) -> Self {
        Lex(error)
    }
}

impl From<chumsky::error::Simple<lex::Token>> for SingleError {
    fn from(error: chumsky::error::Simple<lex::Token>) -> Self {
        Parse(error)
    }
}

#[derive(Debug)]
pub enum Error {
    Single(SingleError),
    Spanned(Span, SingleError),
    Multiple(Vec<Error>),
}

impl Error {
    pub fn report(self) -> Vec<Report> {
        use Error::*;
        match self {
            Single(error) => vec![report_single(error).finish()],
            Spanned(span, error) => vec![
                report_single(error)
                    .with_label(Label::new(span).with_message("here"))
                    .finish()
            ],
            Multiple(errors) =>
                errors.into_iter()
                    .flat_map(|e| e.report())
                    .collect()
        }
    }
}

pub(crate) type RoughAddr = i32;

use SingleError::*;

#[derive(Debug)]
pub enum SingleError {
    Io(std::io::Error),
    Lex(chumsky::error::Simple<char>),
    Parse(chumsky::error::Simple<lex::Token>),
    Assemble,
    Link,
    Layer,

    BadRegion,
    BadInstruction,
    BadLabel,
    BadOpcode,
    BadOperands,
    BadOperand,
    WrongNumberOfOperands { expected: usize, actual: usize },
    OperandTypeMismatch { expected: OperandType, actual: OperandType },
    DuplicateLabel { label: String, occurrences: Vec<Span>, },
    InvalidLabelReference { label: String, reason: InvalidReferenceReason },
    LabelTooDistant { label: String, width: u8, est_ref_pos: RoughAddr, est_label_pos: RoughAddr, offset: SignedWord },
    RegionsOverlap { placement1: RegionPlacement, placement2: RegionPlacement },
    NoTokens,
    NoOrig,
    NoEnd,
}

#[derive(Debug)]
pub enum InvalidReferenceReason {
    Undefined,
    Duplicated,
    OutOfBounds,
}

impl SingleError {
    pub(crate) fn regions_overlap(p1: RegionPlacement, p2: RegionPlacement) -> Self {
        let (placement1, placement2) =
            if p1.span_in_memory.start <= p2.span_in_memory.start {
                (p1, p2)
            } else {
                (p2, p1)
            };
        RegionsOverlap { placement1, placement2 }
    }

    fn message(&self) -> String {
        match self {
            BadRegion => String::from("invalid region"),
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
                    InvalidReferenceReason::Undefined => "not previously defined",
                    InvalidReferenceReason::Duplicated => "defined in multiple locations",
                    InvalidReferenceReason::OutOfBounds => "defined at invalid address",
                };
                format!("reference to label {} invalid: {}", label, reason_str)
            }
            LabelTooDistant { label, width, est_ref_pos, est_label_pos, offset } => {
                format!("label {} at {:#0label_pos_width$X} referenced at {:#0ref_pos_width$X}; too distant, cannot represent offset of {} in available bits: {}",
                        label, est_label_pos, est_ref_pos, offset, width,
                        // TODO: Rust '#X' formatter automatically fixes width to multiple of 4... find or implement workaround to control sign-extension; for example, for 9-bit signed offsets, we would want to display 0x2FF, not 0xFEFF. Showing as decimal for now.
                        label_pos_width = max(4, min_signed_hex_digits_required(*est_ref_pos) as usize),
                        ref_pos_width = max(4, min_signed_hex_digits_required(*est_label_pos) as usize),)
            }
            RegionsOverlap { placement1, placement2 } => {
                format!("region {} in file occupying [{:#0o1s_width$X}, {:#0o1e_width$X}) overlaps region {} occupying [{:#0o2s_width$X}, {:#0o2e_width$X})",
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
        }
    }
}

fn min_signed_hex_digits_required(n: i32) -> u8 {
    let bin_digits = util::min_signed_width(n);
    let extra = if bin_digits % 4 == 0 { 0 } else { 1 };
    bin_digits / 4 + extra
}


fn report_single(error: SingleError) -> ReportBuilder<Span> {
    let mut r = Report::build(ReportKind::Error, (), 0)
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
        RegionsOverlap { placement1, placement2 } => {
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


#[derive(Clone, Debug)]
pub enum OperandType {
    Register,
    UnqualifiedNumber,
    Number { signed: bool, width: u8 },
    String,
    Label,
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

#[derive(Clone, Debug)]
pub struct RegionPlacement {
    pub(crate) position_in_file: usize,
    pub(crate) span_in_file: Span,
    pub(crate) span_in_memory: Range<RoughAddr>,
}

