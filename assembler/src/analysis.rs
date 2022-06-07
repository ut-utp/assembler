use std::cmp::{max, min};
use std::collections::{HashMap, HashSet};
use std::convert::{TryFrom, TryInto};
use std::fmt::{Debug, Display, format, Formatter};
use std::ops::Range;
use std::string::String;
use itertools::{concat, Itertools, zip};
use ariadne::{Label, Report, ReportBuilder, ReportKind};
use lc3_isa::{Addr, SignedWord, Word};
use crate::lexer::{LiteralValue, Opcode};
use crate::parser::{File, get, get_result, Instruction, Operand, Program, result, WithErrData};
use crate::{Span, Spanned};

type ErrorList = Vec<Spanned<Error>>;

use Error::*;
pub enum Error {
    BadProgram,
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
    ObjectsOverlap { placement1: ObjectPlacement, placement2: ObjectPlacement }
}

pub enum InvalidReferenceReason {
    Undefined,
    Duplicated,
    OutOfBounds,
}

impl Error {
    fn objects_overlap(p1: ObjectPlacement, p2: ObjectPlacement) -> Self {
        let (placement1, placement2) =
            if p1.span_in_memory.start <= p2.span_in_memory.start {
                (p1, p2)
            } else {
                (p2, p1)
            };
        ObjectsOverlap { placement1, placement2 }
    }

    fn message(&self) -> String {
        match self {
            BadProgram     => String::from("invalid program"),
            BadInstruction => String::from("invalid instruction"),
            BadLabel       => String::from("invalid label"),
            BadOpcode      => String::from("invalid opcode"),
            BadOperands    => String::from("invalid operand list"),
            BadOperand     => String::from("invalid operand"),
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
            ObjectsOverlap { placement1, placement2 } => {
                format!("object {} in file occupying [{:#0o1s_width$X}, {:#0o1e_width$X}) overlaps object {} occupying [{:#0o2s_width$X}, {:#0o2e_width$X})",
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
        }
    }
}

fn min_signed_hex_digits_required(n: i32) -> u8 {
    let bin_digits = min_signed_width(n);
    let extra = if bin_digits % 4 == 0 { 0 } else { 1 };
    bin_digits / 4 + extra
}


pub fn report(spanned_error: Spanned<Error>) -> Report {
    let (error, span) = spanned_error;
    let mut r =
        Report::build(ReportKind::Error, (), 0)
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
        ObjectsOverlap { placement1, placement2 } => {
            r = r.with_label(Label::new(placement1.span_in_file)
                    .with_message("end of this object overlaps the other"))
                 .with_label(Label::new(placement2.span_in_file)
                    .with_message("start of this object overlaps the other"));
        }
        _ => {
            r = r.with_label(Label::new(span).with_message("here"));
        }
    }
    r.finish()
}

use OperandType::*;
use crate::assembler::{calculate_offset, get_orig};

#[derive(Clone)]
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
        match self {
            Register                 => write!(f, "Register"),
            UnqualifiedNumber        => write!(f, "Unqualified Number"),
            Number { signed, width } => write!(f, "Number ({}-bit, {})", width, (if *signed { "signed" } else { "unsigned" })),
            String                   => write!(f, "String"),
            Label                    => write!(f, "Label"),
            Or(t1, t2)               => write!(f, "{} or {}", t1, t2),
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

        match self {
            Number { signed, .. } => if *signed { Signed } else { Unsigned },
            Or(t1, t2) => t1.accepted_number_signs().or(&t2.accepted_number_signs()),
            _ => None
        }
    }
    pub(crate) fn signed_or_unsigned_number(width: u8) -> Self {
        Or(Box::new(Number { signed: false, width }),
           Box::new(Number { signed: true,  width }))
    }

    pub(crate) fn reg_or_imm5() -> Self {
        Or(Box::new(Register), Box::new(Number { signed: true, width: 5 }))
    }

    pub(crate) fn pc_offset(width: u8) -> Self {
        Or(Box::new(Label), Box::new(Number { signed: true, width }))
    }

    pub(crate) fn check(&self, operand: &Operand) -> bool {
        match self {
            Register                 => matches!(operand, Operand::Register(_)),
            UnqualifiedNumber        => matches!(operand, Operand::UnqualifiedNumberLiteral(_)),
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
            String                   => matches!(operand, Operand::StringLiteral(_)),
            Label                    => matches!(operand, Operand::Label(_)),
            Or(t1, t2)               => t1.check(operand) || t2.check(operand),
        }
    }

    pub(crate) fn of(operand: &Operand) -> Self {
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

        let value =
            match literal_value {
                LiteralValue::Word(value)       => *value as i32,
                LiteralValue::SignedWord(value) => *value as i32,
            };
        let unsigned_interpretation = Number { signed: false, width: min_unsigned_width(value) };
        let signed_interpretation   = Number { signed: true,  width: min_signed_width(value)   };
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

fn min_signed_width(n: i32) -> u8 {
    let mut width = 1;
    const BASE: i32 = 2;
    while n < -BASE.pow(width - 1) || n >= BASE.pow(width - 1) {
        width += 1;
    }
    width as u8
}

fn min_unsigned_width(n: i32) -> u8 {
    let mut width = 1;
    const BASE: i32 = 2;
    while n >= BASE.pow(width) {
        width += 1;
    }
    width as u8
}


#[derive(Default)]
struct ParseErrorsAnalysis {
    errors: ErrorList
}

impl ParseErrorsAnalysis {
    fn new() -> Self {
        Default::default()
    }

    fn push_error(&mut self, error: Error, span: &Span) {
        self.errors.push((error, span.clone()));
    }
}

impl MutVisitor for ParseErrorsAnalysis {
    fn enter_program_error(&mut self, span: &Span) {
        self.push_error(BadProgram, span);
    }
    fn enter_orig_error(&mut self, span: &Span) {
        self.push_error(BadOperands, span);
    }
    fn enter_instruction_error(&mut self, span: &Span, _location: &LocationCounter) {
        self.push_error(BadInstruction, span);
    }
    fn enter_label_error(&mut self, span: &Span, _location: &LocationCounter) {
        self.push_error(BadLabel, span);
    }
    fn enter_opcode_error(&mut self, span: &Span, _location: &LocationCounter) {
        self.push_error(BadOpcode, span);
    }
    fn enter_operands_error(&mut self, span: &Span, _location: &LocationCounter) {
        self.push_error(BadOperands, span);
    }
    fn enter_operand_error(&mut self, span: &Span, _location: &LocationCounter) {
        self.push_error(BadOperand, span);
    }
}


#[derive(Default)]
struct DuplicateLabelsAnalysis {
    errors: ErrorList,
    labels: HashMap<String, Vec<Span>>,
}

impl DuplicateLabelsAnalysis {
    fn new() -> Self {
        Default::default()
    }
}

impl MutVisitor for DuplicateLabelsAnalysis {
    fn exit_file(&mut self, _file: &File) {
        let DuplicateLabelsAnalysis { errors, labels } = self;
        labels.iter()
            .filter(|(_, occurrences)| occurrences.len() > 1)
            .map(|(label, occurrences)|
                     (DuplicateLabel {
                         label: label.clone(),
                         occurrences: occurrences.clone()
                     }, 0..0) // TODO: dummy span, refactor so not required for errors with alternate span data
            )
            .for_each(|e| errors.push(e));
    }

    fn enter_label(&mut self, label: &String, span: &Span, _location: &LocationCounter) {
        let occurrences = self.labels.entry(label.clone()).or_insert(Vec::new());
        occurrences.push(span.clone());
    }
}


type RoughAddr = i32;

#[derive(Debug)]
enum InvalidSymbolError {
    InvalidOrig { estimated_addr: RoughAddr },
    PriorInvalidInstruction { estimated_addr: RoughAddr },
    Duplicated,
    OutOfBounds,
}

type SymbolTableValue = Result<Addr, InvalidSymbolError>;

enum AddressesOccupiedError {
    BadOpcode,
    BadOperand
}

impl Instruction {
    fn get_label(&self) -> Option<&String> {
        self.label.as_ref()
            .and_then(|res| get_result(res).as_ref().ok())
    }

    fn get_first_operand(&self) -> Option<&Operand> {
        get_result(&self.operands).as_ref().ok()
            .and_then(|ops| get(ops, 0))
    }

    fn addresses_occupied(&self) -> Result<Addr, AddressesOccupiedError> {
        match get_result(&self.opcode) {
            Err(()) => Err(AddressesOccupiedError::BadOpcode),
            Ok(oc) => match oc {
                Opcode::Stringz =>
                    self.get_first_operand()
                        .and_then(|op| op.clone().get_string())
                        .ok_or(AddressesOccupiedError::BadOperand)
                        .map(|s| (s.len() + 1) as Addr),
                Opcode::Blkw =>
                    self.get_first_operand()
                        .and_then(|op| op.clone().get_unqualified_number_value())
                        .ok_or(AddressesOccupiedError::BadOperand),
                _ => Ok(1)
            }
        }
    }
}

type SymbolTable = HashMap<String, SymbolTableValue>;

#[derive(Debug, Default)]
struct SymbolTableAnalysis {
    symbol_table: SymbolTable,
}

impl SymbolTableAnalysis {
    fn new() -> Self {
        Default::default()
    }
}

const ORIG_ERROR_STARTING_ADDRESS_ESTIMATE: RoughAddr = 0x3000;
const INSTRUCTION_ERROR_ADDRESSES_OCCUPIED_ESTIMATE: RoughAddr = 1;

impl MutVisitor for SymbolTableAnalysis {
    fn enter_label(&mut self, label: &String, _span: &Span, location: &LocationCounter) {
        self.symbol_table.entry(label.clone())
            .and_modify(|e| *e = Err(InvalidSymbolError::Duplicated))
            .or_insert(
                match location.state {
                    LocationCounterState::Valid =>
                        location.value.try_into()
                            .map_err(|_| InvalidSymbolError::OutOfBounds),
                    LocationCounterState::InvalidOrig =>
                        Err(InvalidSymbolError::InvalidOrig {
                            estimated_addr: location.value
                        }),
                    LocationCounterState::InvalidInstruction =>
                        Err(InvalidSymbolError::PriorInvalidInstruction {
                            estimated_addr: location.value
                        }),
                }
            );
    }
}


struct ExpectedLabel {
    width: u8,
    position: usize
}

struct LabelOffsetBoundsAnalysis<'a> {
    errors: ErrorList,
    symbol_table: &'a SymbolTable,
    expected_label: Option<ExpectedLabel>
}

impl<'a> LabelOffsetBoundsAnalysis<'a> {
    fn new(symbol_table: &'a SymbolTable) -> Self {
        Self {
            errors: Default::default(),
            symbol_table,
            expected_label: Default::default(),
        }
    }

    fn check_offset(&mut self, label: &String, span: &Span, width: u8, label_addr: RoughAddr, ref_addr: RoughAddr) {
        match calculate_offset(ref_addr, label_addr) {
            Err(_) => {
                // TODO: make more precise. This case shouldn't be possible unless one of the estimated addresses is far out of bounds.
                self.errors.push(
                    (InvalidLabelReference {
                        label: label.clone(),
                        reason: InvalidReferenceReason::OutOfBounds
                    }, span.clone()));
            }
            Ok(offset) => {
                if min_signed_width(offset as i32) > width {
                    self.errors.push(
                        (LabelTooDistant {
                            label: label.clone(),
                            width,
                            est_ref_pos: ref_addr,
                            offset,
                            est_label_pos: label_addr,
                        }, span.clone()))
                }
            }
        }
    }
}

impl<'a> MutVisitor for LabelOffsetBoundsAnalysis<'a> {
    fn enter_opcode_error(&mut self, _span: &Span, _location: &LocationCounter) {
        self.expected_label = None;
    }

    fn enter_opcode(&mut self, opcode: &Opcode, _span: &Span, _location: &LocationCounter) {
        use Opcode::*;
        self.expected_label =
            match opcode {
                Ld | Ldi | Lea
                | St | Sti     => Some(ExpectedLabel { width:  9, position: 1 }),
                Br(_)          => Some(ExpectedLabel { width:  9, position: 0 }),
                Jsr            => Some(ExpectedLabel { width: 11, position: 0 }),
                Fill           => Some(ExpectedLabel { width: 16, position: 0 }),
                _ => None,
            }
    }

    fn enter_operands(&mut self, operands: &Vec<WithErrData<Operand>>, _span: &Span, location: &LocationCounter) {
        if let Some(ExpectedLabel { width, position }) = &self.expected_label {
            if let Some((Ok(Operand::Label(label)), op_span)) = operands.get(*position) {
                match self.symbol_table.get(label) {
                    None => {
                        self.errors.push(
                            (InvalidLabelReference {
                                     label: label.clone(),
                                     reason: InvalidReferenceReason::Undefined
                                 }, op_span.clone()));
                    }
                    Some(stv) => match stv {
                        Ok(addr) => {
                            self.check_offset(label, op_span, *width, *addr as RoughAddr, location.value);
                        }
                        Err(ste) => match ste {
                            InvalidSymbolError::InvalidOrig { estimated_addr }
                            | InvalidSymbolError::PriorInvalidInstruction { estimated_addr } => {
                                self.check_offset(label, op_span, *width, *estimated_addr, location.value);
                            }
                            InvalidSymbolError::Duplicated => {
                                self.errors.push(
                                    (InvalidLabelReference {
                                        label: label.clone(),
                                        reason: InvalidReferenceReason::Duplicated
                                    }, op_span.clone()));
                            }
                            InvalidSymbolError::OutOfBounds => {
                                self.errors.push(
                                    (InvalidLabelReference {
                                        label: label.clone(),
                                        reason: InvalidReferenceReason::OutOfBounds
                                    }, op_span.clone()));
                            }
                        }
                    }
                }
            }
        }
    }

}


#[derive(Default)]
struct OperandTypesAnalysis {
    errors: ErrorList,
    expected_operands: Option<Vec<OperandType>>
}

impl OperandTypesAnalysis {
    fn new() -> Self {
        Default::default()
    }

    fn check_operands(&mut self, operands: &Vec<WithErrData<Operand>>, span: &Span) {
        if let Some(expected) = &self.expected_operands {
            // TODO: create longest common subsequence diff for more precise errors
            let ops_len = operands.len();
            let exp_len = expected.len();
            if ops_len != exp_len {
                self.errors.push((WrongNumberOfOperands { expected: exp_len, actual: ops_len }, span.clone()))
            } else {
                for ((op_res, op_span), exp_ty) in zip(operands, expected) {
                    if let Ok(op) = op_res {
                        if !exp_ty.check(op) {
                            let actual = if let Operand::NumberLiteral(value) = op {
                                OperandType::of_number_literal(value, Some(exp_ty.accepted_number_signs()))
                            } else {
                                OperandType::of(op)
                            };
                            self.errors.push((OperandTypeMismatch { expected: exp_ty.clone(), actual }, op_span.clone()));
                        }
                    }
                }
            }
        }
    }
}

fn orig_expected_operands() -> Vec<OperandType> {
    vec![OperandType::signed_or_unsigned_number(16)] // TODO: Disallow signed?
}

impl MutVisitor for OperandTypesAnalysis {
    fn enter_orig(&mut self, orig: &Vec<WithErrData<Operand>>, span: &Span, _location: &LocationCounter) {
        self.expected_operands = Some(orig_expected_operands());
        self.check_operands(orig, span);
    }

    fn enter_opcode_error(&mut self, _span: &Span, _location: &LocationCounter) {
        self.expected_operands = None;
    }

    fn enter_opcode(&mut self, opcode: &Opcode, _span: &Span, _location: &LocationCounter) {
        use Opcode::*;
        self.expected_operands = Some(
            match opcode {
                Add | And      => vec![Register, Register, OperandType::reg_or_imm5()],
                Br(_)          => vec![OperandType::pc_offset(9)],
                Jsr            => vec![OperandType::pc_offset(11)],
                Jmp | Jsrr     => vec![Register],
                Ld | Ldi | Lea
                | St | Sti     => vec![Register, OperandType::pc_offset(9)],
                Ldr | Str      => vec![Register, Register, Number { signed: true, width: 6 }],
                Not            => vec![Register, Register],
                Ret | Rti
                | Getc | Out
                | Puts | In
                | Putsp | Halt => vec![],
                Trap           => vec![OperandType::signed_or_unsigned_number(8)],
                Orig           => orig_expected_operands(),
                Blkw           => vec![UnqualifiedNumber],
                Fill           => vec![Or(Box::new(Label),
                                          Box::new(OperandType::signed_or_unsigned_number(16)))],
                Stringz        => vec![String],
            }
        );
    }

    fn enter_operands(&mut self, operands: &Vec<WithErrData<Operand>>, span: &Span, _location: &LocationCounter) {
        self.check_operands(operands, span);
    }
}


struct ObjectPlacementAnalysis {
    errors: ErrorList,
    last_start: RoughAddr,
    object_index: usize,
    object_spans: Vec<ObjectPlacement>,
}

#[derive(Clone)]
pub struct ObjectPlacement {
    position_in_file: usize,
    span_in_file: Span,
    span_in_memory: Range<RoughAddr>,
}

impl ObjectPlacementAnalysis {
    fn new() -> Self {
        Self {
            errors: Default::default(),
            last_start: ORIG_ERROR_STARTING_ADDRESS_ESTIMATE,
            object_index: 0,
            object_spans: Default::default(),
        }
    }
}

impl MutVisitor for ObjectPlacementAnalysis {
    fn exit_file(&mut self, _file: &File) {
        self.object_spans.sort_unstable_by_key(|span| span.span_in_memory.start);
        for (op1, op2) in self.object_spans.iter().tuple_windows() {
            if op2.span_in_memory.start < op1.span_in_memory.end {
                self.errors.push((
                    Error::objects_overlap(op1.clone(), op2.clone()),
                    0..0)); // TODO: refactor to avoid dummy span
            }
        }
    }

    fn exit_program(&mut self, _program: &Program, span: &Span, location: &LocationCounter) {
        self.object_spans.push(
            ObjectPlacement {
                position_in_file: self.object_index,
                span_in_file: span.clone(),
                span_in_memory: self.last_start..location.value
            });
        self.object_index += 1;
    }

    fn exit_orig(&mut self, _orig: &Vec<WithErrData<Operand>>, _span: &Span, location: &LocationCounter) {
        self.last_start = location.value;
    }
}


struct LocationCounter {
    value: RoughAddr,
    state: LocationCounterState,
}

impl LocationCounter {
    fn new() -> Self {
        Self {
            value: Default::default(),
            state: LocationCounterState::Valid,
        }
    }

}

#[derive(Debug)]
enum LocationCounterState {
    Valid,
    InvalidOrig,
    InvalidInstruction,
}

impl Default for LocationCounterState {
    fn default() -> Self {
        LocationCounterState::Valid
    }
}

impl LocationCounterState {
    fn if_valid_set(&mut self, state: LocationCounterState) {
        if let LocationCounterState::Valid = self {
            *self = state;
        }
    }
}

fn visit(v: &mut impl MutVisitor, file: &File) {
    v.enter_file(file);
    for program in file.programs.iter() {
        visit_program(v, program);
    }
    v.exit_file(file);
}

fn visit_program(v: &mut impl MutVisitor, program: &WithErrData<Program>) {
    let (program_res, span) = program;
    match program_res {
        Err(_) => { v.enter_program_error(span); }
        Ok(p) => {
            v.enter_program( p, span);

            let mut location_counter = LocationCounter::new();

            let Program { orig, instructions } = p;
            visit_orig(v, orig, &mut location_counter);
            for instruction in instructions {
                visit_instruction(v, instruction, &mut location_counter);
            }

            v.exit_program(p, span, &mut location_counter);
        }
    }
}

fn visit_orig(v: &mut impl MutVisitor, orig: &WithErrData<Vec<WithErrData<Operand>>>, location_counter: &mut LocationCounter) {
    let (orig_res, span) = orig;
    match orig_res {
        Err(_) => {
            location_counter.value = ORIG_ERROR_STARTING_ADDRESS_ESTIMATE;
            location_counter.state.if_valid_set(LocationCounterState::InvalidOrig);
            v.enter_orig_error(span);
        }
        Ok(o) => {
            location_counter.value = get(o, 0)
                .and_then(|op| Word::try_from(op.clone()).map(|w| w as RoughAddr).ok())
                .unwrap_or_else(| | {
                    location_counter.state.if_valid_set(LocationCounterState::InvalidOrig);
                    ORIG_ERROR_STARTING_ADDRESS_ESTIMATE
                });

            v.enter_orig( o, span, location_counter);
            for operand in o {
                visit_operand(v, operand, location_counter);
            }

            v.exit_orig(o, span, location_counter);
        }
    }
}

fn visit_instruction(v: &mut impl MutVisitor, instruction: &WithErrData<Instruction>, location_counter: &mut LocationCounter) {
    let (inst_res, span) = instruction;
    match inst_res {
        Err(_) => {
            v.enter_instruction_error(span, location_counter);
            location_counter.value += INSTRUCTION_ERROR_ADDRESSES_OCCUPIED_ESTIMATE;
            location_counter.state.if_valid_set(LocationCounterState::InvalidInstruction);
        }
        Ok(i) => {
            v.enter_instruction(i, span, location_counter);

            let Instruction { label, opcode, operands } = i;
            if let Some(l) = label {
                visit_label(v, l, location_counter);
            }
            visit_opcode(v, opcode, location_counter);
            visit_operands(v, operands, location_counter);

            v.exit_instruction(i, span, location_counter);

            location_counter.value += i.addresses_occupied()
                .unwrap_or_else(|_| {
                    location_counter.state.if_valid_set(LocationCounterState::InvalidInstruction);
                    INSTRUCTION_ERROR_ADDRESSES_OCCUPIED_ESTIMATE as Addr
                }) as RoughAddr;
        }
    }
}

fn visit_label(v: &mut impl MutVisitor, label: &WithErrData<String>, location_counter: &mut LocationCounter) {
    let (label_res, span) = label;
    match label_res {
        Err(_) => { v.enter_label_error(span, location_counter); }
        Ok(l) => { v.enter_label( l, span, location_counter); }
    }
}

fn visit_opcode(v: &mut impl MutVisitor, opcode: &WithErrData<Opcode>, location_counter: &mut LocationCounter) {
    let (opcode_res, span) = opcode;
    match opcode_res {
        Err(_) => { v.enter_opcode_error(span, location_counter); }
        Ok(oc) => { v.enter_opcode( oc, span, location_counter); }
    }
}

fn visit_operands(v: &mut impl MutVisitor, operands: &WithErrData<Vec<WithErrData<Operand>>>, location_counter: &mut LocationCounter) {
    let (ops_res, span) = operands;
    match ops_res {
        Err(_) => { v.enter_operands_error(span, location_counter); }
        Ok(o) => {
            v.enter_operands( o, span, location_counter);
            for operand in o {
                visit_operand(v, operand, location_counter);
            }
        }
    }
}

fn visit_operand(v: &mut impl MutVisitor, operand: &WithErrData<Operand>, location_counter: &mut LocationCounter) {
    let (op_res, span) = operand;
    match op_res {
        Err(_) => { v.enter_operand_error(span, location_counter); }
        Ok(o) => { v.enter_operand( o, span, location_counter); }
    }
}

trait MutVisitor {
    fn enter_file(&mut self, _file: &File) {}
    fn exit_file(&mut self, _file: &File) {}

    fn enter_program_error(&mut self, _span: &Span) {}
    fn enter_program(&mut self, _program: &Program, _span: &Span) {}
    fn exit_program(&mut self, _program: &Program, _span: &Span, _location: &LocationCounter) {}

    fn enter_orig_error(&mut self, _span: &Span) {}
    fn enter_orig(&mut self, _orig: &Vec<WithErrData<Operand>>, _span: &Span, _location: &LocationCounter) {}
    fn exit_orig(&mut self, _orig: &Vec<WithErrData<Operand>>, _span: &Span, _location: &LocationCounter) {}

    fn enter_instruction_error(&mut self, _span: &Span, _location: &LocationCounter) {}
    fn enter_instruction(&mut self, _instruction: &Instruction, _span: &Span, _location: &LocationCounter) {}
    fn exit_instruction(&mut self, _instruction: &Instruction, _span: &Span, _location: &LocationCounter) {}

    fn enter_label_error(&mut self, _span: &Span, _location: &LocationCounter) {}
    fn enter_label(&mut self, _label: &String, _span: &Span, _location: &LocationCounter) {}

    fn enter_opcode_error(&mut self, _span: &Span, _location: &LocationCounter) {}
    fn enter_opcode(&mut self, _opcode: &Opcode, _span: &Span, _location: &LocationCounter) {}

    fn enter_operands_error(&mut self, _span: &Span, _location: &LocationCounter) {}
    fn enter_operands(&mut self, _operands: &Vec<WithErrData<Operand>>, _span: &Span, _location: &LocationCounter) {}

    fn enter_operand_error(&mut self, _span: &Span, _location: &LocationCounter) {}
    fn enter_operand(&mut self, _operand: &Operand, _span: &Span, _location: &LocationCounter) {}
}

pub fn validate(file: &File) -> ErrorList {
    let mut pe = ParseErrorsAnalysis::new();
    visit(&mut pe, file);

    let mut dl = DuplicateLabelsAnalysis::new();
    visit(&mut dl, file);

    let mut ot = OperandTypesAnalysis::new();
    visit(&mut ot, file);

    let mut st = SymbolTableAnalysis::new();
    visit(&mut st, file);

    let mut lob = LabelOffsetBoundsAnalysis::new(&st.symbol_table);
    visit(&mut lob, file);

    let mut op = ObjectPlacementAnalysis::new();
    visit(&mut op, file);

    concat([
      pe.errors,
      dl.errors,
      ot.errors,
      lob.errors,
      op.errors,
    ])
}

