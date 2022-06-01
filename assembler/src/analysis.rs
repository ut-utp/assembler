use std::collections::{HashMap, HashSet};
use std::fmt::{Display, format, Formatter};
use std::string::String;
use itertools::{concat, zip};
use ariadne::{Label, Report, ReportBuilder, ReportKind};
use lc3_isa::{Addr, SignedWord, Word};
use crate::lexer::{LiteralValue, Opcode};
use crate::parser::{File, Instruction, Operand, Program, WithErrData};
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
}

impl Error {
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
                format!("same label used for multiple locations: {}", label)
        }
    }
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
        _ => {
            r = r.with_label(Label::new(span).with_message("here"));
        }
    }
    r.finish()
}

use OperandType::*;
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

enum InvalidSymbolReason {
    InvalidOrig,
    PriorInvalidInstruction { estimated_addr: Addr },
    Duplicated,
    OutOfBounds,
}

type SymbolTableValue = Result<Addr, InvalidSymbolReason>;


#[derive(Default)]
struct ParseErrors {
    errors: ErrorList
}

impl ParseErrors {
    fn new() -> Self {
        Default::default()
    }

    fn push_error(&mut self, error: Error, span: &Span) {
        self.errors.push((error, span.clone()));
    }
}

impl MutVisitor for ParseErrors {
    fn enter_program_error(&mut self, span: &Span) {
        self.push_error(BadProgram, span);
    }
    fn enter_orig_error(&mut self, span: &Span) {
        self.push_error(BadOperands, span);
    }
    fn enter_instruction_error(&mut self, span: &Span) {
        self.push_error(BadInstruction, span);
    }
    fn enter_label_error(&mut self, span: &Span) {
        self.push_error(BadLabel, span);
    }
    fn enter_opcode_error(&mut self, span: &Span) {
        self.push_error(BadOpcode, span);
    }
    fn enter_operands_error(&mut self, span: &Span) {
        self.push_error(BadOperands, span);
    }
    fn enter_operand_error(&mut self, span: &Span) {
        self.push_error(BadOperand, span);
    }
}


#[derive(Default)]
struct DuplicateLabels {
    errors: ErrorList,
    labels: HashMap<String, Vec<Span>>,
}

impl DuplicateLabels {
    fn new() -> Self {
        Default::default()
    }
}

impl MutVisitor for DuplicateLabels {
    fn exit_file(&mut self, _file: &File) {
        let DuplicateLabels { errors, labels } = self;
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

    fn enter_label(&mut self, label: &String, span: &Span) {
        let occurrences = self.labels.entry(label.clone()).or_insert(Vec::new());
        occurrences.push(span.clone());
    }
}


#[derive(Default)]
struct OperandTypes {
    errors: ErrorList,
    expected_operands: Option<Vec<OperandType>>
}

impl OperandTypes {
    fn new() -> Self {
        Default::default()
    }
}

fn orig_expected_operands() -> Vec<OperandType> {
    vec![OperandType::signed_or_unsigned_number(16)] // TODO: Disallow signed?
}

impl MutVisitor for OperandTypes {
    fn enter_orig(&mut self, orig: &Vec<WithErrData<Operand>>, span: &Span) {
        self.expected_operands = Some(orig_expected_operands());
        self.enter_operands(orig, span);
    }

    fn enter_opcode_error(&mut self, _span: &Span) {
        self.expected_operands = None;
    }

    fn enter_opcode(&mut self, opcode: &Opcode, _span: &Span) {
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

    fn enter_operands(&mut self, operands: &Vec<WithErrData<Operand>>, span: &Span) {
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

            let Program { orig, instructions } = p;
            visit_orig(v, orig);
            for instruction in instructions {
                visit_instruction(v, instruction);
            }
        }
    }
}

fn visit_orig(v: &mut impl MutVisitor, orig: &WithErrData<Vec<WithErrData<Operand>>>) {
    let (orig_res, span) = orig;
    match orig_res {
        Err(_) => { v.enter_orig_error(span); }
        Ok(o) => {
            v.enter_orig( o, span);
            for operand in o {
                visit_operand(v, operand);
            }
        }
    }
}

fn visit_instruction(v: &mut impl MutVisitor, instruction: &WithErrData<Instruction>) {
    let (inst_res, span) = instruction;
    match inst_res {
        Err(_) => { v.enter_instruction_error(span); }
        Ok(i) => {
            v.enter_instruction(i, span);

            let Instruction { label, opcode, operands } = i;
            if let Some(l) = label {
                visit_label(v, l);
            }
            visit_opcode(v, opcode);
            visit_operands(v, operands);
        }
    }
}

fn visit_label(v: &mut impl MutVisitor, label: &WithErrData<String>) {
    let (label_res, span) = label;
    match label_res {
        Err(_) => { v.enter_label_error(span); }
        Ok(l) => { v.enter_label( l, span); }
    }
}

fn visit_opcode(v: &mut impl MutVisitor, opcode: &WithErrData<Opcode>) {
    let (opcode_res, span) = opcode;
    match opcode_res {
        Err(_) => { v.enter_opcode_error(span); }
        Ok(oc) => { v.enter_opcode( oc, span); }
    }
}

fn visit_operands(v: &mut impl MutVisitor, operands: &WithErrData<Vec<WithErrData<Operand>>>) {
    let (ops_res, span) = operands;
    match ops_res {
        Err(_) => { v.enter_operands_error(span); }
        Ok(o) => {
            v.enter_operands( o, span);
            for operand in o {
                visit_operand(v, operand);
            }
        }
    }
}

fn visit_operand(v: &mut impl MutVisitor, operand: &WithErrData<Operand>) {
    let (op_res, span) = operand;
    match op_res {
        Err(_) => { v.enter_operand_error(span); }
        Ok(o) => { v.enter_operand( o, span); }
    }
}

trait MutVisitor {
    fn enter_file(&mut self, _file: &File) {}
    fn exit_file(&mut self, _file: &File) {}

    fn enter_program_error(&mut self, _span: &Span) {}
    fn enter_program(&mut self, _program: &Program, _span: &Span) {}

    fn enter_orig_error(&mut self, _span: &Span) {}
    fn enter_orig(&mut self, _orig: &Vec<WithErrData<Operand>>, _span: &Span) {}

    fn enter_instruction_error(&mut self, _span: &Span) {}
    fn enter_instruction(&mut self, _instruction: &Instruction, _span: &Span) {}

    fn enter_label_error(&mut self, _span: &Span) {}
    fn enter_label(&mut self, _label: &String, _span: &Span) {}

    fn enter_opcode_error(&mut self, _span: &Span) {}
    fn enter_opcode(&mut self, _opcode: &Opcode, _span: &Span) {}

    fn enter_operands_error(&mut self, _span: &Span) {}
    fn enter_operands(&mut self, _operands: &Vec<WithErrData<Operand>>, _span: &Span) {}

    fn enter_operand_error(&mut self, _span: &Span) {}
    fn enter_operand(&mut self, _operand: &Operand, _span: &Span) {}
}

pub fn validate(file: &File) -> ErrorList {
    let mut pe = ParseErrors::new();
    visit(&mut pe, file);

    let mut dl = DuplicateLabels::new();
    visit(&mut dl, file);

    let mut ot = OperandTypes::new();
    visit(&mut ot, file);

    concat([
      pe.errors,
      dl.errors,
      ot.errors
    ])
}

