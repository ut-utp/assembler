use std::collections::{HashMap, HashSet};
use std::fmt::{Display, format, Formatter};
use std::string::String;
use itertools::zip;
use ariadne::{Label, Report, ReportBuilder, ReportKind};
use lc3_isa::{SignedWord, Word};
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

use Analysis::*;
enum Analysis {
    OperandTypes { expected_operands: Option<Vec<OperandType>> },
    DuplicateLabels { labels: HashMap<String, Vec<Span>>, }
}

impl Analysis {
    fn operand_types() -> Self {
        OperandTypes { expected_operands: None }
    }

    fn duplicate_labels() -> Self {
        DuplicateLabels { labels: HashMap::new() }
    }

    fn visit_file(&mut self, errors: &mut ErrorList, file: &File) {
        match self {
            _ => {}
        }
    }

    fn visit_program(&mut self, errors: &mut ErrorList, program: &Program, span: &Span) {
        match self {
            _ => {}
        }
    }

    fn visit_instruction(&mut self, errors: &mut ErrorList, instruction: &Instruction, span: &Span) {
        match self {
            OperandTypes { expected_operands } => {
                use Opcode::*;
                *expected_operands = match &instruction.opcode.0 {
                    Err(_) => None,
                    Ok(oc) => Some(match oc {
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
                        Orig           => vec![OperandType::signed_or_unsigned_number(16)], // TODO: Disallow signed?
                        Blkw           => vec![UnqualifiedNumber],
                        Fill           => vec![Or(Box::new(Label),
                                                  Box::new(OperandType::signed_or_unsigned_number(16)))],
                        Stringz        => vec![String],
                    }),
                };
            }
            _ => {}
        }
    }

    fn visit_label(&mut self, errors: &mut ErrorList, label: &String, span: &Span) {
        match self {
            DuplicateLabels { labels } => {
                let occurrences = labels.entry(label.clone()).or_insert(Vec::new());
                occurrences.push(span.clone());
            }
            _ => {}
        }
    }

    fn visit_operands(&mut self, errors: &mut ErrorList, operands: &Vec<WithErrData<Operand>>, span: &Span) {
        match self {
            OperandTypes { expected_operands } => {
                if let Some(expected) = expected_operands {
                    // TODO: create longest common subsequence diff for more precise errors
                    let ops_len = operands.len();
                    let exp_len = expected.len();
                    if ops_len != exp_len {
                        errors.push((WrongNumberOfOperands { expected: exp_len, actual: ops_len }, span.clone()))
                    } else {
                        for ((op_res, op_span), exp_ty) in zip(operands, expected) {
                            if let Ok(op) = op_res {
                                if !exp_ty.check(op) {
                                    let actual = if let Operand::NumberLiteral(value) = op {
                                        OperandType::of_number_literal(value, Some(exp_ty.accepted_number_signs()))
                                    } else {
                                        OperandType::of(op)
                                    };
                                    errors.push((OperandTypeMismatch { expected: exp_ty.clone(), actual }, op_span.clone()));
                                }
                            }
                        }
                    }
                }
            }
            _ => {}
        }
    }

    fn visit_operand(&mut self, errors: &mut ErrorList, operand: &Operand, span: &Span) {
        match self {
            _ => {}
        }
    }

    fn exit_file(&mut self, errors: &mut ErrorList, file: &File) {
        match self {
            DuplicateLabels { labels } => {
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
            _ => {}
        }
    }

}

struct Analyzer {
    errors: ErrorList,
    analyses: [Analysis; 2],
}

impl Analyzer {
    fn new() -> Self {
        Self {
            errors: Vec::new(),
            analyses: [
                Analysis::operand_types(),
                Analysis::duplicate_labels()
            ]
        }
    }

    fn analyze(&mut self, file: &File) {
        for analysis in self.analyses.iter_mut() {
            analysis.visit_file(&mut self.errors, file);
        }
        for program in file {
            self.analyze_program(program);
        }
        for analysis in self.analyses.iter_mut() {
            analysis.exit_file(&mut self.errors, file);
        }
    }

    fn analyze_program(&mut self, program: &WithErrData<Program>) {
        let (program_res, program_span) = program;
        match program_res {
            Err(_) => { self.errors.push((BadProgram, program_span.clone())); }
            Ok(prog) => {
                for analysis in self.analyses.iter_mut() {
                    analysis.visit_program(&mut self.errors, prog, program_span);
                }

                let Program { orig, instructions } = prog;
                self.analyze_instruction(orig);
                for instruction in instructions {
                    self.analyze_instruction(instruction);
                }
            }
        }
    }

    fn analyze_instruction(&mut self, instruction: &WithErrData<Instruction>) {
        let (instruction_res, instruction_span) = instruction;
        match instruction_res {
            Err(_) => { self.errors.push((BadInstruction, instruction_span.clone())); },
            Ok(inst) => {
                for analysis in self.analyses.iter_mut() {
                    analysis.visit_instruction(&mut self.errors, inst, instruction_span);
                }

                let Instruction { label, opcode, operands } = inst;

                if let Some(l_wed) = label {
                    let (label_res, label_span) = l_wed;
                    match label_res {
                        Err(_) => { self.errors.push((BadLabel, label_span.clone())) },
                        Ok(l) => {
                            for analysis in self.analyses.iter_mut() {
                                analysis.visit_label(&mut self.errors, l, label_span);
                            }
                        }
                    }
                }

                let (oc_res, opcode_span) = opcode;
                if let Err(_) = oc_res {
                    self.errors.push((BadOpcode, opcode_span.clone()));
                }

                self.analyze_operands(operands);
            }
        }
    }

    fn analyze_operands(&mut self, operands: &WithErrData<Vec<WithErrData<Operand>>>) {
        let (operands_res, operands_span) = operands;
        match operands_res {
            Err(_) => { self.errors.push((BadOperands, operands_span.clone())); }
            Ok(ops) => {
                for analysis in self.analyses.iter_mut() {
                    analysis.visit_operands(&mut self.errors, ops, operands_span);
                }
            }
        }
    }

    fn analyze_operand(&mut self, operand: &WithErrData<Operand>) {
        let (operand_res, operand_span) = operand;
        match operand_res {
            Err(_) => { self.errors.push((BadOperand, operand_span.clone())); }
            Ok(op) => {
                for analysis in self.analyses.iter_mut() {
                    analysis.visit_operand(&mut self.errors, op, operand_span);
                }
            }
        }
    }
}

pub fn validate(file: &File) -> ErrorList {
    let mut analyzer = Analyzer::new();
    analyzer.analyze(file);
    analyzer.errors
}

