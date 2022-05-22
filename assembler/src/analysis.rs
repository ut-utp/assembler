use std::fmt::{Display, format, Formatter};
use std::ops::Range;
use std::string::String;
use itertools::zip;
use ariadne::{Label, Report, ReportBuilder, ReportKind};
use lc3_isa::{SignedWord, Word};
use crate::lexer::{LiteralValue, Opcode};
use crate::parser::{File, Instruction, Operand, Program, WithErrData};
use crate::Spanned;

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
    OperandTypeMismatch { expected: OperandType, actual: OperandType }
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
        }
    }
}

pub fn report(spanned_error: Spanned<Error>) -> Report {
    let (error, span) = spanned_error;
    Report::build(ReportKind::Error, (), 0)
        .with_message(error.message())
        .with_label(Label::new(span).with_message("here"))
        .finish()
}

use OperandType::*;
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

fn check_result_then<T>(errors: &mut ErrorList, wed: &WithErrData<T>, error: Error, f: impl FnOnce(&mut ErrorList, &T, &Range<usize>)) {
    let (res, span) = wed;
    match res {
        Err(_) => { errors.push((error, span.clone())); }
        Ok(v) => { f(errors, v, span); }
    }
}

pub fn validate(file: &File) -> ErrorList {
    let mut errors = Vec::new();
    for program in file {
        validate_program(&mut errors, program);
    }
    errors
}

fn validate_program(errors: &mut ErrorList, program: &WithErrData<Program>) {
    check_result_then(errors, program, BadProgram, |es, prog, _| {
        let Program { orig, instructions, end } = prog;
        validate_instruction(es, orig);
        for instruction in instructions {
            validate_instruction(es, instruction);
        }
        validate_instruction(es, end);
    });
}

fn validate_instruction(errors: &mut ErrorList, instruction: &WithErrData<Instruction>) {
    check_result_then(errors, instruction, BadInstruction, |es, inst, _| {
        let Instruction { label, opcode, operands } = inst;

        if let Some((Err(_), label_span)) = label {
            es.push((BadLabel, label_span.clone()));
        }

        let (oc_res, opcode_span) = opcode;
        if let Err(_) = oc_res {
            es.push((BadOpcode, opcode_span.clone()));
        }

        use Opcode::*;
        let expected_operands = match oc_res {
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
                | Putsp | Halt
                | End          => vec![],
                Trap           => vec![OperandType::signed_or_unsigned_number(8)],
                Orig           => vec![OperandType::signed_or_unsigned_number(16)], // TODO: Disallow signed?
                Blkw           => vec![UnqualifiedNumber],
                Fill           => vec![Or(Box::new(Label),
                                          Box::new(OperandType::signed_or_unsigned_number(16)))],
                Stringz        => vec![String],
            }),
        };
        validate_operands(es, operands, expected_operands);
    });
}

fn validate_operands(errors: &mut ErrorList, operands: &WithErrData<Vec<WithErrData<Operand>>>, expected_types: Option<Vec<OperandType>>) {
    check_result_then(errors, operands, BadOperands, |es, ops, ops_span| {
        if let Some(expected) = expected_types {
            // TODO: create longest common subsequence diff for more precise errors
            let ops_len = ops.len();
            let exp_len = expected.len();
            if ops_len != exp_len {
                es.push((WrongNumberOfOperands { expected: exp_len, actual: ops_len }, ops_span.clone()))
            } else {
                for ((op_res, op_span), exp_ty) in zip(ops, expected) {
                    match op_res {
                        Err(_) => { es.push((BadOperand, op_span.clone())) }
                        Ok(op) => {
                            if !exp_ty.check(op) {
                                let actual = if let Operand::NumberLiteral(value) = op {
                                    OperandType::of_number_literal(value, Some(exp_ty.accepted_number_signs()))
                                } else {
                                    OperandType::of(op)
                                };
                                es.push((OperandTypeMismatch { expected: exp_ty, actual }, op_span.clone()));
                            }
                        }
                    }
                }
            }
        }
    });
}