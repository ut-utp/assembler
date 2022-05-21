use std::ops::Range;
use itertools::zip;
use crate::lexer::Opcode;
use crate::parser::{File, Instruction, Operand, Program, WithErrData};
use crate::Spanned;

type ErrorList = Vec<Spanned<Error>>;

use Error::*;
enum Error {
    BadProgram,
    BadInstruction,
    BadLabel,
    BadOpcode,
    BadOperands,
    BadOperand,
    WrongNumberOfOperands { expected: usize, actual: usize },
    OperandTypeMismatch { expected: OperandType, actual: OperandType }
}

use OperandType::*;
enum OperandType {
    Register,
    UnqualifiedNumber,
    Number,
    String,
    Label,
    Or(Box<OperandType>, Box<OperandType>)
}

impl OperandType {
    pub(crate) fn reg_or_imm() -> Self {
        Or(Box::new(Register), Box::new(Number))
    }

    pub(crate) fn pc_offset() -> Self {
        Or(Box::new(Label), Box::new(Number))
    }

    pub(crate) fn check(&self, operand: &Operand) -> bool {
        match self {
            Register          => matches!(operand, Operand::Register(_)),
            UnqualifiedNumber => matches!(operand, Operand::UnqualifiedNumberLiteral(_)),
            Number            => matches!(operand, Operand::NumberLiteral(_)),
            String            => matches!(operand, Operand::StringLiteral(_)),
            Label             => matches!(operand, Operand::Label(_)),
            Or(t1, t2)        => t1.check(operand) || t2.check(operand),
        }
    }

    pub(crate) fn of(operand: &Operand) -> Self {
        match operand {
            Operand::Register(_)                 => Register,
            Operand::UnqualifiedNumberLiteral(_) => UnqualifiedNumber,
            Operand::NumberLiteral(_)            => Number,
            Operand::StringLiteral(_)            => String,
            Operand::Label(_)                    => Label,
        }
    }
}

fn check_result_then<T>(errors: &mut ErrorList, wed: &WithErrData<T>, error: Error, f: impl FnOnce(&mut ErrorList, &T, &Range<usize>)) {
    let (res, span) = wed;
    match res {
        Err(_) => { errors.push((error, span.clone())); }
        Ok(v) => { f(errors, v, span); }
    }
}

fn validate(file: &File) -> ErrorList {
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
                Add | And      => vec![Register, Register, OperandType::reg_or_imm()],
                Br(_) | Jsr    => vec![OperandType::pc_offset()],
                Jmp | Jsrr     => vec![Register],
                Ld | Ldi | Lea
                | St | Sti     => vec![Register, OperandType::pc_offset()],
                Ldr | Str      => vec![Register, Register, Number],
                Not            => vec![Register, Register],
                Ret | Rti
                | Getc | Out
                | Puts | In
                | Putsp | Halt
                | End          => vec![],
                Trap
                | Orig | Blkw  => vec![Number],
                Fill           => vec![Or(Box::new(Label), Box::new(Number))],
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
                                es.push((OperandTypeMismatch { expected: exp_ty, actual: OperandType::of(op) }, op_span.clone()));
                            }
                        }
                    }
                }
            }
        }
    });
}