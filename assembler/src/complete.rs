use lc3_isa::{Addr, Word, Instruction, SignedWord};
use crate::lexer::{Token, Span};
use crate::analysis::symbol_table;
use crate::analysis::symbol_table::{SymbolTableError, build_symbol_table};
use crate::error::ParseError;
use crate::ir::ir4_parse_ambiguous_tokens::{UnsignedImmOrLabel, ImmOrLabel, Sr2OrImm5};
use crate::ir::{ir2_parse_line_syntax, ir4_parse_ambiguous_tokens, ir5_expand_pseudo_ops,};
use std::collections::HashMap;

/// `complete` will store as much data as possible
/// relating to the source *and* what it will be assembled to.
/// This will allow querying for the source assembled to a memory location,
/// the addresses corresponding to labels, and whatever is required in the future
/// to provide a nice development environment.

pub type Label<'input> = ir5_expand_pseudo_ops::Label<'input>;
pub type Immediate<'input, Addr> = ir5_expand_pseudo_ops::Immediate<'input, Addr>;
pub type SymbolTable<'input> = Result<symbol_table::SymbolTable<'input>, Vec<SymbolTableError>>;

pub struct Program<'input> {
    pub objects: Vec<Object<'input>>
}

pub struct Object<'input> {
    pub origin_src: ir5_expand_pseudo_ops::Operation<'input>,
    pub origin: Immediate<'input, Addr>,
    pub content: ObjectContent<'input>,
    pub symbol_table: SymbolTable<'input>,
}

pub struct ObjectContent<'input> {
    pub operations: Vec<Operation<'input>>,
    pub empty_lines: Vec<ir2_parse_line_syntax::Line<'input>>,
    pub hanging_labels: Vec<ir2_parse_line_syntax::Line<'input>>,
    pub invalid_lines: Vec<ir2_parse_line_syntax::Line<'input>>,
}

pub struct Operation<'input> {
    pub label: Option<Label<'input>>,
    pub operator: Token<'input>,
    pub nzp: Result<Option<ConditionCodes>, ParseError>,
    pub operands: Operands<'input>,

    pub src_lines: Vec<String>,
    pub separators: Vec<Separator<'input>>,
    pub whitespace: Vec<Token<'input>>,
    pub comments: Vec<Token<'input>>,
    pub newlines: Vec<Token<'input>>,

    pub instruction_or_values: Result<InstructionOrValues, Vec<ConstructInstructionError>>,
}

pub enum InstructionOrValues {
    Instruction(Instruction, Word),
    Values(Vec<Word>),
}

impl<'input> Operation<'input> {

    pub fn num_memory_locations_occupied(&self) -> Result<usize, Vec<ConstructInstructionError>> {
        use InstructionOrValues::*;
        match &self.instruction_or_values {
            Ok(Instruction(_, _)) => Ok(1),
            Ok(Values(values)) => Ok(values.len()),
            Err(errors) => Err(errors.clone())
        }
    }

}

pub type Operands<'input> = ir5_expand_pseudo_ops::Operands<'input>;
pub type ConditionCodes = ir5_expand_pseudo_ops::ConditionCodes;
pub type Separator<'input> = ir5_expand_pseudo_ops::Separator<'input>;

pub fn construct_all_instructions(file: ir5_expand_pseudo_ops::File) -> Program {
    file.into_iter()
        .map(construct_instructions)
        .collect()
}

pub fn construct_instructions(object: ir5_expand_pseudo_ops::Object) -> Object {
    let symbol_table = build_symbol_table(&object);
    let ir5_expand_pseudo_ops::Object { origin_src, origin, content } = object;
    let content = construct_object_content_instructions(content, &origin, &symbol_table);
    Object { origin_src, origin, content, symbol_table }
}

fn construct_object_content_instructions<'input>(content: ir5_expand_pseudo_ops::ObjectContent<'input>, origin: &Immediate<'input, Addr>, symbol_table: &SymbolTable<'input>) -> ObjectContent<'input> {
    let ir5_expand_pseudo_ops::ObjectContent { operations, empty_lines, hanging_labels, invalid_lines } = content;
    let operations = construct_operations_instructions(operations, origin, symbol_table);
    ObjectContent { operations, empty_lines, hanging_labels, invalid_lines, }
}

fn construct_operations_instructions<'input, O>(operations: O, origin: &Immediate<'input, Addr>, symbol_table: &SymbolTable<'input>) -> Vec<Operation<'input>>
    where O: IntoIterator<Item=ir5_expand_pseudo_ops::Operation<'input>>
{
    use ConstructInstructionError::*;

    let mut operations = Vec::new();
    let mut current_location = match &origin.value {
        Err(error) => Err(ConstructInstructionError { error }),
        Ok(origin) => Ok(origin),
    };
    for operation in operations {
        let operation = construct_instruction_for_operation(operation, , symbol_table);
        operations.push(operation);
    }

    operations
};

pub enum ConstructInstructionError {
    EarlierParseError {
        error: ParseError
    },
    InvalidLabel {
        span: Span,
        label: String,
    },
}

fn construct_instruction_for_operation<'input>(
    operation: ir5_expand_pseudo_ops::Operation<'input>,
    location: Result<Addr, Vec<ConstructInstructionError>>,
    symbol_table: &SymbolTable<'input>
) -> Operation<'input> {

    use ConstructInstructionError::*;

    let ir5_expand_pseudo_ops::Operation { label, operator, nzp, operands, src_lines, separators, whitespace, comments, newlines, expanded } = operation;

    let instruction_or_values = match location {
        Err(errors) => Err(errors),
        Ok(current_location) => {
            let mut errors = Vec::new();
            match &operands {
                Operands::Fill { value } => {
                    let value = match &value.value {
                        Err(error) => { errors.push(EarlierParseError { error: error.clone() }); Err(errors) }
                        Ok(UnsignedImmOrLabel::Imm(immediate)) => match &immediate.value {
                            Err(error) => { errors.push(EarlierParseError { error: error.clone() }); Err(errors) }
                            Ok(immediate) => Ok(immediate),
                        },
                        Ok(UnsignedImmOrLabel::Label(label)) => match &label.value {
                            Err(error) => { errors.push(EarlierParseError { error: error.clone() }); Err(errors) }
                            Ok(label_value) => {
                                match symbol_table.get(label_value) {
                                    Some(addr) => Ok(addr),
                                    None => {
                                        errors.push(InvalidLabel {
                                            span: label.src.span,
                                            label: label_value.to_string(),
                                        });
                                        Err(errors)
                                    }
                                }
                            },
                        },
                    };
                    value.map(|value| InstructionOrValues::Values(vec![value]))
                },
                Operands::Add { dr, sr1, sr2_or_imm5, } => match sr2_or_imm5.value {
                    Err(error) => { errors.push(EarlierParseError { error: error.clone() }); Err(errors) }
                    Ok(Sr2OrImm5::Imm5(immediate)) => {
                        Instruction::new_add_imm(dr.unwrap(), sr1.unwrap(), immediate.unwrap()),
                    },
                    Ok(Sr2OrImm5::Sr2(src_reg))    => {
                        Instruction::new_add_reg(dr.unwrap(), sr1.unwrap(), src_reg.unwrap()),
                    }
                },
                Operands::And { dr, sr1, sr2_or_imm5, } => match sr2_or_imm5.unwrap() {
                    Sr2OrImm5::Imm5(immediate) => Instruction::new_and_imm(dr.unwrap(), sr1.unwrap(), immediate.unwrap()),
                    Sr2OrImm5::Sr2(src_reg)    => Instruction::new_and_reg(dr.unwrap(), sr1.unwrap(), src_reg.unwrap()),
                },

                Operands::Ld { dr, pc_offset9 } => Instruction::new_ld(dr.unwrap(), compute_offset(pc_offset9, current_location, &symbol_table)),
                Operands::Ldi { dr, pc_offset9 } => Instruction::new_ldi(dr.unwrap(), compute_offset(pc_offset9, current_location, &symbol_table)),
                Operands::Ldr { dr, base, offset6 } => Instruction::new_ldr(dr.unwrap(), base.unwrap(), offset6.unwrap()),
                Operands::Lea { dr, pc_offset9 } => Instruction::new_lea(dr.unwrap(), compute_offset(pc_offset9, current_location, &symbol_table)),

                Operands::St { sr, pc_offset9 } => Instruction::new_st(sr.unwrap(), compute_offset(pc_offset9, current_location, &symbol_table)),
                Operands::Sti { sr, pc_offset9 } => Instruction::new_sti(sr.unwrap(), compute_offset(pc_offset9, current_location, &symbol_table)),
                Operands::Str { sr, base, offset6 } => Instruction::new_str(sr.unwrap(), base.unwrap(), offset6.unwrap()),

                Operands::Not { dr, sr } => Instruction::new_not(dr.unwrap(), sr.unwrap()),

                Operands::Br { pc_offset9, .. } => {
                    let nzp = nzp.unwrap();
                    Instruction::new_br(nzp.n, nzp.z, nzp.p, compute_offset(pc_offset9, current_location, &symbol_table))
                }

                Operands::Jmp { base } => Instruction::new_jmp(base.unwrap()),
                Operands::Jsr { pc_offset11 } => Instruction::new_jsr(compute_offset(pc_offset11, current_location, &symbol_table)),
                Operands::Jsrr { base } => Instruction::new_jsrr(base.unwrap()),

                Operands::Ret => Instruction::new_ret(),
                Operands::Rti => Instruction::new_rti(),

                Operands::Trap { trap_vec } => Instruction::new_trap(trap_vec.unwrap()),
                Operands::Getc  => Instruction::new_trap(0x20),
                Operands::Out   => Instruction::new_trap(0x21),
                Operands::Puts  => Instruction::new_trap(0x22),
                Operands::In    => Instruction::new_trap(0x23),
                Operands::Putsp => Instruction::new_trap(0x24),
                Operands::Halt  => Instruction::new_trap(0x25),

                Operands::Orig => {

                }
                Operands::Stringz { .. }
                | Operands::Blkw { .. }
                | Operands::End { .. } => {
                    match expanded.unwrap() {
                        Err(error) => { errors.push(EarlierParseError { error: error.clone() }); Err(errors) }
                        Ok(values) => { Ok(InstructionOrValues::Values(values)) }
                    }
                }
            }
        }
    };

    Operation { label, operator, nzp, operands, src_lines, separators, whitespace, comments, newlines, instruction_or_values, }
}

fn compute_offset(pc_offset: ir5_expand_pseudo_ops::Checked<ImmOrLabel>, location: Addr, symbol_table: &HashMap<&str, Addr>) -> SignedWord {
    use ImmOrLabel::*;
    match pc_offset.unwrap() {
        Label(label) => {
            let label = label.unwrap();
            let label_location = symbol_table.get(label).unwrap().clone();
            let label_location = label_location as i64;
            let offset_base = (location + 1) as i64;
            (label_location - offset_base) as SignedWord
        }
        Imm(immediate) => immediate.value.unwrap()
    }
}
