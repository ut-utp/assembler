use lc3_isa::{Addr, Word, Instruction, SignedWord};
use crate::lexer::{Token, Span};
use crate::analysis::symbol_table;
use crate::analysis::symbol_table::{SymbolTableError, build_symbol_table};
use crate::error::ParseError;
use crate::ir::ir4_parse_ambiguous_tokens::{UnsignedImmOrLabel, ImmOrLabel, Sr2OrImm5, Checked};
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
    pub objects: Vec<Object<'input>>,
    pub ignored: Vec<ir2_parse_line_syntax::Line<'input>>,
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

    pub instruction_or_values: Result<InstructionOrValues, ConstructInstructionError>,
}

pub enum InstructionOrValues {
    Instruction(Instruction, Word),
    Values(Vec<Word>),
}

impl<'input> Operation<'input> {

    pub fn num_memory_locations_occupied(&self) -> Result<usize, ConstructInstructionError> {
        use InstructionOrValues::*;
        match &self.instruction_or_values {
            Ok(Instruction(_, _)) => Ok(1),
            Ok(Values(values)) => Ok(values.len()),
            Err(error) => Err(error.clone())
        }
    }

}

pub type Operands<'input> = ir5_expand_pseudo_ops::Operands<'input>;
pub type ConditionCodes = ir5_expand_pseudo_ops::ConditionCodes;
pub type Separator<'input> = ir5_expand_pseudo_ops::Separator<'input>;

#[derive(Clone)]
pub enum ConstructInstructionError {
    EarlierParseError {
        error: ParseError,
    },
    SymbolTableInvalid {
        errors: Vec<SymbolTableError>,
    },
    InvalidLabel {
        span: Span,
        label: String,
    },
}

pub fn construct_all_instructions(file: ir5_expand_pseudo_ops::File) -> Program {
    let ir5_expand_pseudo_ops::File { objects, ignored } = file;
    let objects = objects.into_iter()
        .map(construct_instructions)
        .collect();
    Program { objects, ignored }
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

    let mut current_location = match &origin.value {
        Err(error) => Err(error.into()),
        Ok(origin) => Ok(*origin),
    };
    let mut new_operations = Vec::new();
    for operation in operations {
        let increment = operation.num_memory_locations_occupied();
        let new_operation = construct_instruction_for_operation(operation, &current_location, symbol_table);
        new_operations.push(new_operation);
        current_location = match (current_location, increment) {
            (Ok(location), Ok(amount)) => Ok(location + amount as u16),
            (Ok(_), Err(error)) => Err(error.into()),
            (error, _) => error,
        };
    }

    new_operations
}


fn construct_instruction_for_operation<'input>(
    operation: ir5_expand_pseudo_ops::Operation<'input>,
    location: &Result<Addr, ConstructInstructionError>,
    symbol_table: &SymbolTable<'input>
) -> Operation<'input> {
    let ir5_expand_pseudo_ops::Operation { label, operator, nzp, operands, src_lines, separators, whitespace, comments, newlines, expanded } = operation;
    let instruction_or_values = construct_instruction_or_values(location, symbol_table, &nzp, operands.clone(), &expanded);
    Operation { label, operator, nzp, operands, src_lines, separators, whitespace, comments, newlines, instruction_or_values, }
}

impl From<ParseError> for ConstructInstructionError {
    fn from(error: ParseError) -> Self {
        ConstructInstructionError::EarlierParseError {
            error
        }
    }
}
impl From<&ParseError> for ConstructInstructionError {
    fn from(error: &ParseError) -> Self {
        ConstructInstructionError::EarlierParseError {
            error: error.clone()
        }
    }
}

fn construct_instruction_or_values(
    location: &Result<Addr, ConstructInstructionError>,
    symbol_table: &SymbolTable,
    nzp: &Result<Option<ConditionCodes>, ParseError>,
    operands: ir5_expand_pseudo_ops::Operands,
    expanded: &Option<Result<Vec<Word>, ParseError>>,
) -> Result<InstructionOrValues, ConstructInstructionError> {
    use ConstructInstructionError::*;

    let location = location.clone()?;
    match operands {
        Operands::Fill { value } => {
            let value = value.value?; // TODO: lol
            let value = match value {
                UnsignedImmOrLabel::Imm(immediate) => {
                    let immediate = immediate.value?;
                    Ok(immediate)
                },
                UnsignedImmOrLabel::Label(label) => {
                    let label_value = &label.value?;
                    if let Err(errors) = symbol_table {
                        return Err(ConstructInstructionError::SymbolTableInvalid { errors: errors.clone() })
                    }
                    let symbol_table = symbol_table.as_ref().unwrap();
                    match symbol_table.get(label_value) {
                        None => {
                            Err(InvalidLabel {
                                span: label.src.span,
                                label: label_value.to_string(),
                            })
                        },
                        Some(addr) => Ok(*addr),
                    }
                },
            };
            value.map(|value| InstructionOrValues::Values(vec![value]))
        },
        Operands::Add { dr, sr1, sr2_or_imm5, } => match sr2_or_imm5.value? {
            Sr2OrImm5::Imm5(immediate) => { Ok(Instruction::new_add_imm(dr.value?, sr1.value?, immediate.value?,).into()) },
            Sr2OrImm5::Sr2(src_reg) => { Ok(Instruction::new_add_reg(dr.value?, sr1.value?, src_reg.value?,).into()) }
        },
        Operands::And { dr, sr1, sr2_or_imm5, } => match sr2_or_imm5.value? {
            Sr2OrImm5::Imm5(immediate) => { Ok(Instruction::new_and_imm(dr.value?, sr1.value?, immediate.value?,).into()) },
            Sr2OrImm5::Sr2(src_reg) => { Ok(Instruction::new_and_reg(dr.value?, sr1.value?, src_reg.value?,).into()) }
        },
        Operands::Ld { dr, pc_offset9 } => Ok(Instruction::new_ld(dr.value?, compute_offset(pc_offset9, location, symbol_table)?).into()),
        Operands::Ldi { dr, pc_offset9 } => Ok(Instruction::new_ldi(dr.value?, compute_offset(pc_offset9, location, symbol_table)?).into()),
        Operands::Ldr { dr, base, offset6 } => Ok(Instruction::new_ldr(dr.value?, base.value?, offset6.value?).into()),
        Operands::Lea { dr, pc_offset9 } => Ok(Instruction::new_lea(dr.value?, compute_offset(pc_offset9, location, symbol_table)?).into()),

        Operands::St { sr, pc_offset9 } => Ok(Instruction::new_st(sr.value?, compute_offset(pc_offset9, location, symbol_table)?).into()),
        Operands::Sti { sr, pc_offset9 } => Ok(Instruction::new_sti(sr.value?, compute_offset(pc_offset9, location, symbol_table)?).into()),
        Operands::Str { sr, base, offset6 } => Ok(Instruction::new_str(sr.value?, base.value?, offset6.value?).into()),

        Operands::Not { dr, sr } => Ok(Instruction::new_not(dr.value?, sr.value?).into()),

        Operands::Br { pc_offset9, .. } => {
            let nzp = nzp.clone()?.unwrap();
            Ok(Instruction::new_br(
                nzp.n, nzp.z, nzp.p,
                compute_offset(pc_offset9, location, symbol_table)?
            ).into())
        }

        Operands::Jmp { base } => Ok(Instruction::new_jmp(base.value?).into()),
        Operands::Jsr { pc_offset11 } => Ok(Instruction::new_jsr(compute_offset(pc_offset11, location, symbol_table)?).into()),
        Operands::Jsrr { base } => Ok(Instruction::new_jsrr(base.value?).into()),

        Operands::Ret => Ok(Instruction::new_ret().into()),
        Operands::Rti => Ok(Instruction::new_rti().into()),

        Operands::Trap { trap_vec } => Ok(Instruction::new_trap(trap_vec.value?).into()),
        Operands::Getc  => Ok(Instruction::new_trap(0x20).into()),
        Operands::Out   => Ok(Instruction::new_trap(0x21).into()),
        Operands::Puts  => Ok(Instruction::new_trap(0x22).into()),
        Operands::In    => Ok(Instruction::new_trap(0x23).into()),
        Operands::Putsp => Ok(Instruction::new_trap(0x24).into()),
        Operands::Halt  => Ok(Instruction::new_trap(0x25).into()),

        Operands::Stringz { .. }
        | Operands::Blkw { .. }
        | Operands::End { .. } => {
            let expanded_inner = expanded.as_ref().unwrap();
            let values = expanded_inner.as_ref()?;
            Ok(InstructionOrValues::Values(values.clone()))
        }

        Operands::Orig { .. } => { unreachable!("Unexpected attempt to assemble a .ORIG.") }
    }
}

impl From<Instruction> for InstructionOrValues {
    fn from(inst: Instruction) -> Self {
        InstructionOrValues::Instruction(inst, inst.into())
    }
}

fn compute_offset(pc_offset: Checked<ImmOrLabel>, location: Addr, symbol_table: &SymbolTable) -> Result<SignedWord, ConstructInstructionError> {
    use ImmOrLabel::*;

    let pc_offset = pc_offset.value?;
    if let Err(errors) = symbol_table {
        return Err(ConstructInstructionError::SymbolTableInvalid { errors: errors.clone() })
    }
    let symbol_table = symbol_table.as_ref().unwrap();
    match pc_offset {
        Imm(immediate) => Ok(immediate.value?),
        Label(label) => {
            let label_value = label.value?;
            match symbol_table.get(label_value) {
                None => {
                    Err(ConstructInstructionError::InvalidLabel {
                        span: label.src.span,
                        label: label_value.to_string(),
                    })
                },
                Some(addr) => {
                    let label_location = *addr as i64;
                    let offset_base = (location + 1) as i64;
                    Ok((label_location - offset_base) as SignedWord)
                },
            }
        },
    }
}
