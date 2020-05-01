use crate::complete::{Program, Object, ObjectContent, Operation, Operands};
use crate::error::{Error, ParseError};
use crate::ir::ir4_parse_ambiguous_tokens;
use crate::ir::ir5_expand_pseudo_ops;

pub fn extract_errors(program: &Program) -> Vec<Error> {
    let mut errors = Vec::new();

    let Program { objects, memory_placement_errors, .. } = program;
    if objects.len() == 0 {
        errors.push(ParseError::NoObjects.into());
    }

    for object in objects {
        extract_object_errors_into(object, &mut errors);
    }

    for memory_placement_error in memory_placement_errors {
        errors.push(memory_placement_error.into());
    }

    errors
}

fn extract_object_errors_into(object: &Object, errors: &mut Vec<Error>) {
    let Object { origin_src, origin, content, symbol_table, } = object;

    extract_ir5_operation_errors(origin_src, errors);
    origin.extract_error_into(errors);
    extract_object_content_errors(content, errors);

    if let Err(symbol_table_errors) = symbol_table {
        for symbol_table_error in symbol_table_errors {
            errors.push(symbol_table_error.into());
        }
    }
}

fn extract_object_content_errors(object_content: &ObjectContent, errors: &mut Vec<Error>) {
    let ObjectContent { operations, hanging_labels, invalid_lines, .. } = object_content;

    for operation in operations {
        extract_operation_errors(operation, errors);
    }

    for hanging_label in hanging_labels {
        let range = hanging_label.span().unwrap();
        errors.push(ParseError::HangingLabel { range }.into());
    }

    for invalid_line in invalid_lines {
        let range = invalid_line.span();
        errors.push(ParseError::InvalidLine { range }.into());
    }

}

fn extract_operation_errors(operation: &Operation, errors: &mut Vec<Error>) {
    let Operation { label, operands, nzp, instruction_or_values, .. } = operation;

    if let Some(label) = label {
        label.extract_error_into(errors);
    }

    extract_operands_errors(operands, errors);

    if let Err(error) = nzp {
        errors.push(error.into());
    }

    if let Err(inst_error) = instruction_or_values {
        errors.push(inst_error.into());
    }
}

fn extract_ir5_operation_errors(operation: &ir5_expand_pseudo_ops::Operation, errors: &mut Vec<Error>) {
    let ir5_expand_pseudo_ops::Operation { label, operands, nzp, expanded, .. } = operation;

    if let Some(label) = label {
        label.extract_error_into(errors);
    }

    extract_operands_errors(operands, errors);

    if let Err(error) = nzp {
        errors.push(error.into());
    }

    if let Some(Err(parse_error)) = expanded {
        errors.push(parse_error.into());
    }

}

fn extract_operands_errors(operands: &Operands, errors: &mut Vec<Error>) {
    use ir4_parse_ambiguous_tokens::Operands::*;

    match operands {
        Add { dr, sr1, sr2_or_imm5 } => {
            dr.extract_error_into(errors);
            sr1.extract_error_into(errors);
            sr2_or_imm5.extract_error_into(errors);
        },
        And { dr, sr1, sr2_or_imm5 } => {
            dr.extract_error_into(errors);
            sr1.extract_error_into(errors);
            sr2_or_imm5.extract_error_into(errors);
        },
        Br { pc_offset9 } => {
            pc_offset9.extract_error_into(errors);
        },
        Jmp { base } => {
            base.extract_error_into(errors);
        },
        Jsr { pc_offset11 } => {
            pc_offset11.extract_error_into(errors);
        },
        Jsrr { base } => {
            base.extract_error_into(errors);
        },
        Ld { dr, pc_offset9 } => {
            dr.extract_error_into(errors);
            pc_offset9.extract_error_into(errors);
        },
        Ldi { dr, pc_offset9 } => {
            dr.extract_error_into(errors);
            pc_offset9.extract_error_into(errors);
        },
        Ldr { dr, base, offset6 } => {
            dr.extract_error_into(errors);
            base.extract_error_into(errors);
            offset6.extract_error_into(errors);
        },
        Lea { dr, pc_offset9 } => {
            dr.extract_error_into(errors);
            pc_offset9.extract_error_into(errors);
        },
        Not { dr, sr } => {
            dr.extract_error_into(errors);
            sr.extract_error_into(errors);
        },
        St { sr, pc_offset9 } => {
            sr.extract_error_into(errors);
            pc_offset9.extract_error_into(errors);
        }
        Sti { sr, pc_offset9 } => {
            sr.extract_error_into(errors);
            pc_offset9.extract_error_into(errors);
        }
        Str { sr, base, offset6 } => {
            sr.extract_error_into(errors);
            base.extract_error_into(errors);
            offset6.extract_error_into(errors);
        }
        Trap { trap_vec } => {
            trap_vec.extract_error_into(errors);
        }
        Orig { origin } => {
            origin.extract_error_into(errors);
        }
        Fill { value } => {
            value.extract_error_into(errors);
        }
        Blkw { size, .. } => {
            size.extract_error_into(errors);
        }
        Stringz { .. } => {}

        // Putting these in instead of _ to avoid forgetting to change
        Ret
        | Rti
        | Getc
        | Out
        | Puts
        | In
        | Putsp
        | Halt
        | End => {}
    };
}
