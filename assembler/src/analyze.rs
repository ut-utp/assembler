//! Functions for identifying errors in the syntax trees produced by [`parse`](crate::parse).
//!
//! This module is primarily for semantic analysis, or identifying semantic errors
//! in a syntax tree. These are errors that don't have to do with incorrect syntax;
//! for example, branching (with `BR`) to a label that isn't defined. It is correct
//! syntax to use `BR` with a label, but the label doesn't refer to any address,
//! so the assembler wouldn't have the necessary information to calculate an offset.
//! In a situation like this where the syntax is correct, but the meaning (semantics)
//! is still invalid or contradictory, it is a semantic error.
//!
//! Secondarily, this module also identifies parse (or *syntax*) errors
//! inserted into the syntax tree during parsing. When performing semantic analysis,
//! the parse errors are typically ignored, in order to identify as many independent errors
//! as possible.
//! When the parse errors affect the meaning, we attempt to work around the missing semantic
//! information by making reasonable assumptions.
//! For example, if program starts with `.ORIG xOOPS`, some analyses may assume
//! that the intent was to place the program at `x3000`, the start of user space,
//! commonly used in examples.
//!
//! In other words, our approach to semantic analysis tries to avoid identifying
//! multiple errors stemming from the same root cause, particularly when the root
//! cause is a parse error. The goal is to be clear where and why a change needs to be
//! made to make the program valid, not to show every problem that an error implies.
//! So when semantic analysis encounters a parse error, it makes whatever assumptions
//! it needs in order to treat the rest of the program as normal.
use std::collections::HashMap;
use std::convert::{TryFrom, TryInto};
use std::fmt::Debug;
use std::string::String;
use itertools::{concat, Itertools, zip};
use lc3_isa::{Addr, Word};
use crate::lex::{LexData, Opcode};
use crate::parse::{File, Instruction, Operand, ProgramBlock};
use crate::{get, get_result, SourceId, Spanned, SpanWithSource, util, WithErrData};
use crate::assemble::calculate_offset;
use crate::error::{Error, InvalidReferenceReason, OperandType, ProgramBlockPlacement, RoughAddr, SingleError};
use crate::error::OperandType::*;
use crate::error::Error::*;
use crate::error::SingleError::*;

#[derive(Default)]
struct ParseErrorsAnalysis {
    errors: Vec<Error>
}

impl ParseErrorsAnalysis {
    fn push_error(&mut self, single_error: SingleError, span: &SpanWithSource) {
        self.errors.push(Spanned(span.clone(), single_error));
    }
}

// TODO: use context to provide useful hints as to *why* the error occurred
impl Visit for ParseErrorsAnalysis {
    type Data = ();

    fn new(_data: Self::Data) -> Self {
        Default::default()
    }

    type Output = ();
    fn finish(self) -> (Self::Output, Vec<Error>) {
        ((), self.errors)
    }

    fn enter_program_block_error(&mut self, span: &SpanWithSource) {
        self.push_error(BadProgramBlock, span);
    }
    fn enter_orig_error(&mut self, span: &SpanWithSource) {
        self.push_error(BadOperands, span);
    }
    fn enter_instruction_error(&mut self, span: &SpanWithSource, _location: &LocationCounter) {
        self.push_error(BadInstruction, span);
    }
    fn enter_label_error(&mut self, span: &SpanWithSource, _location: &LocationCounter) {
        self.push_error(BadLabel, span);
    }
    fn enter_opcode_error(&mut self, span: &SpanWithSource, _location: &LocationCounter) {
        self.push_error(BadOpcode, span);
    }
    fn enter_operands_error(&mut self, span: &SpanWithSource, _location: &LocationCounter) {
        self.push_error(BadOperands, span);
    }
    fn enter_operand_error(&mut self, span: &SpanWithSource, _location: &LocationCounter) {
        self.push_error(BadOperand, span);
    }
}

#[derive(Default)]
struct DuplicateLabelsAnalysis {
    errors: Vec<Error>,
    labels: HashMap<String, Vec<SpanWithSource>>,
}

impl Visit for DuplicateLabelsAnalysis {
    type Data = ();

    fn new(_data: ()) -> Self {
        Default::default()
    }

    type Output = ();
    fn finish(self) -> (Self::Output, Vec<Error>) {
        ((), self.errors)
    }

    fn exit_file(&mut self, _file: &File, _span: &SpanWithSource) {
        let DuplicateLabelsAnalysis { errors, labels } = self;
        labels.iter()
            .filter(|(_, occurrences)| occurrences.len() > 1)
            .map(|(label, occurrences)|
                 Single(occurrences.get(0).unwrap().id.clone(),
                     DuplicateLabel {
                         label: label.clone(),
                         occurrences: occurrences.clone()
                     })
            )
            .for_each(|e| errors.push(e));
    }

    fn enter_label(&mut self, label: &String, span: &SpanWithSource, _location: &LocationCounter) {
        let occurrences = self.labels.entry(label.clone()).or_insert(Vec::new());
        occurrences.push(span.clone());
    }
}

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

const ORIG_ERROR_STARTING_ADDRESS_ESTIMATE: RoughAddr = 0x3000;
const INSTRUCTION_ERROR_ADDRESSES_OCCUPIED_ESTIMATE: RoughAddr = 1;

impl Visit for SymbolTableAnalysis {
    type Data = ();

    fn new(_data: ()) -> Self {
        Default::default()
    }

    type Output = SymbolTable;
    fn finish(self) -> (Self::Output, Vec<Error>) {
        (self.symbol_table, vec![])
    }


    fn enter_label(&mut self, label: &String, _span: &SpanWithSource, location: &LocationCounter) {
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
    errors: Vec<Error>,
    symbol_table: &'a SymbolTable,
    expected_label: Option<ExpectedLabel>
}

impl<'a> LabelOffsetBoundsAnalysis<'a> {
    fn check_offset(&mut self, label: &String, span: &SpanWithSource, width: u8, label_addr: RoughAddr, ref_addr: RoughAddr) {
        match calculate_offset(ref_addr, label_addr) {
            Err(_) => {
                // TODO: make more precise. This case shouldn't be possible unless one of the estimated addresses is far out of bounds.
                self.errors.push(
                    Spanned(span.clone(),
                        InvalidLabelReference {
                            label: label.clone(),
                            reason: InvalidReferenceReason::OutOfBounds
                        }));
            }
            Ok(offset) => {
                if util::min_signed_width(offset as i32) > width {
                    self.errors.push(
                        Spanned(span.clone(),
                            LabelTooDistant {
                                label: label.clone(),
                                width,
                                est_ref_pos: ref_addr,
                                offset,
                                est_label_pos: label_addr,
                            }));
                }
            }
        }
    }
}

impl<'a> Visit for LabelOffsetBoundsAnalysis<'a> {
    type Data = &'a SymbolTable;

    fn new(symbol_table: &'a SymbolTable) -> Self {
        Self {
            errors: Default::default(),
            symbol_table,
            expected_label: Default::default(),
        }
    }

    type Output = ();
    fn finish(self) -> (Self::Output, Vec<Error>) {
        ((), self.errors)
    }


    fn enter_opcode_error(&mut self, _span: &SpanWithSource, _location: &LocationCounter) {
        self.expected_label = None;
    }

    fn enter_opcode(&mut self, opcode: &Opcode, _span: &SpanWithSource, _location: &LocationCounter) {
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

    fn enter_operands(&mut self, operands: &Vec<WithErrData<Operand>>, span: &SpanWithSource, location: &LocationCounter) {
        if let Some(ExpectedLabel { width, position }) = &self.expected_label {
            if let Some((Ok(Operand::Label(label)), op_span_no_source)) = operands.get(*position) {
                let op_span = (span.id.clone(), op_span_no_source.clone()).into();
                match self.symbol_table.get(label) {
                    None => {
                        self.errors.push(
                            Spanned(op_span,
                                InvalidLabelReference {
                                     label: label.clone(),
                                     reason: InvalidReferenceReason::Undefined
                                }));
                    }
                    Some(stv) => match stv {
                        Ok(addr) => {
                            self.check_offset(label, &op_span, *width, *addr as RoughAddr, location.value);
                        }
                        Err(ste) => match ste {
                            InvalidSymbolError::InvalidOrig { estimated_addr }
                            | InvalidSymbolError::PriorInvalidInstruction { estimated_addr } => {
                                self.check_offset(label, &op_span, *width, *estimated_addr, location.value);
                            }
                            InvalidSymbolError::Duplicated => {
                                self.errors.push(
                                    Spanned(op_span,
                                        InvalidLabelReference {
                                            label: label.clone(),
                                            reason: InvalidReferenceReason::Duplicated
                                        }));
                            }
                            InvalidSymbolError::OutOfBounds => {
                                self.errors.push(
                                    Spanned(op_span,
                                        InvalidLabelReference {
                                            label: label.clone(),
                                            reason: InvalidReferenceReason::OutOfBounds
                                        }));
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
    errors: Vec<Error>,
    expected_operands: Option<Vec<OperandType>>
}

impl OperandTypesAnalysis {
    fn check_operands(&mut self, operands: &Vec<WithErrData<Operand>>, span: &SpanWithSource) {
        if let Some(expected) = &self.expected_operands {
            // TODO: create longest common subsequence diff for more precise errors
            let ops_len = operands.len();
            let exp_len = expected.len();
            if ops_len != exp_len {
                self.errors.push(Spanned(span.clone(), WrongNumberOfOperands { expected: exp_len, actual: ops_len }))
            } else {
                for ((op_res, op_span_no_source), exp_ty) in zip(operands, expected) {
                    let op_span = (span.id.clone(), op_span_no_source.clone()).into();
                    if let Ok(op) = op_res {
                        if !exp_ty.check(op) {
                            let actual = if let Operand::NumberLiteral(value) = op {
                                OperandType::of_number_literal(value, Some(exp_ty.accepted_number_signs()))
                            } else {
                                OperandType::of(op)
                            };
                            self.errors.push(Spanned(op_span, OperandTypeMismatch { expected: exp_ty.clone(), actual }));
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

impl Visit for OperandTypesAnalysis {
    type Data = ();

    fn new(_data: Self::Data) -> Self {
        Default::default()
    }

    type Output = ();
    fn finish(self) -> (Self::Output, Vec<Error>) {
        ((), self.errors)
    }


    fn enter_orig(&mut self, orig: &Vec<WithErrData<Operand>>, span: &SpanWithSource, _location: &LocationCounter) {
        self.expected_operands = Some(orig_expected_operands());
        self.check_operands(orig, span);
    }

    fn enter_opcode_error(&mut self, _span: &SpanWithSource, _location: &LocationCounter) {
        self.expected_operands = None;
    }

    fn enter_opcode(&mut self, opcode: &Opcode, _span: &SpanWithSource, _location: &LocationCounter) {
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

    fn enter_operands(&mut self, operands: &Vec<WithErrData<Operand>>, span: &SpanWithSource, _location: &LocationCounter) {
        self.check_operands(operands, span);
    }
}


struct ObjectPlacementAnalysis {
    errors: Vec<Error>,
    last_start: RoughAddr,
    object_index: usize,
    object_spans: Vec<ProgramBlockPlacement>,
}

impl Default for ObjectPlacementAnalysis {
    fn default() -> Self {
        Self {
            errors: Default::default(),
            last_start: ORIG_ERROR_STARTING_ADDRESS_ESTIMATE,
            object_index: 0,
            object_spans: Default::default(),
        }
    }
}

impl Visit for ObjectPlacementAnalysis {
    type Data = ();

    fn new(_data: ()) -> Self {
        Default::default()
    }

    type Output = ();
    fn finish(self) -> (Self::Output, Vec<Error>) {
        ((), self.errors)
    }

    fn exit_file(&mut self, _file: &File, span: &SpanWithSource) {
        self.object_spans.sort_unstable_by_key(|span| span.span_in_memory.start);
        for (op1, op2) in self.object_spans.iter().tuple_windows() {
            if op2.span_in_memory.start < op1.span_in_memory.end {
                self.errors.push(Single(span.id.clone(), SingleError::program_blocks_overlap(op1.clone(), op2.clone())));
            }
        }
    }

    fn exit_program_block(&mut self, _program_block: &ProgramBlock, span: &SpanWithSource, location: &LocationCounter) {
        self.object_spans.push(
            ProgramBlockPlacement {
                position_in_file: self.object_index,
                span_in_file: span.clone(),
                span_in_memory: self.last_start..location.value
            });
        self.object_index += 1;
    }

    fn exit_orig(&mut self, _orig: &Vec<WithErrData<Operand>>, _span: &SpanWithSource, location: &LocationCounter) {
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

fn visit<'a, V, D, O>(data: D, file: &File, span: &SpanWithSource) -> (O, Vec<Error>)
    where V: Visit<Data=D, Output=O>
{
    let mut v = V::new(data);
    v.enter_file(file, span);
    for block in file.blocks.iter() {
        visit_program_block(&mut v, file.id.clone(), block);
    }
    v.exit_file(file, span);
    v.finish()
}

fn visit_program_block(v: &mut impl Visit, id: SourceId, program_block: &WithErrData<ProgramBlock>) {
    let (pb_res, span) = program_block;
    let span = (id.clone(), span.clone()).into();
    match pb_res {
        Err(_) => { v.enter_program_block_error(&span); }
        Ok(pb) => {
            v.enter_program_block(pb, &span);

            let mut location_counter = LocationCounter::new();

            let ProgramBlock { orig, instructions } = pb;
            visit_orig(v, id.clone(), orig, &mut location_counter);
            for instruction in instructions {
                visit_instruction(v, id.clone(), instruction, &mut location_counter);
            }

            v.exit_program_block(pb, &span, &mut location_counter);
        }
    }
}

fn visit_orig(v: &mut impl Visit, id: SourceId, orig: &WithErrData<Vec<WithErrData<Operand>>>, location_counter: &mut LocationCounter) {
    let (orig_res, span) = orig;
    let span = (id.clone(), span.clone()).into();
    match orig_res {
        Err(_) => {
            location_counter.value = ORIG_ERROR_STARTING_ADDRESS_ESTIMATE;
            location_counter.state.if_valid_set(LocationCounterState::InvalidOrig);
            v.enter_orig_error(&span);
        }
        Ok(o) => {
            location_counter.value = get(o, 0)
                .and_then(|op| Word::try_from(op.clone()).map(|w| w as RoughAddr).ok())
                .unwrap_or_else(| | {
                    location_counter.state.if_valid_set(LocationCounterState::InvalidOrig);
                    ORIG_ERROR_STARTING_ADDRESS_ESTIMATE
                });

            v.enter_orig( o, &span, location_counter);
            for operand in o {
                visit_operand(v, id.clone(), operand, location_counter);
            }

            v.exit_orig(o, &span, location_counter);
        }
    }
}

fn visit_instruction(v: &mut impl Visit, id: SourceId, instruction: &WithErrData<Instruction>, location_counter: &mut LocationCounter) {
    let (inst_res, span) = instruction;
    let span = (id.clone(), span.clone()).into();
    match inst_res {
        Err(_) => {
            v.enter_instruction_error(&span, location_counter);
            location_counter.value += INSTRUCTION_ERROR_ADDRESSES_OCCUPIED_ESTIMATE;
            location_counter.state.if_valid_set(LocationCounterState::InvalidInstruction);
        }
        Ok(i) => {
            v.enter_instruction(i, &span, location_counter);

            let Instruction { label, opcode, operands } = i;
            if let Some(l) = label {
                visit_label(v, id.clone(), l, location_counter);
            }
            visit_opcode(v, id.clone(), opcode, location_counter);
            visit_operands(v, id.clone(), operands, location_counter);

            v.exit_instruction(i, &span, location_counter);

            location_counter.value += i.addresses_occupied()
                .unwrap_or_else(|_| {
                    location_counter.state.if_valid_set(LocationCounterState::InvalidInstruction);
                    INSTRUCTION_ERROR_ADDRESSES_OCCUPIED_ESTIMATE as Addr
                }) as RoughAddr;
        }
    }
}

fn visit_label(v: &mut impl Visit, id: SourceId, label: &WithErrData<String>, location_counter: &mut LocationCounter) {
    let (label_res, span) = label;
    let span = (id, span.clone()).into();
    match label_res {
        Err(_) => { v.enter_label_error(&span, location_counter); }
        Ok(l) => { v.enter_label( l, &span, location_counter); }
    }
}

fn visit_opcode(v: &mut impl Visit, id: SourceId, opcode: &WithErrData<Opcode>, location_counter: &mut LocationCounter) {
    let (opcode_res, span) = opcode;
    let span = (id, span.clone()).into();
    match opcode_res {
        Err(_) => { v.enter_opcode_error(&span, location_counter); }
        Ok(oc) => { v.enter_opcode( oc, &span, location_counter); }
    }
}

fn visit_operands(v: &mut impl Visit, id: SourceId, operands: &WithErrData<Vec<WithErrData<Operand>>>, location_counter: &mut LocationCounter) {
    let (ops_res, span) = operands;
    let span = (id.clone(), span.clone()).into();
    match ops_res {
        Err(_) => { v.enter_operands_error(&span, location_counter); }
        Ok(o) => {
            v.enter_operands( o, &span, location_counter);
            for operand in o {
                visit_operand(v, id.clone(), operand, location_counter);
            }
        }
    }
}

fn visit_operand(v: &mut impl Visit, id: SourceId, operand: &WithErrData<Operand>, location_counter: &mut LocationCounter) {
    let (op_res, span) = operand;
    let span = (id, span.clone()).into();
    match op_res {
        Err(_) => { v.enter_operand_error(&span, location_counter); }
        Ok(o) => { v.enter_operand( o, &span, location_counter); }
    }
}

/// A trait for syntax tree visitors, to be used by [`visit`].
///
/// This trait is really just a way to separate the logic of different
/// types of analysis. Analysis that can be done in one independent
/// pass over the tree can be encapsulated in its own `Visit` implementation.
trait Visit {
    type Data;
    fn new(data: Self::Data) -> Self;

    type Output;
    fn finish(self) -> (Self::Output, Vec<Error>);

    fn enter_file(&mut self, _file: &File, _span: &SpanWithSource) {}
    fn exit_file(&mut self, _file: &File, _span: &SpanWithSource) {}

    fn enter_program_block_error(&mut self, _span: &SpanWithSource) {}
    fn enter_program_block(&mut self, _program_block: &ProgramBlock, _span: &SpanWithSource) {}
    fn exit_program_block(&mut self, _program_block: &ProgramBlock, _span: &SpanWithSource, _location: &LocationCounter) {}

    fn enter_orig_error(&mut self, _span: &SpanWithSource) {}
    fn enter_orig(&mut self, _orig: &Vec<WithErrData<Operand>>, _span: &SpanWithSource, _location: &LocationCounter) {}
    fn exit_orig(&mut self, _orig: &Vec<WithErrData<Operand>>, _span: &SpanWithSource, _location: &LocationCounter) {}

    fn enter_instruction_error(&mut self, _span: &SpanWithSource, _location: &LocationCounter) {}
    fn enter_instruction(&mut self, _instruction: &Instruction, _span: &SpanWithSource, _location: &LocationCounter) {}
    fn exit_instruction(&mut self, _instruction: &Instruction, _span: &SpanWithSource, _location: &LocationCounter) {}

    fn enter_label_error(&mut self, _span: &SpanWithSource, _location: &LocationCounter) {}
    fn enter_label(&mut self, _label: &String, _span: &SpanWithSource, _location: &LocationCounter) {}

    fn enter_opcode_error(&mut self, _span: &SpanWithSource, _location: &LocationCounter) {}
    fn enter_opcode(&mut self, _opcode: &Opcode, _span: &SpanWithSource, _location: &LocationCounter) {}

    fn enter_operands_error(&mut self, _span: &SpanWithSource, _location: &LocationCounter) {}
    fn enter_operands(&mut self, _operands: &Vec<WithErrData<Operand>>, _span: &SpanWithSource, _location: &LocationCounter) {}

    fn enter_operand_error(&mut self, _span: &SpanWithSource, _location: &LocationCounter) {}
    fn enter_operand(&mut self, _operand: &Operand, _span: &SpanWithSource, _location: &LocationCounter) {}
}

/// Implement [`Visit`] for tuples of [`Visit`].
/// In general, each method is called on the elements of the tuple in sequence,
/// and if there are results, they are combined in a result tuple in the same sequence.
macro_rules! impl_visit_tuple {
    () => {};
    ($head:ident $head_data:ident $head_output:ident, $($tail:ident $tail_data:ident $tail_output:ident,)*) => {
        impl<$head, $head_data, $head_output, $($tail, $tail_data, $tail_output),*> Visit for ($head, $($tail),*)
        where
            $head: Visit<Data=$head_data, Output=$head_output>,
            $($tail: Visit<Data=$tail_data, Output=$tail_output>),*
        {
            type Data = ($head_data, $($tail_data),*);

            fn new(($head_data, $($tail_data,)*): Self::Data) -> Self {
                (
                    $head::new($head_data),
                    $(
                        $tail::new($tail_data)
                    ),*
                )
            }

            type Output = ($head_output, $($tail_output),*);

            fn finish(self) -> (Self::Output, Vec<Error>) {
                let ($head, $($tail),*) = self;
                let ($head_output, $head_data) = $head.finish();
                $(
                    let ($tail_output, $tail_data) = $tail.finish();
                )*
                (
                    (
                        $head_output,
                        $($tail_output),*
                    )
                    ,
                    concat([
                        $head_data,
                        $($tail_data),*
                    ])
                )
            }

            fn enter_file(&mut self, file: &File, span: &SpanWithSource) {
                let ($head, $($tail,)*) = self;
                $head.enter_file(file, span);
                $(
                    $tail.enter_file(file, span);
                )*
            }
            fn exit_file(&mut self, file: &File, span: &SpanWithSource) {
                let ($head, $($tail,)*) = self;
                $head.exit_file(file, span);
                $(
                    $tail.exit_file(file, span);
                )*
            }

            fn enter_program_block_error(&mut self, span: &SpanWithSource) {
                let ($head, $($tail,)*) = self;
                $head.enter_program_block_error(span);
                $(
                    $tail.enter_program_block_error(span);
                )*
            }
            fn enter_program_block(&mut self, program_block: &ProgramBlock, span: &SpanWithSource) {
                let ($head, $($tail,)*) = self;
                $head.enter_program_block(program_block, span);
                $(
                    $tail.enter_program_block(program_block, span);
                )*
            }
            fn exit_program_block(&mut self, program_block: &ProgramBlock, span: &SpanWithSource, location: &LocationCounter) {
                let ($head, $($tail,)*) = self;
                $head.exit_program_block(program_block, span, location);
                $(
                    $tail.exit_program_block(program_block, span, location);
                )*
            }

            fn enter_orig_error(&mut self, span: &SpanWithSource) {
                let ($head, $($tail,)*) = self;
                $head.enter_orig_error(span);
                $(
                    $tail.enter_orig_error(span);
                )*
            }
            fn enter_orig(&mut self, orig: &Vec<WithErrData<Operand>>, span: &SpanWithSource, location: &LocationCounter) {
                let ($head, $($tail,)*) = self;
                $head.enter_orig(orig, span, location);
                $(
                    $tail.enter_orig(orig, span, location);
                )*
            }
            fn exit_orig(&mut self, orig: &Vec<WithErrData<Operand>>, span: &SpanWithSource, location: &LocationCounter) {
                let ($head, $($tail,)*) = self;
                $head.exit_orig(orig, span, location);
                $(
                    $tail.exit_orig(orig, span, location);
                )*
            }

            fn enter_instruction_error(&mut self, span: &SpanWithSource, location: &LocationCounter) {
                let ($head, $($tail,)*) = self;
                $head.enter_instruction_error(span, location);
                $(
                    $tail.enter_instruction_error(span, location);
                )*
            }
            fn enter_instruction(&mut self, instruction: &Instruction, span: &SpanWithSource, location: &LocationCounter) {
                let ($head, $($tail,)*) = self;
                $head.enter_instruction(instruction, span, location);
                $(
                    $tail.enter_instruction(instruction, span, location);
                )*
            }
            fn exit_instruction(&mut self, instruction: &Instruction, span: &SpanWithSource, location: &LocationCounter) {
                let ($head, $($tail,)*) = self;
                $head.exit_instruction(instruction, span, location);
                $(
                    $tail.exit_instruction(instruction, span, location);
                )*
            }

            fn enter_label_error(&mut self, span: &SpanWithSource, location: &LocationCounter) {
                let ($head, $($tail,)*) = self;
                $head.enter_label_error(span, location);
                $(
                    $tail.enter_label_error(span, location);
                )*
            }
            fn enter_label(&mut self, label: &String, span: &SpanWithSource, location: &LocationCounter) {
                let ($head, $($tail,)*) = self;
                $head.enter_label(label, span, location);
                $(
                    $tail.enter_label(label, span, location);
                )*
            }

            fn enter_opcode_error(&mut self, span: &SpanWithSource, location: &LocationCounter) {
                let ($head, $($tail,)*) = self;
                $head.enter_opcode_error(span, location);
                $(
                    $tail.enter_opcode_error(span, location);
                )*
            }
            fn enter_opcode(&mut self, opcode: &Opcode, span: &SpanWithSource, location: &LocationCounter) {
                let ($head, $($tail,)*) = self;
                $head.enter_opcode(opcode, span, location);
                $(
                    $tail.enter_opcode(opcode, span, location);
                )*
            }

            fn enter_operands_error(&mut self, span: &SpanWithSource, location: &LocationCounter) {
                let ($head, $($tail,)*) = self;
                $head.enter_operands_error(span, location);
                $(
                    $tail.enter_operands_error(span, location);
                )*
            }
            fn enter_operands(&mut self, operands: &Vec<WithErrData<Operand>>, span: &SpanWithSource, location: &LocationCounter) {
                let ($head, $($tail,)*) = self;
                $head.enter_operands(operands, span, location);
                $(
                    $tail.enter_operands(operands, span, location);
                )*
            }

            fn enter_operand_error(&mut self, span: &SpanWithSource, location: &LocationCounter) {
                let ($head, $($tail,)*) = self;
                $head.enter_operand_error(span, location);
                $(
                    $tail.enter_operand_error(span, location);
                )*
            }
            fn enter_operand(&mut self, operand: &Operand, span: &SpanWithSource, location: &LocationCounter) {
                let ($head, $($tail,)*) = self;
                $head.enter_operand(operand, span, location);
                $(
                    $tail.enter_operand(operand, span, location);
                )*
            }
        }

        impl_visit_tuple!($($tail $tail_data $tail_output,)*);
    }
}

impl_visit_tuple!(A DA OA, B DB OB, C DC OC, D DD OD, E DE OE,);

fn analyze_lex_data(lex_data: &LexData, file_span: &SpanWithSource) -> Vec<Error> {
    let mut errors = Vec::new();
    if lex_data.no_tokens {
        errors.push(Single(file_span.id.clone(), NoTokens))
    } else {
        if !lex_data.orig_present {
            let start_span = (file_span.id.clone(), file_span.span.start..file_span.span.start).into();
            errors.push(Spanned(start_span, NoOrig));
        }
        if !lex_data.end_present {
            let end_span = (file_span.id.clone(), file_span.span.end..file_span.span.end).into();
            errors.push(Spanned(end_span, NoEnd));
        }
    }
    errors
}

/// Identify as many independent errors as possible which are present in the given [`File`](crate::parse::File).
///
/// An error indicates that assembly will not be successful, and why.
/// If the result is an empty vector, the program is valid and assembly will succeed (save for bugs in analysis or assembly).
///
/// The [`LexData`](crate::lex::LexData) provides information from lexing that is
/// lost during parsing, but can help produce more useful errors.
///
/// See [the `error` module](crate::error) for details about the types of errors
/// and how to present them.
///
/// See the [module-level documentation](crate::analyze) for a discussion of this
/// function's philosophy toward generating errors. In short, it will try to
/// identify a set of independent issues that can be corrected to make
/// the program valid, but not necessarily *all* the issues present. It
/// also makes some assumptions which may not always be correct depending
/// on the intent of the input's programmer.
pub fn validate(lex_data: &LexData, file_spanned: &Spanned<File>) -> Vec<Error> {
    let (file, file_span) = file_spanned;

    let file_span_with_source = (file.id.clone(), file_span.clone()).into();
    let errors_from_lex_data = analyze_lex_data(&lex_data, &file_span_with_source);

    let ((symbol_table, _, _, _, _), first_pass_errors) =
        visit::<(
            SymbolTableAnalysis,
            ParseErrorsAnalysis,
            DuplicateLabelsAnalysis,
            OperandTypesAnalysis,
            ObjectPlacementAnalysis,
        ), _, _>(((), (), (), (), ()), file, &file_span_with_source);

    let (_, second_pass_errors) =
        visit::<LabelOffsetBoundsAnalysis, _, _>(&symbol_table, file, &file_span_with_source);

    concat([
        errors_from_lex_data,
        first_pass_errors,
        second_pass_errors,
    ])
}

