use std::collections::HashMap;
use std::convert::{TryFrom, TryInto};
use std::fmt::Debug;
use std::num::{ParseIntError, TryFromIntError};
use lc3_isa::{Addr, Reg, SignedWord, Word};
use crate::new::lexer::{ConditionCodes, LiteralValue, Opcode};
use crate::new::parser::Operand;
use super::parser;
use super::parser::{Program, WithErrData};

pub(crate) type SymbolTable = HashMap<String, Addr>;

#[derive(Clone)]
pub(crate) enum Sr2OrImm5 {
    Sr2(Reg),
    Imm5(SignedWord)
}

impl TryFrom<Operand> for Sr2OrImm5 {
    type Error = ();

    fn try_from(value: Operand) -> Result<Self, Self::Error> {
        Reg::try_from(value.clone())
            .map(Sr2OrImm5::Sr2)
            .or_else(|_|
                LiteralValue::try_from(value)
                    .unwrap_try_into()
                    .map(Sr2OrImm5::Imm5)
                    .map_err(|_| ()))
    }
}

impl TryFrom<Operand> for PcOffset {
    type Error = ();

    fn try_from(value: Operand) -> Result<Self, Self::Error> {
        LiteralValue::try_from(value.clone())
            .map(|lv| {
                let sw = lv.try_into().unwrap();
                PcOffset::Number(sw)
            })
            .or_else(|_| Ok(PcOffset::Label(value.label())))
    }
}

impl TryFrom<Operand> for SignedWord {
    type Error = TryFromIntError;

    fn try_from(value: Operand) -> Result<Self, Self::Error> {
        LiteralValue::try_from(value).unwrap_try_into()
    }
}

impl TryFrom<Operand> for Word {
    type Error = TryFromIntError;

    fn try_from(value: Operand) -> Result<Self, Self::Error> {
        LiteralValue::try_from(value).unwrap_try_into()
    }
}

impl TryFrom<Operand> for u8 {
    type Error = TryFromIntError;

    fn try_from(value: Operand) -> Result<Self, Self::Error> {
        LiteralValue::try_from(value).unwrap_try_into()
    }
}

impl TryFrom<Operand> for FillValue {
    type Error = ();

    fn try_from(value: Operand) -> Result<Self, Self::Error> {
        LiteralValue::try_from(value.clone())
            .unwrap_try_into()
            .map(FillValue::Number)
            .or_else(|_| Ok(FillValue::Label(value.label())))
    }
}

#[derive(Clone)]
pub(crate) enum PcOffset {
    Number(SignedWord),
    Label(String),
}

#[derive(Clone)]
pub(crate) enum FillValue {
    Number(Word),
    Label(String),
}

#[derive(Clone)]
pub(crate) enum Instruction {
    Add { dr: Reg, sr1: Reg, sr2_or_imm5: Sr2OrImm5 },
    And { dr: Reg, sr1: Reg, sr2_or_imm5: Sr2OrImm5 },
    Br { cond_codes: ConditionCodes, pc_offset9: PcOffset },
    Jmp { base: Reg },
    Jsr { pc_offset11: PcOffset },
    Jsrr { base: Reg },
    Ld { dr: Reg, pc_offset9: PcOffset },
    Ldi { dr: Reg, pc_offset9: PcOffset },
    Ldr { dr: Reg, base: Reg, offset6: SignedWord },
    Lea { dr: Reg, pc_offset9: PcOffset },
    Not { dr: Reg, sr: Reg },
    Ret,
    Rti,
    St { sr: Reg, pc_offset9: PcOffset },
    Sti { sr: Reg, pc_offset9: PcOffset },
    Str { sr: Reg, base: Reg, offset6: SignedWord },
    Trap { trap_vec: u8 },

    Fill { value: FillValue },
    Blkw { size: Addr }, // Addr used here to signify a number of locations, as max value of Addr is number of possible Addrs.
    Stringz { string: String },
}

impl Instruction {
    fn addresses_occupied(&self) -> Addr {
        use Instruction::*;

        match self {
            Add { .. }
            | And { .. }
            | Br { .. }
            | Jmp { .. }
            | Jsr { .. }
            | Jsrr { .. }
            | Ld { .. }
            | Ldi { .. }
            | Ldr { .. }
            | Lea { .. }
            | Not { .. }
            | Ret
            | Rti
            | St { .. }
            | Sti { .. }
            | Str { .. }
            | Trap { .. }
            | Fill { .. } => 1,

            Blkw { size } => *size,

            // +1 is to count the null-terminator
            Stringz { string } => (string.len() + 1) as Addr, // TODO: correct for escape characters
        }
    }
}

pub struct Object {
    pub(crate) symbol_table: SymbolTable,
    pub(crate) origin: Addr,
    pub(crate) words: Vec<ObjectWord>,
}

#[derive(Clone)]
pub(crate) enum ObjectWord {
    Value(Word),
    UnlinkedInstruction(Instruction),
}

fn unwrap<T>(v: WithErrData<T>) -> T {
    v.0.unwrap()
}

trait UnwrapTryFrom<T> where
    Self: Sized
{
    type Error;

    fn unwrap_try_from(v: T) -> Result<Self, Self::Error>;
}

trait UnwrapTryInto<T> {
    type Error;

    fn unwrap_try_into(self) -> Result<T, Self::Error>;
}

impl<T, U, E> UnwrapTryInto<T> for U where
    T: UnwrapTryFrom<U, Error=E>
{
    type Error = E;

    fn unwrap_try_into(self) -> Result<T, Self::Error> {
        T::unwrap_try_from(self)
    }
}

impl<T, U, E> UnwrapTryFrom<WithErrData<T>> for U where
    U: TryFrom<T, Error=E>
{
    type Error = E;

    fn unwrap_try_from(v: WithErrData<T>) -> Result<Self, Self::Error> {
        unwrap(v).try_into()
    }
}

impl<T, U, E, E2: Debug> UnwrapTryFrom<Result<T, E2>> for U where
    U: TryFrom<T, Error=E>
{
    type Error = E;

    fn unwrap_try_from(v: Result<T, E2>) -> Result<Self, Self::Error> {
        v.unwrap().try_into()
    }
}

fn unwrap_into<T, U, E>(maybe_v: Option<T>) -> U where
    E: Debug,
    U: UnwrapTryFrom<T, Error=E>
{
    maybe_v.unwrap().unwrap_try_into().unwrap()
}


impl TryFrom<parser::Instruction> for Instruction {
    type Error = ();

    fn try_from(i: parser::Instruction) -> Result<Self, Self::Error> {

        let parser::Instruction { opcode: raw_opcode, operands: raw_operands, .. } = i;
        let operands = unwrap(raw_operands);
        match unwrap(raw_opcode) {
            Opcode::Add => {
                let mut os = operands.into_iter();
                let dr = unwrap_into(os.next());
                let sr1 = unwrap_into(os.next());
                let sr2_or_imm5 = unwrap_into(os.next());
                Ok(Instruction::Add { dr, sr1, sr2_or_imm5 })
            }
            Opcode::And => {
                let mut os = operands.into_iter();
                let dr = unwrap_into(os.next());
                let sr1 = unwrap_into(os.next());
                let sr2_or_imm5 = unwrap_into(os.next());
                Ok(Instruction::And { dr, sr1, sr2_or_imm5 })
            }
            Opcode::Br(cond_codes) => {
                let mut os = operands.into_iter();
                let pc_offset9 = unwrap_into(os.next());
                Ok(Instruction::Br { cond_codes, pc_offset9 })
            }
            Opcode::Jmp => {
                let mut os = operands.into_iter();
                let base = unwrap_into(os.next());
                Ok(Instruction::Jmp { base })
            }
            Opcode::Jsr => {
                let mut os = operands.into_iter();
                let pc_offset11 = unwrap_into(os.next());
                Ok(Instruction::Jsr { pc_offset11 })
            }
            Opcode::Jsrr => {
                let mut os = operands.into_iter();
                let base = unwrap_into(os.next());
                Ok(Instruction::Jsrr { base })
            }
            Opcode::Ld => {
                let mut os = operands.into_iter();
                let dr = unwrap_into(os.next());
                let pc_offset9 = unwrap_into(os.next());
                Ok(Instruction::Ld { dr, pc_offset9 })
            }
            Opcode::Ldi => {
                let mut os = operands.into_iter();
                let dr = unwrap_into(os.next());
                let pc_offset9 = unwrap_into(os.next());
                Ok(Instruction::Ldi { dr, pc_offset9 })
            }
            Opcode::Ldr => {
                let mut os = operands.into_iter();
                let dr = unwrap_into(os.next());
                let base = unwrap_into(os.next());
                let offset6 = unwrap_into(os.next());
                Ok(Instruction::Ldr { dr, base, offset6 })
            }
            Opcode::Lea => {
                let mut os = operands.into_iter();
                let dr = unwrap_into(os.next());
                let pc_offset9 = unwrap_into(os.next());
                Ok(Instruction::Lea { dr, pc_offset9 })
            }
            Opcode::Not => {
                let mut os = operands.into_iter();
                let dr = unwrap_into(os.next());
                let sr = unwrap_into(os.next());
                Ok(Instruction::Not { dr, sr })
            }
            Opcode::Ret => Ok(Instruction::Ret),
            Opcode::Rti => Ok(Instruction::Rti),
            Opcode::St => {
                let mut os = operands.into_iter();
                let sr = unwrap_into(os.next());
                let pc_offset9 = unwrap_into(os.next());
                Ok(Instruction::St { sr, pc_offset9 })
            }
            Opcode::Sti => {
                let mut os = operands.into_iter();
                let sr = unwrap_into(os.next());
                let pc_offset9 = unwrap_into(os.next());
                Ok(Instruction::Sti { sr, pc_offset9 })
            }
            Opcode::Str => {
                let mut os = operands.into_iter();
                let sr = unwrap_into(os.next());
                let base = unwrap_into(os.next());
                let offset6 = unwrap_into(os.next());
                Ok(Instruction::Str { sr, base, offset6 })
            }
            Opcode::Trap => {
                let mut os = operands.into_iter();
                let trap_vec = unwrap_into(os.next());
                Ok(Instruction::Trap { trap_vec })
            }

            // TODO: improve error
            Opcode::Orig => Err(()),
            Opcode::End => Err(()),

            Opcode::Fill => {
                let mut os = operands.into_iter();
                let value = unwrap_into(os.next());
                Ok(Instruction::Fill { value })
            }
            Opcode::Blkw => {
                let mut os = operands.into_iter();
                let size = unwrap(os.next().unwrap()).unqualified_number_value();
                Ok(Instruction::Blkw { size })
            }
            Opcode::Stringz => {
                let mut os = operands.into_iter();
                let string = unwrap(os.next().unwrap()).string();
                Ok(Instruction::Stringz { string })
            }

            Opcode::Getc  => Ok(Instruction::Trap { trap_vec: 0x20 }),
            Opcode::Out   => Ok(Instruction::Trap { trap_vec: 0x21 }),
            Opcode::Puts  => Ok(Instruction::Trap { trap_vec: 0x22 }),
            Opcode::In    => Ok(Instruction::Trap { trap_vec: 0x23 }),
            Opcode::Putsp => Ok(Instruction::Trap { trap_vec: 0x24 }),
            Opcode::Halt  => Ok(Instruction::Trap { trap_vec: 0x25 }),
        }
    }
}

impl TryFrom<parser::Instruction> for ObjectWord {
    type Error = ();

    fn try_from(value: parser::Instruction) -> Result<Self, Self::Error> {
        Instruction::try_from(value)
            .map(ObjectWord::UnlinkedInstruction)
    }
}

pub(crate) enum AssemblyResult {
    SingleObjectWord(ObjectWord),
    MultipleObjectWords(Vec<ObjectWord>),
}

fn calculate_offset(location_counter: &Addr, label_address: &Addr) -> SignedWord {
    let lc = *location_counter as i32;
    let la = *label_address as i32;
    (la - (lc + 1)) as SignedWord
}

pub(crate) fn try_assemble(symbol_table: &SymbolTable, location_counter: &Addr, instruction: Instruction) -> AssemblyResult {
    use AssemblyResult::*;
    use ObjectWord::*;

    match instruction {
        Instruction::Add { dr, sr1, sr2_or_imm5 } => {
            let word =
                match sr2_or_imm5 {
                    Sr2OrImm5::Sr2(sr2) => lc3_isa::Instruction::new_add_reg(dr, sr1, sr2),
                    Sr2OrImm5::Imm5(imm5) => lc3_isa::Instruction::new_add_imm(dr, sr1, imm5),
                }.into();
            SingleObjectWord(Value(word))
        }
        Instruction::And { dr, sr1, sr2_or_imm5 } => {
            let word =
                match sr2_or_imm5 {
                    Sr2OrImm5::Sr2(sr2) => lc3_isa::Instruction::new_and_reg(dr, sr1, sr2),
                    Sr2OrImm5::Imm5(imm5) => lc3_isa::Instruction::new_and_imm(dr, sr1, imm5),
                }.into();
            SingleObjectWord(Value(word))
        }
        Instruction::Br { cond_codes: ConditionCodes { n, z, p }, pc_offset9 } => {
            match pc_offset9 {
                PcOffset::Number(sw) => SingleObjectWord(Value(lc3_isa::Instruction::new_br(n, z, p, sw).into())),
                PcOffset::Label(label) =>
                    match symbol_table.get(&label) {
                        Some(addr) => {
                            let offset = calculate_offset(location_counter, addr);
                            SingleObjectWord(Value(lc3_isa::Instruction::new_br(n, z, p, offset).into()))
                        }
                        None => SingleObjectWord(UnlinkedInstruction(Instruction::Br { cond_codes: ConditionCodes { n, z, p }, pc_offset9: PcOffset::Label(label) })),
                    }
            }
        }
        Instruction::Jmp { base } => SingleObjectWord(Value(lc3_isa::Instruction::new_jmp(base).into())),
        Instruction::Jsr { pc_offset11 } => {
            match pc_offset11 {
                PcOffset::Number(sw) => SingleObjectWord(Value(lc3_isa::Instruction::new_jsr(sw).into())),
                PcOffset::Label(label) =>
                    match symbol_table.get(&label) {
                        Some(addr) => {
                            let offset = calculate_offset(location_counter, addr);
                            SingleObjectWord(Value(lc3_isa::Instruction::new_jsr(offset).into()))
                        }
                        None => SingleObjectWord(UnlinkedInstruction(Instruction::Jsr { pc_offset11: PcOffset::Label(label) })),
                    }
            }
        }
        Instruction::Jsrr { base } => SingleObjectWord(Value(lc3_isa::Instruction::new_jsrr(base).into())),
        Instruction::Ld { dr, pc_offset9 } => {
            match pc_offset9 {
                PcOffset::Number(sw) => SingleObjectWord(Value(lc3_isa::Instruction::new_ld(dr, sw).into())),
                PcOffset::Label(label) =>
                    match symbol_table.get(&label) {
                        Some(addr) => {
                            let offset = calculate_offset(location_counter, addr);
                            SingleObjectWord(Value(lc3_isa::Instruction::new_ld(dr, offset).into()))
                        }
                        None => SingleObjectWord(UnlinkedInstruction(Instruction::Ld { dr, pc_offset9: PcOffset::Label(label)})),
                    }
            }
        }
        Instruction::Ldi { dr, pc_offset9 } => {
            match pc_offset9 {
                PcOffset::Number(sw) => SingleObjectWord(Value(lc3_isa::Instruction::new_ldi(dr, sw).into())),
                PcOffset::Label(label) =>
                    match symbol_table.get(&label) {
                        Some(addr) => {
                            let offset = calculate_offset(location_counter, addr);
                            SingleObjectWord(Value(lc3_isa::Instruction::new_ldi(dr, offset).into()))
                        }
                        None => SingleObjectWord(UnlinkedInstruction(Instruction::Ldi { dr, pc_offset9: PcOffset::Label(label)})),
                    }
            }
        }
        Instruction::Ldr { dr, base, offset6 } => SingleObjectWord(Value(lc3_isa::Instruction::new_ldr(dr, base, offset6).into())),
        Instruction::Lea { dr, pc_offset9 } => {
            match pc_offset9 {
                PcOffset::Number(sw) => SingleObjectWord(Value(lc3_isa::Instruction::new_lea(dr, sw).into())),
                PcOffset::Label(label) =>
                    match symbol_table.get(&label) {
                        Some(addr) => {
                            let offset = calculate_offset(location_counter, addr);
                            SingleObjectWord(Value(lc3_isa::Instruction::new_lea(dr, offset).into()))
                        }
                        None => SingleObjectWord(UnlinkedInstruction(Instruction::Lea { dr, pc_offset9: PcOffset::Label(label)})),
                    }
            }
        }
        Instruction::Not { dr, sr } => SingleObjectWord(Value(lc3_isa::Instruction::new_not(dr, sr).into())),
        Instruction::Ret => SingleObjectWord(Value(lc3_isa::Instruction::new_ret().into())),
        Instruction::Rti => SingleObjectWord(Value(lc3_isa::Instruction::new_rti().into())),
        Instruction::St { sr, pc_offset9 } => {
            match pc_offset9 {
                PcOffset::Number(sw) => SingleObjectWord(Value(lc3_isa::Instruction::new_st(sr, sw).into())),
                PcOffset::Label(label) =>
                    match symbol_table.get(&label) {
                        Some(addr) => {
                            let offset = calculate_offset(location_counter, addr);
                            SingleObjectWord(Value(lc3_isa::Instruction::new_st(sr, offset).into()))
                        }
                        None => SingleObjectWord(UnlinkedInstruction(Instruction::St { sr, pc_offset9: PcOffset::Label(label)})),
                    }
            }
        }
        Instruction::Sti { sr, pc_offset9 } => {
            match pc_offset9 {
                PcOffset::Number(sw) => SingleObjectWord(Value(lc3_isa::Instruction::new_sti(sr, sw).into())),
                PcOffset::Label(label) =>
                    match symbol_table.get(&label) {
                        Some(addr) => {
                            let offset = calculate_offset(location_counter, addr);
                            SingleObjectWord(Value(lc3_isa::Instruction::new_sti(sr, offset).into()))
                        }
                        None => SingleObjectWord(UnlinkedInstruction(Instruction::Sti { sr, pc_offset9: PcOffset::Label(label)})),
                    }
            }
        }
        Instruction::Str { sr, base, offset6 } => SingleObjectWord(Value(lc3_isa::Instruction::new_str(sr, base, offset6).into())),
        Instruction::Trap { trap_vec } => SingleObjectWord(Value(lc3_isa::Instruction::new_trap(trap_vec).into())),

        Instruction::Fill { value } => {
            match value {
                FillValue::Number(sw) => SingleObjectWord(Value(sw)),
                FillValue::Label(label) =>
                    match symbol_table.get(&label) {
                        Some(addr) => SingleObjectWord(Value(*addr)),
                        None => SingleObjectWord(UnlinkedInstruction(Instruction::Fill { value: FillValue::Label(label) })),
                    }
            }
        }

        Instruction::Blkw { size } => MultipleObjectWords(
            std::iter::repeat(Value(0x00))
                .take(size as usize)
                .collect()),
        Instruction::Stringz { string } => {
            let mut chars = string.chars()
                    .map(|c| Value(c as Word)) // TODO: correct for escape chars
                    .collect::<Vec<_>>();
            chars.push(Value(0x00)); // null-terminator
            MultipleObjectWords(chars)
        }
    }
}

fn first_pass(origin: Addr, instructions: Vec<WithErrData<parser::Instruction>>) -> (Vec<Instruction>, SymbolTable) {
    let mut symbol_table = HashMap::new();
    let mut words = Vec::new();
    let mut location_counter = origin;

    for raw_instruction in instructions.into_iter() {
        let parser_instruction = unwrap(raw_instruction);
        if let Some(l) = parser_instruction.label.clone() { // TODO: label not needed for conversion to Instruction; consider changing to TryFrom<(Opcode, Operands)> to avoid clone
            symbol_table.insert(unwrap(l), location_counter);
        };

        let instruction: Instruction = parser_instruction.try_into().unwrap();
        let addresses_used = instruction.addresses_occupied();
        words.push(instruction);

        location_counter += addresses_used;
    }

    (words, symbol_table)
}

fn second_pass(symbol_table: SymbolTable, origin: Addr, instructions: Vec<Instruction>) -> Object {
    let mut location_counter = origin;
    let mut words = Vec::new();

    for instruction in instructions.into_iter() {
        let addresses_used = instruction.addresses_occupied();
        match try_assemble(&symbol_table, &location_counter, instruction) {
            AssemblyResult::SingleObjectWord(wd) => { words.push(wd); }
            AssemblyResult::MultipleObjectWords(wds) => { words.extend(wds); }
        }
        location_counter += addresses_used;
    }

    Object { origin, symbol_table, words }
}

pub fn assemble(program: Program) -> Object {
    let Program { orig, instructions: parser_instructions, .. } = program;
    let parser::Instruction { operands: raw_orig_operands, .. } = unwrap(orig);
    let orig_operand = unwrap(raw_orig_operands).remove(0);
    let origin = LiteralValue::unwrap_try_from(orig_operand).unwrap_try_into().unwrap();

    let (instructions, symbol_table) = first_pass(origin, parser_instructions);
    second_pass(symbol_table, origin, instructions)
}