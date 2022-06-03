use std::collections::HashMap;
use std::convert::{TryFrom, TryInto};
use std::fmt::Debug;
use std::num::{ParseIntError, TryFromIntError};
use lc3_isa::{Addr, Reg, SignedWord, Word};
use crate::lexer::{ConditionCodes, LiteralValue, Opcode};
use crate::parser::{Operand, result, try_map, try_result};
use crate::parser;
use crate::parser::{Program, WithErrData};

pub(crate) type SymbolTable = HashMap<String, Addr>;

#[derive(Clone)]
pub(crate) enum Sr2OrImm5 {
    Sr2(Reg),
    Imm5(SignedWord)
}

impl TryFrom<Operand> for Sr2OrImm5 {
    type Error = ();

    fn try_from(value: Operand) -> Result<Self, Self::Error> {
        value.clone().try_into()
            .map(Sr2OrImm5::Sr2)
            .or_else(|_|
                value.try_into()
                    .map(Sr2OrImm5::Imm5)
                    .map_err(|_| ()))
    }
}

impl TryFrom<Operand> for PcOffset {
    type Error = ();

    fn try_from(value: Operand) -> Result<Self, Self::Error> {
        value.clone().try_into()
            .map(PcOffset::Number)
            .or_else(|_|
                value.get_label()
                    .ok_or(())
                    .map(PcOffset::Label))
    }
}

impl TryFrom<Operand> for SignedWord {
    type Error = ();

    fn try_from(value: Operand) -> Result<Self, Self::Error> {
        LiteralValue::try_from(value)?
            .try_into()
            .map_err(|_| ())
    }
}

impl TryFrom<Operand> for Word {
    type Error = ();

    fn try_from(value: Operand) -> Result<Self, Self::Error> {
        LiteralValue::try_from(value)?
            .try_into()
            .map_err(|_| ())
    }
}

impl TryFrom<Operand> for u8 {
    type Error = ();

    fn try_from(value: Operand) -> Result<Self, Self::Error> {
        LiteralValue::try_from(value)?
            .try_into()
            .map_err(|_| ())
    }
}

impl TryFrom<Operand> for FillValue {
    type Error = ();

    fn try_from(value: Operand) -> Result<Self, Self::Error> {
        value.clone().try_into()
            .map(FillValue::Number)
            .or_else(|_|
                value.get_label()
                    .ok_or(())
                    .map(FillValue::Label))
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
        match self {
            Instruction::Blkw { size } => *size,

            // +1 is to count the null-terminator
            Instruction::Stringz { string } => (string.len() + 1) as Addr,
            _ => 1,
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


impl TryFrom<parser::Instruction> for Instruction {
    type Error = ();

    fn try_from(i: parser::Instruction) -> Result<Self, Self::Error> {

        let parser::Instruction { opcode: raw_opcode, operands: raw_operands, .. } = i;
        let operands = result(raw_operands)?;
        match result(raw_opcode)? {
            Opcode::Add => {
                let mut os = operands.into_iter();
                let dr = try_map(os.next())?;
                let sr1 = try_map(os.next())?;
                let sr2_or_imm5 = try_map(os.next())?;
                Ok(Instruction::Add { dr, sr1, sr2_or_imm5 })
            }
            Opcode::And => {
                let mut os = operands.into_iter();
                let dr = try_map(os.next())?;
                let sr1 = try_map(os.next())?;
                let sr2_or_imm5 = try_map(os.next())?;
                Ok(Instruction::And { dr, sr1, sr2_or_imm5 })
            }
            Opcode::Br(cond_codes) => {
                let mut os = operands.into_iter();
                let pc_offset9 = try_map(os.next())?;
                Ok(Instruction::Br { cond_codes, pc_offset9 })
            }
            Opcode::Jmp => {
                let mut os = operands.into_iter();
                let base = try_map(os.next())?;
                Ok(Instruction::Jmp { base })
            }
            Opcode::Jsr => {
                let mut os = operands.into_iter();
                let pc_offset11 = try_map(os.next())?;
                Ok(Instruction::Jsr { pc_offset11 })
            }
            Opcode::Jsrr => {
                let mut os = operands.into_iter();
                let base = try_map(os.next())?;
                Ok(Instruction::Jsrr { base })
            }
            Opcode::Ld => {
                let mut os = operands.into_iter();
                let dr = try_map(os.next())?;
                let pc_offset9 = try_map(os.next())?;
                Ok(Instruction::Ld { dr, pc_offset9 })
            }
            Opcode::Ldi => {
                let mut os = operands.into_iter();
                let dr = try_map(os.next())?;
                let pc_offset9 = try_map(os.next())?;
                Ok(Instruction::Ldi { dr, pc_offset9 })
            }
            Opcode::Ldr => {
                let mut os = operands.into_iter();
                let dr = try_map(os.next())?;
                let base = try_map(os.next())?;
                let offset6 = try_map(os.next())?;
                Ok(Instruction::Ldr { dr, base, offset6 })
            }
            Opcode::Lea => {
                let mut os = operands.into_iter();
                let dr = try_map(os.next())?;
                let pc_offset9 = try_map(os.next())?;
                Ok(Instruction::Lea { dr, pc_offset9 })
            }
            Opcode::Not => {
                let mut os = operands.into_iter();
                let dr = try_map(os.next())?;
                let sr = try_map(os.next())?;
                Ok(Instruction::Not { dr, sr })
            }
            Opcode::Ret => Ok(Instruction::Ret),
            Opcode::Rti => Ok(Instruction::Rti),
            Opcode::St => {
                let mut os = operands.into_iter();
                let sr = try_map(os.next())?;
                let pc_offset9 = try_map(os.next())?;
                Ok(Instruction::St { sr, pc_offset9 })
            }
            Opcode::Sti => {
                let mut os = operands.into_iter();
                let sr = try_map(os.next())?;
                let pc_offset9 = try_map(os.next())?;
                Ok(Instruction::Sti { sr, pc_offset9 })
            }
            Opcode::Str => {
                let mut os = operands.into_iter();
                let sr = try_map(os.next())?;
                let base = try_map(os.next())?;
                let offset6 = try_map(os.next())?;
                Ok(Instruction::Str { sr, base, offset6 })
            }
            Opcode::Trap => {
                let mut os = operands.into_iter();
                let trap_vec = try_map(os.next())?;
                Ok(Instruction::Trap { trap_vec })
            }

            // TODO: improve error
            Opcode::Orig => Err(()),

            Opcode::Fill => {
                let mut os = operands.into_iter();
                let value = try_map(os.next())?;
                Ok(Instruction::Fill { value })
            }
            Opcode::Blkw => {
                let mut os = operands.into_iter();
                let size = try_result(os.next())?.get_unqualified_number_value().ok_or(())?;
                Ok(Instruction::Blkw { size })
            }
            Opcode::Stringz => {
                let mut os = operands.into_iter();
                let string = try_result(os.next())?.get_string().ok_or(())?;
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

fn calculate_addr_offset(location_counter: &Addr, label_address: &Addr) -> Result<SignedWord, TryFromIntError> {
    calculate_offset(*location_counter as i32, *label_address as i32)
}

pub(crate) fn calculate_offset(location_counter: i32, label_address: i32) -> Result<SignedWord, TryFromIntError> {
    (label_address - (location_counter + 1)).try_into()
}

pub(crate) fn assemble_instruction(symbol_table: &SymbolTable, location_counter: &Addr, instruction: Instruction) -> Result<AssemblyResult, TryFromIntError> {
    use AssemblyResult::*;
    use ObjectWord::*;

    let res = match instruction {
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
                            let offset = calculate_addr_offset(location_counter, addr)?;
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
                            let offset = calculate_addr_offset(location_counter, addr)?;
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
                            let offset = calculate_addr_offset(location_counter, addr)?;
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
                            let offset = calculate_addr_offset(location_counter, addr)?;
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
                            let offset = calculate_addr_offset(location_counter, addr)?;
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
                            let offset = calculate_addr_offset(location_counter, addr)?;
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
                            let offset = calculate_addr_offset(location_counter, addr)?;
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
                    .map(|c| Value(c as Word))
                    .collect::<Vec<_>>();
            chars.push(Value(0x00)); // null-terminator
            MultipleObjectWords(chars)
        }
    };
    Ok(res)
}

fn first_pass(origin: Addr, instructions: Vec<WithErrData<parser::Instruction>>) -> Result<(Vec<Instruction>, SymbolTable), ()> {
    let mut symbol_table = HashMap::new();
    let mut words = Vec::new();
    let mut location_counter = origin;

    for raw_instruction in instructions.into_iter() {
        let parser_instruction = result(raw_instruction)?;
        if let Some(l) = parser_instruction.label.clone() { // TODO: label not needed for conversion to Instruction; consider changing to TryFrom<(Opcode, Operands)> to avoid clone
            symbol_table.insert(result(l)?, location_counter);
        };

        let instruction: Instruction = parser_instruction.try_into()?;
        let addresses_used = instruction.addresses_occupied();
        words.push(instruction);

        location_counter += addresses_used;
    }

    Ok((words, symbol_table))
}

fn second_pass(symbol_table: SymbolTable, origin: Addr, instructions: Vec<Instruction>) -> Result<Object, TryFromIntError> {
    let mut location_counter = origin;
    let mut words = Vec::new();

    for instruction in instructions.into_iter() {
        let addresses_used = instruction.addresses_occupied();
        match assemble_instruction(&symbol_table, &location_counter, instruction)? {
            AssemblyResult::SingleObjectWord(wd) => { words.push(wd); }
            AssemblyResult::MultipleObjectWords(wds) => { words.extend(wds); }
        }
        location_counter += addresses_used;
    }

    Ok(Object { origin, symbol_table, words })
}

pub(crate) fn get_orig(orig_operands: WithErrData<Vec<WithErrData<Operand>>>) -> Result<Addr, ()> {
    let orig_operand = result(orig_operands)?.remove(0);
    result(orig_operand)?.try_into()
}

pub fn assemble(program: Program) -> Result<Object, ()> {
    let Program { orig, instructions: parser_instructions, .. } = program;
    let origin = get_orig(orig)?;
    let (instructions, symbol_table) = first_pass(origin, parser_instructions)?;
    second_pass(symbol_table, origin, instructions).map_err(|_| ())
}