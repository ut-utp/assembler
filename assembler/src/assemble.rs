use std::collections::HashMap;
use std::convert::{TryFrom, TryInto};
use std::num::TryFromIntError;
use lc3_isa::{Addr, Reg, SignedWord, Word};
use crate::lex::{ConditionCodes, LiteralValue, Opcode};
use crate::parse::Operand;
use crate::{parse, result, try_map, try_result, WithErrData};

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
    fn new_trap(trap_vec: u8) -> Self {
        Self::Trap { trap_vec }
    }

    fn addresses_occupied(&self) -> Addr {
        match self {
            Instruction::Blkw { size } => *size,

            // +1 is to count the null-terminator
            Instruction::Stringz { string } => (string.len() + 1) as Addr,
            _ => 1,
        }
    }
}

struct FirstPassBlock {
    origin: Addr,
    instructions: Vec<Instruction>,
}

pub struct Object {
    pub(crate) symbol_table: SymbolTable,
    pub(crate) blocks: Vec<ObjectBlock>,
}

pub struct ObjectBlock {
    pub(crate) origin: Addr,
    pub(crate) words: Vec<ObjectWord>,
}

#[derive(Clone)]
pub(crate) enum ObjectWord {
    Value(Word),
    UnlinkedInstruction(Instruction),
}


macro_rules! try_map_operands {
    ($operands:expr => $variant:ident { $($field:ident),*$(,)* })
    =>
    {
        {
            let mut os = $operands.into_iter();
            let i = Instruction::$variant {
                $($field: try_map(os.next())?,)*
            };
            Ok(i)
        }
    }
}

impl TryFrom<(WithErrData<Opcode>, WithErrData<Vec<WithErrData<Operand>>>)> for Instruction {
    type Error = ();


    fn try_from((raw_opcode, raw_operands): (WithErrData<Opcode>, WithErrData<Vec<WithErrData<Operand>>>)) -> Result<Self, Self::Error> {
        let operands = result(raw_operands)?;
        match result(raw_opcode)? {
            Opcode::Add  => try_map_operands!( operands => Add { dr, sr1, sr2_or_imm5 } ),
            Opcode::And  => try_map_operands!( operands => And { dr, sr1, sr2_or_imm5 } ),
            Opcode::Br(cond_codes) => {
                let mut os = operands.into_iter();
                let pc_offset9 = try_map(os.next())?;
                Ok(Instruction::Br { cond_codes, pc_offset9 })
            }
            Opcode::Jmp  => try_map_operands!( operands => Jmp { base }),
            Opcode::Jsr  => try_map_operands!( operands => Jsr { pc_offset11 }),
            Opcode::Jsrr => try_map_operands!( operands => Jsrr { base }),
            Opcode::Ld   => try_map_operands!( operands => Ld { dr, pc_offset9 }),
            Opcode::Ldi  => try_map_operands!( operands => Ldi { dr, pc_offset9 }),
            Opcode::Ldr  => try_map_operands!( operands => Ldr { dr, base, offset6 }),
            Opcode::Lea  => try_map_operands!( operands => Lea { dr, pc_offset9 }),
            Opcode::Not  => try_map_operands!( operands => Not { dr, sr }),
            Opcode::Ret  => Ok(Instruction::Ret),
            Opcode::Rti  => Ok(Instruction::Rti),
            Opcode::St   => try_map_operands!( operands => St { sr, pc_offset9 }),
            Opcode::Sti  => try_map_operands!( operands => Sti { sr, pc_offset9 }),
            Opcode::Str  => try_map_operands!( operands => Str { sr, base, offset6 }),
            Opcode::Trap => try_map_operands!( operands => Trap { trap_vec }),

            // TODO: improve error
            Opcode::Orig => Err(()),

            Opcode::Fill => try_map_operands!( operands => Fill { value }),
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

            Opcode::Getc  => Ok(Instruction::new_trap(0x20)),
            Opcode::Out   => Ok(Instruction::new_trap(0x21)),
            Opcode::Puts  => Ok(Instruction::new_trap(0x22)),
            Opcode::In    => Ok(Instruction::new_trap(0x23)),
            Opcode::Putsp => Ok(Instruction::new_trap(0x24)),
            Opcode::Halt  => Ok(Instruction::new_trap(0x25)),
        }
    }
}

impl TryFrom<parse::Instruction> for ObjectWord {
    type Error = ();

    fn try_from(value: parse::Instruction) -> Result<Self, Self::Error> {
        (value.opcode, value.operands).try_into()
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


impl From<lc3_isa::Instruction> for AssemblyResult {
    fn from(i: lc3_isa::Instruction) -> Self {
        AssemblyResult::SingleObjectWord(ObjectWord::Value(i.into()))
    }
}


pub(crate) fn assemble_instruction(symbol_table: &SymbolTable, location_counter: &Addr, instruction: Instruction) -> Result<AssemblyResult, TryFromIntError> {
    use AssemblyResult::*;
    use ObjectWord::*;

    macro_rules! assemble_pc_offset {
        ($pc_offset:ident => $new_i:ident, $instr:ident { $($field:ident),*$(,)* } )
        =>
        {
            match $pc_offset {
                PcOffset::Number(sw) => lc3_isa::Instruction::$new_i($($field,)* sw).into(),
                PcOffset::Label(label) =>
                    match symbol_table.get(&label) {
                        Some(addr) => {
                            let offset = calculate_addr_offset(location_counter, addr)?;
                            lc3_isa::Instruction::$new_i($($field,)* offset).into()
                        }
                        None => SingleObjectWord(UnlinkedInstruction(Instruction::$instr { $($field,)* $pc_offset: PcOffset::Label(label)})),
                    }
            }
        }
    }

    let res = match instruction {
        Instruction::Add { dr, sr1, sr2_or_imm5 } =>
            match sr2_or_imm5 {
                Sr2OrImm5::Sr2(sr2) => lc3_isa::Instruction::new_add_reg(dr, sr1, sr2),
                Sr2OrImm5::Imm5(imm5) => lc3_isa::Instruction::new_add_imm(dr, sr1, imm5),
            }.into(),
        Instruction::And { dr, sr1, sr2_or_imm5 } =>
            match sr2_or_imm5 {
                Sr2OrImm5::Sr2(sr2) => lc3_isa::Instruction::new_and_reg(dr, sr1, sr2),
                Sr2OrImm5::Imm5(imm5) => lc3_isa::Instruction::new_and_imm(dr, sr1, imm5),
            }.into(),
        Instruction::Br { cond_codes: ConditionCodes { n, z, p }, pc_offset9 } => {
            match pc_offset9 {
                PcOffset::Number(sw) => lc3_isa::Instruction::new_br(n, z, p, sw).into(),
                PcOffset::Label(label) =>
                    match symbol_table.get(&label) {
                        Some(addr) => {
                            let offset = calculate_addr_offset(location_counter, addr)?;
                            lc3_isa::Instruction::new_br(n, z, p, offset).into()
                        }
                        None => SingleObjectWord(UnlinkedInstruction(Instruction::Br { cond_codes: ConditionCodes { n, z, p }, pc_offset9: PcOffset::Label(label) })),
                    }
            }
        }
        Instruction::Jmp { base } => lc3_isa::Instruction::new_jmp(base).into(),
        Instruction::Jsr { pc_offset11 } => assemble_pc_offset!(pc_offset11 => new_jsr, Jsr {}),
        Instruction::Jsrr { base } => lc3_isa::Instruction::new_jsrr(base).into(),
        Instruction::Ld { dr, pc_offset9 } => assemble_pc_offset!(pc_offset9 => new_ld, Ld { dr, }),
        Instruction::Ldi { dr, pc_offset9 } => assemble_pc_offset!(pc_offset9 => new_ldi, Ldi { dr, }),
        Instruction::Ldr { dr, base, offset6 } => lc3_isa::Instruction::new_ldr(dr, base, offset6).into(),
        Instruction::Lea { dr, pc_offset9 } => assemble_pc_offset!(pc_offset9 => new_lea, Lea { dr, }),
        Instruction::Not { dr, sr } => lc3_isa::Instruction::new_not(dr, sr).into(),
        Instruction::Ret => lc3_isa::Instruction::new_ret().into(),
        Instruction::Rti => lc3_isa::Instruction::new_rti().into(),
        Instruction::St { sr, pc_offset9 } => assemble_pc_offset!(pc_offset9 => new_st, St { sr, }),
        Instruction::Sti { sr, pc_offset9 } => assemble_pc_offset!(pc_offset9 => new_sti, Sti { sr, }),
        Instruction::Str { sr, base, offset6 } => lc3_isa::Instruction::new_str(sr, base, offset6).into(),
        Instruction::Trap { trap_vec } => lc3_isa::Instruction::new_trap(trap_vec).into(),

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

type ParserInstructions = Vec<WithErrData<parse::Instruction>>;

fn first_pass(program_block_data: impl IntoIterator<Item=(Addr, ParserInstructions)>) -> Result<(Vec<FirstPassBlock>, SymbolTable), ()> {
    let mut fp_blocks = Vec::new();
    let mut symbol_table = HashMap::new();

    for (origin, parser_instructions) in program_block_data {
        let mut instructions = Vec::new();
        let mut location_counter = origin;

        for raw_instruction in parser_instructions.into_iter() {
            let parser_instruction = result(raw_instruction)?;
            if let Some(l) = parser_instruction.label {
                symbol_table.insert(result(l)?, location_counter);
            };

            let instruction: Instruction = (parser_instruction.opcode, parser_instruction.operands).try_into()?;
            let addresses_used = instruction.addresses_occupied();
            instructions.push(instruction);

            location_counter += addresses_used;
        }

        fp_blocks.push(FirstPassBlock { origin, instructions });
    }

    Ok((fp_blocks, symbol_table))
}

fn second_pass_one_block(symbol_table: &SymbolTable, fp_block: FirstPassBlock) -> Result<ObjectBlock, TryFromIntError> {
    let FirstPassBlock { origin, instructions } = fp_block;

    let mut words = Vec::new();
    let mut location_counter = origin;

    for instruction in instructions.into_iter() {
        let addresses_used = instruction.addresses_occupied();
        match assemble_instruction(&symbol_table, &location_counter, instruction)? {
            AssemblyResult::SingleObjectWord(wd) => { words.push(wd); }
            AssemblyResult::MultipleObjectWords(wds) => { words.extend(wds); }
        }
        location_counter += addresses_used;
    }

    Ok(ObjectBlock { origin, words })
}

fn second_pass(symbol_table: SymbolTable, fp_blocks: Vec<FirstPassBlock>) -> Result<Object, TryFromIntError> {
    let blocks =
        fp_blocks.into_iter()
            .map(|fp_block| second_pass_one_block(&symbol_table, fp_block))
            .collect::<Result<Vec<ObjectBlock>, TryFromIntError>>()?;

    Ok(Object { symbol_table, blocks })
}

pub(crate) fn get_orig(orig_operands: WithErrData<Vec<WithErrData<Operand>>>) -> Result<Addr, ()> {
    let orig_operand = result(orig_operands)?.remove(0);
    result(orig_operand)?.try_into()
}

pub fn assemble(file: parse::File) -> Result<Object, ()> {
    let block_data =
        file.blocks.into_iter()
            .map(|p| {
                let parse::ProgramBlock { orig, instructions } = result(p)?;
                let origin = get_orig(orig)?;
                Ok((origin, instructions))
            })
            .collect::<Result<Vec<(Addr, ParserInstructions)>, ()>>()?;

    let (fp_blocks, symbol_table) = first_pass(block_data)?;

    second_pass(symbol_table, fp_blocks).map_err(|_| ())
}