extern crate lc3_assembler;

use lc3_assembler::lexer::Lexer;
use lc3_assembler::parser::parse;
use lc3_isa::Word;
use lc3_assembler::parser::LeniencyLevel::Lenient;
use std::ops::Index;
use lc3_isa::util::MemoryDump;


#[test]
fn arithmetic_small() {
    test(
        include_str!("inputs/arithmetic_small.asm"),
        0x3000,
        &[
            0x1042,
            0x5705,
            0x9DFF,
            0xF025,
        ]
    );
}

#[test]
fn load_store_medium() {
    test(
        include_str!("inputs/load_store_medium.asm"),
        0x3000,
        &[
            0x21FF,
            0xA3FF,
            0x64FE,
            0xE9FF,
            0x3BFF,
            0xBDFF,
            0x7E3E,
            0xF025,
        ]
    );
}

#[test]
fn pseudo_ops() {
    test(
        include_str!("inputs/pseudo_ops.asm"),
        0x4000,
        &[
            0x0022,
            0x0074,
            0x0068,
            0x0069,
            0x0073,
            0x005C,
            0x0074,
            0x0068,
            0x0061,
            0x0074,
            0x0022,
            0x0000,
            0xBEEF,
        ]
    );
}

#[test]
fn add() {
    single_instruction_tests(&[
        ("ADD R0 R0 R0", 0x1000),
        ("ADD R1 R2 R3", 0x1283),
        ("ADD R4 R5 R6", 0x1946),
        ("ADD R7 R7 #0", 0x1FE0),
        ("ADD R7 R7 #1", 0x1FE1),
        ("ADD R7 R7 #15", 0x1FEF),
        ("ADD R7 R7 #-1", 0x1FFF),
    ]);
}

#[test]
fn and() {
    single_instruction_tests(&[
        ("AND R0 R0 R0", 0x5000),
        ("AND R1 R2 R3", 0x5283),
        ("AND R4 R5 R6", 0x5946),
        ("AND R7 R7 #0", 0x5FE0),
        ("AND R7 R7 #1", 0x5FE1),
        ("AND R7 R7 #15", 0x5FEF),
        ("AND R7 R7 #-1", 0x5FFF),
    ]);
}

#[test]
fn jmp() {
    single_instruction_tests(&[
        ("JMP R0", 0xC000),
        ("JMP R1", 0xC040),
        ("JMP R2", 0xC080),
        ("JMP R3", 0xC0C0),
        ("JMP R4", 0xC100),
        ("JMP R5", 0xC140),
        ("JMP R6", 0xC180),
        ("JMP R7", 0xC1C0),
    ])
}

#[test]
fn jsrr() {
    single_instruction_tests(&[
        ("JSRR R0", 0x4000),
        ("JSRR R1", 0x4040),
        ("JSRR R2", 0x4080),
        ("JSRR R3", 0x40C0),
        ("JSRR R4", 0x4100),
        ("JSRR R5", 0x4140),
        ("JSRR R6", 0x4180),
        ("JSRR R7", 0x41C0),
    ])
}

#[test]
fn rti() {
    single_instruction_test("RTI", 0x8000);
}

#[test]
fn ret() {
    single_instruction_test("RET", 0xC1C0);
}

#[test]
fn ldr() {
    single_instruction_tests(&[
        ("LDR R0 R0 #0", 0x6000),
        ("LDR R1 R2 #3", 0x6283),
        ("LDR R3 R4 #31", 0x671F),
        ("LDR R5 R6 #-1", 0x6BBF),
        ("LDR R7 R7 #-32", 0x6FE0),
    ])
}

#[test]
fn not() {
    single_instruction_tests(&[
        ("NOT R0 R1", 0x907F),
        ("NOT R2 R3", 0x94FF),
        ("NOT R4 R5", 0x997F),
        ("NOT R6 R7", 0x9DFF),
    ])
}

#[test]
fn str() {
    single_instruction_tests(&[
        ("STR R0 R0 #0", 0x7000),
        ("STR R1 R2 #3", 0x7283),
        ("STR R3 R4 #31", 0x771F),
        ("STR R5 R6 #-1", 0x7BBF),
        ("STR R7 R7 #-32", 0x7FE0),
    ])
}

#[test]
fn trap() {
    single_instruction_tests(&[
        ("TRAP x00", 0xF000),
        ("TRAP x25", 0xF025),
        ("TRAP xFF", 0xF0FF),
        ("TRAP #37", 0xF025),
    ])
}

// TODO: BR
// TODO: JSR
// TODO: LD
// TODO: LDI
// TODO: ST
// TODO: STI
// TODO: LEA
// TODO: Named TRAPs
// TODO: Pseudo-ops

fn test(input: &str, orig: usize, expected_mem: &[Word]) {
    let lexer = Lexer::new(input);
    let cst = parse(lexer, Lenient);

    let mem = cst.assemble(None);
    for i in 0..orig {
        assert_mem(&mem, i, 0x0000);
    }
    for i in 0..expected_mem.len() {
        assert_mem(&mem, orig + i, expected_mem[i]);
    }
    for i in (orig + expected_mem.len())..0xFFFF {
        assert_mem(&mem, i, 0x0000);
    }
}

fn single_instruction_tests(tests: &[(&str, Word)]) {
    for (input, expected) in tests {
        single_instruction_test(input, *expected);
    }
}

fn single_instruction_test(input: &str, expected: Word) {
    let input = format!(".ORIG x3000\n{}\n.END", input);
    test(input.as_str(), 0x3000, &[expected]);
}


fn assert_mem(mem: &MemoryDump, location: usize, expected: Word) {
    let actual = mem[location];
    assert_eq!(expected, actual, "differed at {:#x}: expected {:#x}, was {:#x}", location, expected, actual);
}
