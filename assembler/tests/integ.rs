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
