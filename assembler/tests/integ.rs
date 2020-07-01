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

mod single_instruction {
    use super::*;

    fn single_instruction_test(input: &str, expected: Word) {
        let input = format!(".ORIG x3000\n{}\n.END", input);
        test(input.as_str(), 0x3000, &[expected]);
    }

    macro_rules! single_instruction_tests {
        ($tests_name:ident
            $(
                $test_name:ident: $instruction:expr => $expected:expr
            ),+
            $(,)*
        ) => {
            mod $tests_name {
                use super::*;

                $(
                    #[test]
                    fn $test_name() {
                        single_instruction_test($instruction, $expected);
                    }
                )+
            }
        };
    }

    single_instruction_tests! { add
        minimal:     "ADD R0 R0 R0"  => 0x1000,
        r1_2_3:      "ADD R1 R2 R3"  => 0x1283,
        r4_5_6:      "ADD R4 R5 R6"  => 0x1946,
        r7_imm:      "ADD R7 R7 #0"  => 0x1FE0,
        nonzero_imm: "ADD R7 R7 #1"  => 0x1FE1,
        max_imm:     "ADD R7 R7 #15" => 0x1FEF,
        neg_imm:     "ADD R7 R7 #-1" => 0x1FFF,
    }

    single_instruction_tests! { and
        minimal:     "AND R0 R0 R0"  => 0x5000,
        r1_2_3:      "AND R1 R2 R3"  => 0x5283,
        r4_5_6:      "AND R4 R5 R6"  => 0x5946,
        r7_imm:      "AND R7 R7 #0"  => 0x5FE0,
        nonzero_imm: "AND R7 R7 #1"  => 0x5FE1,
        max_imm:     "AND R7 R7 #15" => 0x5FEF,
        neg_imm:     "AND R7 R7 #-1" => 0x5FFF,
    }

    single_instruction_tests! { jmp
        r0: "JMP R0" => 0xC000,
        r1: "JMP R1" => 0xC040,
        r2: "JMP R2" => 0xC080,
        r3: "JMP R3" => 0xC0C0,
        r4: "JMP R4" => 0xC100,
        r5: "JMP R5" => 0xC140,
        r6: "JMP R6" => 0xC180,
        r7: "JMP R7" => 0xC1C0,
    }

    single_instruction_tests! { jsrr
        r0: "JSRR R0" => 0x4000,
        r1: "JSRR R1" => 0x4040,
        r2: "JSRR R2" => 0x4080,
        r3: "JSRR R3" => 0x40C0,
        r4: "JSRR R4" => 0x4100,
        r5: "JSRR R5" => 0x4140,
        r6: "JSRR R6" => 0x4180,
        r7: "JSRR R7" => 0x41C0,
    }

    #[test]
    fn rti() {
        single_instruction_test("RTI", 0x8000);
    }

    #[test]
    fn ret() {
        single_instruction_test("RET", 0xC1C0);
    }

    single_instruction_tests! { ldr
        minimal: "LDR R0 R0 #0"   => 0x6000,
        r1_2:    "LDR R1 R2 #3"   => 0x6283,
        max_imm: "LDR R3 R4 #31"  => 0x671F,
        neg_imm: "LDR R5 R6 #-1"  => 0x6BBF,
        min_imm: "LDR R7 R7 #-32" => 0x6FE0,
    }

    single_instruction_tests! { not
        r0_1: "NOT R0 R1" => 0x907F,
        r2_3: "NOT R2 R3" => 0x94FF,
        r4_5: "NOT R4 R5" => 0x997F,
        r6_7: "NOT R6 R7" => 0x9DFF,
    }

    single_instruction_tests! { str
        minimal: "STR R0 R0 #0"   => 0x7000,
        r1_2:    "STR R1 R2 #3"   => 0x7283,
        max_imm: "STR R3 R4 #31"  => 0x771F,
        neg_imm: "STR R5 R6 #-1"  => 0x7BBF,
        min_imm: "STR R7 R7 #-32" => 0x7FE0,
    }

    single_instruction_tests! { trap
        minimal: "TRAP x00" => 0xF000,
        halt:    "TRAP x25" => 0xF025,
        max:     "TRAP xFF" => 0xF0FF,
        decimal: "TRAP #37" => 0xF025,
    }

    single_instruction_tests! { named_traps
        getc:  "GETC"  => 0xF020,
        out:   "OUT"   => 0xF021,
        puts:  "PUTS"  => 0xF022,
        in_:   "IN"    => 0xF023,
        putsp: "PUTSP" => 0xF024,
        halt:  "HALT"  => 0xF025,
    }

// TODO: BR
// TODO: JSR
// TODO: LD
// TODO: LDI
// TODO: ST
// TODO: STI
// TODO: LEA
// TODO: Pseudo-ops

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

fn assert_mem(mem: &MemoryDump, location: usize, expected: Word) {
    let actual = mem[location];
    assert_eq!(expected, actual, "differed at {:#x}: expected {:#x}, was {:#x}", location, expected, actual);
}
