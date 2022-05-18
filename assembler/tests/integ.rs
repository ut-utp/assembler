extern crate lc3_assembler;

use lc3_isa::{ADDR_MAX_VAL, Word};
use std::ops::Index;
use lc3_isa::util::MemoryDump;
use lc3_assembler::{assembler, lexer, linker, parser, LeniencyLevel};

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

mod single_instruction {
    use super::*;

    fn single_instruction_multiple_output_test(input: &str, expected: &[Word]) {
        let input = format!(".ORIG x3000\n{}\n.END", input);
        test(input.as_str(), 0x3000, expected);
    }

    fn single_instruction_test(input: &str, expected: Word) {
        single_instruction_multiple_output_test(input, &[expected]);
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

    single_instruction_tests! { alternative_styles
        lowercase:       "add r0 r0 r0"   => 0x1000,
        comma_separated: "add r0, r0, r0" => 0x1000,
        with_semicolon:  "ADD R0 R0 R0;"  => 0x1000,
        nonpatt_hex_imm: "ADD R7 R7 0xA"  => 0x1FEA,
        commented:       "ADD R0 R0 R0 ; comment" => 0x1000,
    }

    single_instruction_tests! { add
        minimal:     "ADD R0 R0 R0"  => 0x1000,
        r1_2_3:      "ADD R1 R2 R3"  => 0x1283,
        r4_5_6:      "ADD R4 R5 R6"  => 0x1946,
        r7_imm:      "ADD R7 R7 #0"  => 0x1FE0,
        nonzero_imm: "ADD R7 R7 #1"  => 0x1FE1,
        max_imm:     "ADD R7 R7 #15" => 0x1FEF,
        neg_imm:     "ADD R7 R7 #-1" => 0x1FFF,
        hex_imm:     "ADD R7 R7 xA"  => 0x1FEA,
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

    single_instruction_tests! { br
        minimal: "BR #0"     => 0x0E00,
        n:       "BRn #0"    => 0x0800,
        z:       "BRz #0"    => 0x0400,
        p:       "BRp #0"    => 0x0200,
        nz:      "BRnz #0"   => 0x0C00,
        np:      "BRnp #0"   => 0x0A00,
        zp:      "BRzp #0"   => 0x0600,
        nzp:     "BRnzp #0"  => 0x0E00,
        neg_imm: "BRnzp #-1" => 0x0FFF,
        pos_imm: "BRnzp #1"  => 0x0E01,
        max_imm: "BRn #255"  => 0x08FF,
        min_imm: "BRz #-256" => 0x0500,
    }

    // TODO: make this more readable :(
    // I couldn't find a way to rearrange the macros to create one
    // for the boilerplate like "($opcode << 12) + ".
    // Consider adding a variant in single_instruction_tests for this case?
    macro_rules! reg_and_pcoffset9_instruction_tests {
        (
            $(
                $name:ident: $operator:expr => $opcode:expr
            ),+
            $(,)*
        ) => {
            $(
                single_instruction_tests! { $name
                    //                                OPERANDS                                    RESULT
                    //                                --------                                    -----
                    minimal: ($operator.to_string() + " R0 #0").as_str()    => (($opcode << 12) + 0x000),
                    pos_imm: ($operator.to_string() + " R1 #1").as_str()    => (($opcode << 12) + 0x201),
                    neg_imm: ($operator.to_string() + " R2 #-1").as_str()   => (($opcode << 12) + 0x5FF),
                    max_imm: ($operator.to_string() + " R3 #255").as_str()  => (($opcode << 12) + 0x6FF),
                    min_imm: ($operator.to_string() + " R4 #-256").as_str() => (($opcode << 12) + 0x900),
                    // hex_imm: ($operator.to_string() + " R5 xA").as_str()    => (($opcode << 12) + 0xA0A), TODO: We currently assume an argument not starting in # is a label. Allow hex literals?
                    r5:      ($operator.to_string() + " R5 #0").as_str()    => (($opcode << 12) + 0xA00),
                    r6:      ($operator.to_string() + " R6 #0").as_str()    => (($opcode << 12) + 0xC00),
                    r7:      ($operator.to_string() + " R7 #0").as_str()    => (($opcode << 12) + 0xE00),
                }
            )+
        };
    }

    reg_and_pcoffset9_instruction_tests! {
        ld:  "LD"  => 0x2,
        ldi: "LDI" => 0xA,
        lea: "LEA" => 0xE,
        st:  "ST"  => 0x3,
        sti: "STI" => 0xB,
    }

    single_instruction_tests! { jsr
        minimal: "JSR #0"     => 0x4800,
        pos_imm: "JSR #1"     => 0x4801,
        neg_imm: "JSR #-1"    => 0x4FFF,
        max_imm: "JSR #1023"  => 0x4BFF,
        min_imm: "JSR #-1024" => 0x4C00,
        // hex_imm: "JSR xA"     => 0x480A, // TODO: We currently assume an argument not starting in # is a label. Allow hex literals?
    }

    mod pseudo_ops {
        use super::*;

        single_instruction_tests! { fill
            minimal:     ".FILL #0"     => 0x0000,
            pos_imm:     ".FILL #1"     => 0x0001,
            max_imm:     ".FILL #65535" => 0xFFFF,
            hex_imm:     ".FILL xA"     => 0x000A,
            hex_imm2:    ".FILL xBEEF"  => 0xBEEF,
            max_hex_imm: ".FILL xFFFF"  => 0xFFFF,
        }

        macro_rules! single_instruction_multiple_output_tests {
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
                            single_instruction_multiple_output_test($instruction, $expected);
                        }
                    )+
                }
            };
        }

        single_instruction_multiple_output_tests! { blkw
            one: ".BLKW 1"  => &[0,],
            two: ".BLKW 2"  => &[0, 0,],
            ten: ".BLKW 10" => &[0, 0, 0, 0, 0, 0, 0, 0, 0, 0,],
        }

        single_instruction_multiple_output_tests! { stringz
            a:            ".STRINGZ \"a\""             => &[0x61, 0x00],
            double_quote: ".STRINGZ \"\\\"\""          => &[0x22, 0x00],
            backslash:    ".STRINGZ \"\\\\\""          => &[0x5C, 0x00],
            hello_world:  ".STRINGZ \"Hello, World!\"" => &[0x48, 0x65, 0x6C, 0x6C, 0x6F, 0x2C, 0x20, 0x57, 0x6F, 0x72, 0x6C, 0x64, 0x21, 0x00],
        }
    }
}

fn test(input: &str, orig: usize, expected_mem: &[Word]) {
    let (maybe_tokens, lex_errs) = lexer::lex(input, LeniencyLevel::Lenient);
    let tokens = maybe_tokens.expect("lexing failed");

    let (maybe_file, parse_errs) = parser::parse(input, tokens, LeniencyLevel::Lenient);
    let (mut file, span) = maybe_file.expect("parsing failed");
    assert_eq!(1, file.len(), "parsed unexpected number of programs: {}", file.len());
    let program = file.remove(0).0.expect("parse error in program");
    let object = assembler::assemble(program);

    let mem = linker::link([object], None);

    for i in 0..orig {
        assert_mem(&mem, i, 0x0000);
    }
    for i in 0..expected_mem.len() {
        assert_mem(&mem, orig + i, expected_mem[i]);
    }
    for i in (orig + expected_mem.len())..(ADDR_MAX_VAL as usize) {
        assert_mem(&mem, i, 0x0000);
    }
}

fn assert_mem(mem: &MemoryDump, location: usize, expected: Word) {
    let actual = mem[location];
    assert_eq!(expected, actual, "differed at {:#x}: expected {:#x}, was {:#x}", location, expected, actual);
}
