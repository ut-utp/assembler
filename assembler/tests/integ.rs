extern crate lc3_assembler;

use lc3_assembler::lexer::Lexer;
use lc3_assembler::parser::parse;
use lc3_assembler::assembler::assemble;
use lc3_isa::Word;
use lc3_assembler::parser::LeniencyLevel::Lenient;


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


fn test(input: &str, orig: usize, expected_mem: &[Word]) {
    let lexer = Lexer::new(input);
    let cst = parse(lexer, Lenient);

    let mem = assemble(cst.objects);
    for i in 0..orig {
        assert_eq!(0x0000, mem[i], "differed at {:#x}", i)
    }
    for i in 0..expected_mem.len() {
        assert_eq!(expected_mem[i], mem[orig + i], "differed at {:#x}", orig + i)
    }
    for i in (orig + expected_mem.len())..0xFFFF {
        assert_eq!(0x0000, mem[i], "differed at {:#x}", i)
    }
}
