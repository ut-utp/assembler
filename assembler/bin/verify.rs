extern crate lc3_assembler;

use std::{env, fs};
use std::path::Path;
use lc3_assembler::lexer::Lexer;
use lc3_assembler::parser::parse;

fn main() {
    let args = env::args().collect::<Vec<_>>();
    for arg in args[1..].iter() {
        let path = Path::new(arg.as_str());
        assert!(path.is_file());

        let string = fs::read_to_string(path).unwrap();
        let lexer = Lexer::new(string.as_str());
        let cst = parse(lexer);


    }
}
