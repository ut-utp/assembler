extern crate lc3_assembler;

use std::{env, fs};
use std::path::{Path, PathBuf};
use lc3_assembler::lexer::Lexer;
use lc3_assembler::parser::parse;
use lc3_assembler::assembler::assemble;
use lc3_shims::memory::FileBackedMemoryShim;

const MEM_DUMP_FILE_EXTENSION: &'static str = "mem";

fn main() {
    let args = env::args().collect::<Vec<_>>();
    for arg in args {
        let path = Path::new(arg.as_str());
        assert!(path.is_file());

        let string = fs::read_to_string(path).unwrap();
        let lexer = Lexer::new(string.as_str());
        let cst = parse(lexer);
        let mem = assemble(cst.objects);

        let mut output_path = PathBuf::from(path);
        output_path.set_extension(MEM_DUMP_FILE_EXTENSION);
        let mut file_backed_mem = FileBackedMemoryShim::with_initialized_memory(output_path, mem);
        file_backed_mem.flush().unwrap();
    }
}
