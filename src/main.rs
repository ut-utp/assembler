mod ast;
mod lexer;
mod error;
mod lc3;
mod parser;

fn main() {
    println!("Hello, world!");
}

#[cfg(test)]
mod tests {
    use super::*;
    use lexer::Lexer;

    #[test]
    fn test_reg() {
        let input = "R1";
        let lexer = Lexer::new(input);
        let mut errors = Vec::new();
        let reg = lc3::RegParser::new()
            .parse(input, &mut errors, lexer)
            .unwrap();
        assert!(
            match reg.reg {
                Ok(lc3_isa::Reg::R1) => true,
                _ => false,
            }
        );
        assert_eq!(reg.src, "R1");
    }
    
    #[test]
    fn test_immediate() {
        let input = "x3000";
        let lexer = Lexer::new(input);
        let mut errors = Vec::new();
        let addr_imm = lc3::AddrImmediateParser::new()
            .parse(input, &mut errors, lexer)
            .unwrap();
        assert!(
            match addr_imm.value {
                Ok(0x3000) => true,
                _ => false,
            }
        );
        assert_eq!(addr_imm.src, "x3000");
    }
}