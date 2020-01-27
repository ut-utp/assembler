use crate::lexer::{Lexer, TokenType, Token, Op, Opcode};
use crate::ast::{Object, File, Operation, Operands};
use itertools::Itertools;
use crate::error::ParseError;
use std::iter::Peekable;
use crate::lexer::PseudoOp;


struct Parser<'input, 'l> {
    errors: Vec<ParseError>,
    tokens: Peekable<&'l mut Lexer<'input>>,
    file: File<'input>,
}

impl<'input, 'l> Parser<'input, 'l> 
{
    pub fn using_lexer(lexer: &'l mut Lexer<'input>) -> Self {
        Self {
            errors: Vec::new(),
            tokens: lexer.peekable(),
            file: File::new(),
        }
    }
    
    pub fn parse(&mut self) -> &File<'input> {
        while self.tokens.peek().is_some() {
            let front_matter = self.tokens.peeking_take_while(|token| token.ty != TokenType::Op(Op::PseudoOp(PseudoOp::Orig)));
            self.file.ignored.extend(front_matter);
            let object = self.parse_object();
            self.file.objects.push(object);
        }

        &self.file
    }
    
    fn parse_object(&mut self) -> Object<'input> {
        let orig = self.parse_orig();
        let mut object = Object::new(orig);
        
        while self.tokens.peek().is_some() {
            let operation = self.parse_operation();
            object.instructions.push(operation);
            if operation.operands == Operands::End { 
                break;
            }
        }
        
        object
    }
    
    fn parse_orig(&mut self) -> Operation<'input> {
        
    }
    
    fn parse_operation(&mut self) -> Operation<'input> {
        self.skip_whitespace();
        let label = match self.tokens.peek() {
            Some(Token { ty: TokenType::Word, .. }) => {
                let label = self.tokens.next().unwrap();
                self.skip_whitespace();
                self.skip_line_break();
                Some(label)
            },
            Some(_) => None,
            None => {
                self.errors.push(ParseError(r#"Hit end of file while looking for next instruction. Suggestion: add a ".END" pseudo-op."#.to_string()));
                None
            },
        };
        self.skip_whitespace();
        
    }
    
    fn skip_whitespace(&mut self) {
        let whitespace = self.tokens.take_while(|token| token.ty == TokenType::Whitespace);
        self.file.ignored.extend(whitespace);
    }
    
    // TODO: Comments
    fn skip_line_break(&mut self) {
        let whitespace = self.tokens.take_while(|token| token.ty == TokenType::Whitespace || token.ty == TokenType::Newline);
        self.file.ignored.extend(whitespace);
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_parse_front_matter() {
        let input = "This is a quick test. .ORIG x3000";
        let mut lexer = Lexer::new(input);
        
        let mut parser = Parser::using_lexer(&mut lexer);
        let file = parser.parse();
        for token in file.ignored {
            println!("{:?}", token);
        }
    }
}