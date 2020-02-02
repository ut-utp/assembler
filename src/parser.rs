use crate::lexer::{Lexer, TokenType::*, Token, Op, Opcode, PseudoOp::*};
use crate::ast::{Object, File, Operation, Operands};
use itertools::{Itertools, PeekingTakeWhile};
use crate::error::ParseError;
use std::iter::Peekable;


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
        // Get first object.
        // We expect at least one object per file (otherwise, why would you try and assemble it?!).
        self.skip_to_orig();
        let object = self.parse_object();
        self.file.objects.push(object);

        // Get remaining objects
        // Remove this loop if we want one object per file.
        loop {
            self.skip_to_orig();
            if self.tokens.peek().is_none() {
                break;
            }
            let object = self.parse_object();
            self.file.objects.push(object);
            if self.tokens.peek().is_none() {
                break;
            }
        }

        &self.file
    }


    fn parse_object(&mut self) -> Object<'input> {
        let orig = self.parse_orig();
        let mut object = Object::new(orig);
        
        while self.tokens.peek().is_some() {
            self.skip_line_breaks();
            let operation = self.parse_operation();
            object.instructions.push(operation);
            if let Operands::End = operation.operands {
                break;
            }
        }
        
        object
    }
    
    fn parse_orig(&mut self) -> Operation<'input> {
        let orig = match self.tokens.next() {
            Some(Token { ty: Op(Op::PseudoOp(Orig)), .. }) => {

            }
            _ =>
        }
    }
    
    fn parse_operation(&mut self) -> Operation<'input> {
        let label = match self.tokens.peek() {
            Some(Token { ty: Word, .. }) => {
                let label = self.tokens.next().unwrap();
                self.skip_line_breaks();
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

    fn skip_to_orig(&mut self) {
        let skipped = self.tokens.peeking_take_while(|token| token.ty != Op(Op::PseudoOp(Op::Orig)));
        self.file.ignored.extend(skipped);
    }

    fn skip_whitespace(&mut self) {
        let whitespace = self.tokens.peeking_take_while(|token| token.ty == Whitespace);
        self.file.ignored.extend(whitespace);
    }
    
    fn skip_line_breaks(&mut self) {
        let whitespace = self.tokens.peeking_take_while(|token| match token.ty {
            Whitespace | Comment | Newline => true,
            _ => false,
        });
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