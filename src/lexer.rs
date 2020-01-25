use regex::Regex;
use crate::error::LexError;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Tok<'input> {
    // We capture the input for each token with multiple possible matches
    // so we can recreate the input for syntax highlighting,
    // good error messages, etc.
    // TODO: retain whitespace? Would make grammar unnecessarily complex, but may be necessary to fully recreate input
    
    // Opcodes
    // These are captured here to distinguish from labels.
    Add(&'input str),
    And(&'input str),
    Br(&'input str),
    Jmp(&'input str),
    Jsr(&'input str),
    Jsrr(&'input str),
    Ld(&'input str),
    Ldi(&'input str),
    Ldr(&'input str),
    Lea(&'input str),
    Not(&'input str),
    Ret(&'input str),
    Rti(&'input str),
    St(&'input str),
    Sti(&'input str),
    Str(&'input str),
    Trap(&'input str),
    
    // Named TRAPs
    // These are captured here to distinguish from labels.
    Getc(&'input str),
    Out(&'input str),
    Puts(&'input str),
    In(&'input str),
    Putsp(&'input str),
    Halt(&'input str),

    // Pseudo-ops
    // These are captured here to distinguish from labels.
    Orig(&'input str),
    Fill(&'input str),
    Blkw(&'input str),
    Stringz(&'input str),
    End(&'input str),

    // String Literals
    // Numeric literals starting with x can't be disambiguated from labels,
    // so we'll do that later based on position.
    String(&'input str),
    
    // Comments
    Comment(&'input str),
    
    // Punctuation
    Comma,
    Newline,
    
    // Chunk of non-whitespace, non-comma, non-semicolon text.
    // Used as a catch-all for tokens that need to be disambiguated at parse-time,
    // for example, labels and hex literals which may both start with 'x'.
    // In more general terms: labels and operands.
    Word(&'input str),
}



pub struct Lexer<'input> {
    src: &'input str,
    patterns: Vec<(Regex, Box<dyn Fn(&str) -> Tok>)>,
    skips: Regex, // TODO: Probably change to associated const (const SKIPS = Regex::new("[ \t]+")) when compile-time calculations available
    cur_pos: usize,
}

impl<'input> Lexer<'input> {
    
    // all non-newline whitespace (1 or more that are not [not-whitespace or CR or LF])
    const NON_NEWLINE_WHITESPACE: &'static str = r"^[^\S\r\n]+";

    pub fn new(src: &'input str) -> Lexer<'input> {
        let mut this = Lexer {
            src,
            patterns: Vec::new(),
            skips: Regex::new(Self::NON_NEWLINE_WHITESPACE).unwrap(),
            cur_pos: 0,
        };

        // TODO: Maybe extract these constants into some table or with some macro. The repetition is killing me.
        // The lexer tries to find these patterns in this order.
        // Registering a pattern will automatically append some stuff to the regex.
        // Notably, it will add ^ to the beginning to ensure that it grabs tokens
        // from the beginning of the slice it's examining, so don't use ^.
        this.register_pattern(r"ADD",  |s| Tok::Add(s));
        this.register_pattern(r"AND",  |s| Tok::And(s));
        this.register_pattern(r"BR",   |s| Tok::Br(s));
        this.register_pattern(r"JMP",  |s| Tok::Jmp(s));
        this.register_pattern(r"JSR",  |s| Tok::Jsr(s));
        this.register_pattern(r"JSRR", |s| Tok::Jsrr(s));
        this.register_pattern(r"LD",   |s| Tok::Ld(s));
        this.register_pattern(r"LDI",  |s| Tok::Ldi(s));
        this.register_pattern(r"LDR",  |s| Tok::Ldr(s));
        this.register_pattern(r"LEA",  |s| Tok::Lea(s));
        this.register_pattern(r"NOT",  |s| Tok::Not(s));
        this.register_pattern(r"RET",  |s| Tok::Ret(s));
        this.register_pattern(r"RTI",  |s| Tok::Rti(s));
        this.register_pattern(r"ST",   |s| Tok::St(s));
        this.register_pattern(r"STI",  |s| Tok::Sti(s));
        this.register_pattern(r"STR",  |s| Tok::Str(s));
        this.register_pattern(r"TRAP", |s| Tok::Trap(s));
        
        this.register_pattern(r"GETC",  |s| Tok::Getc(s));
        this.register_pattern(r"OUT",   |s| Tok::Out(s));
        this.register_pattern(r"PUTS",  |s| Tok::Puts(s));
        this.register_pattern(r"IN",    |s| Tok::In(s));
        this.register_pattern(r"PUTSP", |s| Tok::Putsp(s));
        this.register_pattern(r"HALT",  |s| Tok::Halt(s));
        
        this.register_pattern(r".ORIG",    |s| Tok::Orig(s));
        this.register_pattern(r".FILL",    |s| Tok::Fill(s));
        this.register_pattern(r".BLKW",    |s| Tok::Blkw(s));
        this.register_pattern(r".STRINGZ", |s| Tok::Stringz(s));
        this.register_pattern(r".END",     |s| Tok::End(s));
        
        this.register_pattern(r#""([^"\\]|\\.)*""#, |s| Tok::String(s)); // quotes with any number of non-quote/backslash chars *or* arbitrary chars escaped with backslashes in between.
        
        this.register_pattern(r";.*", |s| Tok::Comment(s)); // semicolon followed by any number of chars that aren't newlines.
        
        this.register_pattern(r",",            |_| Tok::Comma);
        this.register_pattern(r"(\r\n|\r|\n)", |_| Tok::Newline);
        
        this.register_pattern(r"[^\s,;]+", |s| Tok::Word(s)); // At least one non-whitespace, non-comma, non-semicolon character.
        
        this
    }
    
    fn register_pattern<C>(&mut self, pattern: &str, constructor: C) 
    where
        C: Fn(&str) -> Tok + 'static
    {
        assert!(!pattern.starts_with("^"));
        let pattern = format!("^(?i){}", pattern);
        let regex = Regex::new(pattern.as_str()).expect("Invalid regex");
        let constructor = Box::new(constructor);
        self.patterns.push((regex, constructor))
    }

    fn trim_whitespace(&mut self) {
        let tail = self.tail();
        if let Some(found) = self.skips.find(tail) {
            self.cur_pos += found.as_str().len();
        }
    }

    fn tail(&self) -> &'input str {
        &self.src[self.cur_pos..]
    }

    fn is_finished(&self) -> bool {
        self.src.len() <= self.cur_pos
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Result<(usize, Tok<'input>, usize), ()>;

    fn next(&mut self) -> Option<Self::Item> {
        self.trim_whitespace();

        if self.is_finished() {
            return None;
        }

        let start = self.cur_pos;

        for &(ref pattern, ref constructor) in &self.patterns {
            if let Some(found) = pattern.find(self.tail()) {
                self.cur_pos += found.end();

                let token = constructor(found.as_str());
                return Some(Ok((start, token, self.cur_pos)));
            }
        }

        Some(Err(()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_simple() { // TODO: actually assert some stuff
        let input = ".ORIG x3000\nTEST add R0, R0, R0; Tokenize me, cap'n!\nBRnzp TEST\nHALT\n.END";
        let lexer = Lexer::new(input);
        for item in lexer {
            println!("{:?}", item);
        }
    }
}