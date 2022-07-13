use std::{collections::HashMap, vec, ops::Range};

use itertools::Itertools;
use lc3_assembler::{analyze::Visit, SpanWithSource};
use tower_lsp::lsp_types::{SemanticTokensLegend, SemanticTokenType};

type Span = Range<usize>;
type Spanned<T> = (T, Span);

#[derive(Copy, Clone, Debug)]
pub enum SemanticToken {
    LabelDeclaration,
    Opcode,
    LabelReference,
    RegisterReference,
    NumberLiteral,
    StringLiteral,
    Comment,
}

#[derive(Copy, Clone, Debug)]
enum SemanticTokenModifier {
    Declaration = 0
}

impl SemanticTokenModifier {
    const types: [tower_lsp::lsp_types::SemanticTokenModifier; 1] = 
        [
            tower_lsp::lsp_types::SemanticTokenModifier::DECLARATION
        ];

    fn mask(&self) -> u32 {
        1 << (*self as u32)
    }
}

pub fn legend() -> SemanticTokensLegend {
    let token_types = 
        SemanticToken::types.iter()
            .map(|(t, _)| t.clone()) 
            .collect::<Vec<_>>();
    let token_modifiers =
        SemanticTokenModifier::types.iter()
            .map(|t| t.clone()) 
            .collect::<Vec<_>>();

    SemanticTokensLegend { token_types, token_modifiers }
}

impl SemanticToken {
    const types: [(SemanticTokenType, &'static [SemanticTokenModifier]); 7] = 
        [
            (SemanticTokenType::new("label"), &[SemanticTokenModifier::Declaration]),
            (SemanticTokenType::KEYWORD,      &[]),
            (SemanticTokenType::new("label"), &[]), // Label is standard, but not supported by Tower
            (SemanticTokenType::VARIABLE,     &[]),
            (SemanticTokenType::NUMBER,       &[]),
            (SemanticTokenType::STRING,       &[]),
            (SemanticTokenType::COMMENT,      &[]),
        ];


    fn type_(&self) -> (u32, u32) {
        let (_, ms) = Self::types[*self as usize];
        let modifier_bitmap = ms.iter().fold(0, |bitmap, m| bitmap | m.mask());
        (*self as u32, modifier_bitmap)
    }
}

pub fn simple_semantic_tokens(spanned_file: &Spanned<lc3_assembler::parse::File>) -> Vec<Spanned<SemanticToken>> {
    let (file, span) = spanned_file;
    let span_with_src = SpanWithSource::with_dummy_source(span.clone());
    let (simple_tokens, _) = lc3_assembler::analyze::visit::<SemanticTokenAnalysis, _, _>((), file, &span_with_src);
    simple_tokens
}

pub fn semantic_tokens(simple_tokens: &Vec<Spanned<SemanticToken>>, rope: &ropey::Rope)
    -> Option<Vec<tower_lsp::lsp_types::SemanticToken>>
{
    let mut prev_line = 0;
    let mut prev_start = 0;
    let semantic_tokens = simple_tokens.iter()
        .filter_map(|(t, span)| {
            let line = rope.try_byte_to_line(span.start as usize).ok()? as u32;
            let first = rope.try_line_to_char(line as usize).ok()? as u32;
            let start = rope.try_byte_to_char(span.start as usize).ok()? as u32 - first;
            let (token_type, token_modifiers_bitset) = t.type_();
            let ret = Some(tower_lsp::lsp_types::SemanticToken {
                delta_line: line - prev_line,
                delta_start:
                    if start >= prev_start {
                        start - prev_start
                    } else {
                        start
                    },
                length: span.len() as u32,
                token_type,
                token_modifiers_bitset,
            });
            prev_line = line;
            prev_start = start;
            ret
        })
        .collect::<Vec<_>>();
    Some(semantic_tokens)
}

#[derive(Default)]
struct SemanticTokenAnalysis {
    tokens: Vec<Spanned<SemanticToken>>
}

impl SemanticTokenAnalysis {
    fn push_token(&mut self, t: SemanticToken, span: &SpanWithSource) {
        self.tokens.push((t, span.span.clone()))
    }
}

impl Visit for SemanticTokenAnalysis {
    type Data = ();

    fn new(data: Self::Data) -> Self { Default::default() }

    type Output = Vec<Spanned<SemanticToken>>;

    fn finish(self) -> (Self::Output, Vec<lc3_assembler::error::Error>) {
        (self.tokens, vec![])
    }

    fn enter_label(&mut self, _label: &String, span: &SpanWithSource, _location: &lc3_assembler::analyze::LocationCounter) {
        self.push_token(SemanticToken::LabelDeclaration, span);
    }

    fn enter_opcode(&mut self, _opcode: &lc3_assembler::lex::Opcode, span: &SpanWithSource, _location: &lc3_assembler::analyze::LocationCounter) {
        self.push_token(SemanticToken::Opcode, span);
    }

    fn enter_operand(&mut self, operand: &lc3_assembler::parse::Operand, span: &SpanWithSource, _location: &lc3_assembler::analyze::LocationCounter) {
        use lc3_assembler::parse::Operand::*;
        let token = match operand {
            Register(_) 
                => SemanticToken::RegisterReference,
            UnqualifiedNumberLiteral(_) 
            | NumberLiteral(_) 
                => SemanticToken::NumberLiteral,
            StringLiteral(_) 
                => SemanticToken::StringLiteral,
            Label(_) 
                => SemanticToken::LabelReference,
        };
        self.push_token(token, span);
    }
}
