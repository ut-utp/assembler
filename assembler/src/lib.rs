
// TODO: docs
// TODO: denys
// TODO: docs URL

mod error;

pub mod parser;

mod lexer;
mod ir1_simple_lines;
mod ir2_lines;
mod ir3_unvalidated_objects;
mod cst;
mod expanded;

#[cfg(test)]
mod tests {
    use super::*;
    use lexer::Lexer;

}
