mod error;

mod parser;

mod lexer;
mod ir1_simple_lines;
mod ir2_lines;
mod ir3_unvalidated_objects;
mod cst;
mod expanded;

fn main() {
    println!("Hello, world!");
}

#[cfg(test)]
mod tests {
    use super::*;
    use lexer::Lexer;

}