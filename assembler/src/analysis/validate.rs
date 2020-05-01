use crate::error::Error;
use crate::complete::Program;
use crate::analysis::extract_errors::extract_errors;

pub fn validate(program: &Program) -> Result<(), Vec<Error>> {
    let errors = extract_errors(program);
    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}