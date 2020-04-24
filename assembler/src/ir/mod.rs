/// The series of intermediate representations (IRs)
/// of the parse tree necessary to provide as many
/// good errors as possible while assembling.

/// This pass separates the tokens by newline
/// and separates comments.
pub mod ir1_parse_lines;

/// This pass checks that the sequence of tokens on each line is valid.
pub mod ir2_parse_line_syntax;

/// This pass checks that the sequence of lines is valid
/// and groups them into separate objects if there are multiple .ORIG/.ENDs.
pub mod ir3_parse_objects;

/// This pass disambiguates and validates ambiguous immediate operands and labels.
pub mod ir4_parse_ambiguous_tokens;

/// This pass expands each pseudo-op which fills memory into the appropriate list of values.
pub mod ir5_expand_pseudo_ops;
