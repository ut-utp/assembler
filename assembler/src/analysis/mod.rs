/// These modules provide functions that analyze fully-parsed syntax trees
/// and related data structures.

/// Analyzes whether objects can/will be placed in valid memory locations without overlap.
pub mod memory_placement;

/// Creates a structure to store the locations of labels.
/// Used for later computing offsets from label operands.
pub mod symbol_table;

/// Extracts all errors from the parsed structs.
pub mod extract_errors;

/// Determines whether or not a parsed struct can be successfully assembled.
pub mod validate;