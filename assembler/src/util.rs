use crate::lexer::Token;
use itertools::Itertools;

pub(crate) fn reconstruct_src<'input>(tokens: impl IntoIterator<Item=Token<'input>>) -> String {
    let mut vec = tokens.into_iter().collect::<Vec<_>>();
    vec.sort_by_key(|token| token.span.0);
    vec.dedup();
    vec.into_iter()
        .map(|token| token.src)
        .join("")
}

