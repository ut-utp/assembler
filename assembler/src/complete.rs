/// In my hubris, I thought the CST was complete enough.
/// As it turns out, it was nowhere near.
///
/// This module is an attempt to rectify my error in one behemoth structure.
/// When `complete` is complete, it will replace `cst` in name and the latter
/// will become `ir4_validated_objects`.
///
/// The main difference here is that `complete` will store as much data as possible
/// relating to the source *and* what it will be assembled to.
/// This will allow querying for the source assembled to a memory location,
/// the addresses corresponding to labels, and whatever is required in the future
/// to provide a nice development environment.
///
/// `cst` previously stopped where all errors could be represented as part of the tree.
/// `complete` will continue by assembling as much as possible and bringing that data in.
pub struct Program {

}


