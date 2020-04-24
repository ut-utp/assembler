/// `complete` will store as much data as possible
/// relating to the source *and* what it will be assembled to.
/// This will allow querying for the source assembled to a memory location,
/// the addresses corresponding to labels, and whatever is required in the future
/// to provide a nice development environment.


pub struct Program {
    pub objects: Vec<Object>
}

pub struct Object {

}

