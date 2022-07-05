# `analyze`

## How to add a new analysis pass

In `analyze`, different types of errors are detected by different
syntax tree [visitors](https://en.wikipedia.org/wiki/Visitor_pattern).
To analyze the syntax tree for a new type of error:

1. Implement `Visit` (we'll call the implementor `FooAnalysis`).
   1. Set `Visit::Data` to any data it needs at construct time.
   2. Set `Visit::Output` to any data it outputs besides errors. For example,
      `SymbolTableAnalysis` outputs a (estimated) symbol table.
   3. The `visit` function depth-first traverses the syntax tree, 
      calling the "enter" method as it first reaches each node, then calling the
      "exit" method on each node after all of its children have been visited.
      Override any of these methods to get the data needed for the error analysis.
2. `Visit` is implemented for small tuples of `Visit`. In `validate`,
   add `FooAnalysis` to the tuple type in a call to `visit`, or if it has
   data dependencies, add a new call to visit and pass the data to `visit`.
   - If necessary, extend the call to the `impl_visit_tuple` macro
     to increase the maximum length of tuple `Visit` is implemented for.
   - `Visit::Data` for tuples is just a tuple of the component `Visit`s' `Visit::Data`,
     in the same order as the `Visit`s. The same is true for `Visit::Output`.
3. (If a new call to `visit` was added:) Add the error vector output by `visit` to the
   return value of `validate`.