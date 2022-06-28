`id` should be the [`SourceId`] of the source file containing the source `String`.
This can be obtained using [`id`]: `id(&std::path::PathBuf::from("./path/to/source.asm"))`.
If the input is not from a file, the `id` of any path (even `""`) will suffice.
This argument is only to improve error messages by indicating the source file.

