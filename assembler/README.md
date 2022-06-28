### `lc3-assembler` crate

[![](https://github.com/ut-utp/assembler/workflows/assembler/badge.svg)](https://github.com/ut-utp/assembler/actions?query=workflow%3Aassembler)
[![Minimum supported Rust version](https://img.shields.io/badge/rustc-1.56+-red.svg?style=for-the-badge&logo=rust)](#minimum-supported-rust-version-msrv)

A small assembler for a small computer.

#### Example

##### Input

```asm
        .ORIG   x3000
        LD      R0, A0
        LD      R1, A1
        ADD     R2, R0, #R1
        ST      R2, RESULTS
A0      .FILL   #1
A1      .FILL   #2
RESULT  .BLKW   0
        .END
```

##### Error Output
```text
Error: invalid operand
   ╭─[.\example\example.asm:4:25]
   │
 4 │         ADD     R2, R0, #R1
   ·                         ─┬─
   ·                          ╰─── here
───╯
Error: reference to label RESULTS invalid: not previously defined
   ╭─[.\example\example.asm:5:21]
   │
 5 │         ST      R2, RESULTS
   ·                     ───┬───
   ·                        ╰───── here
───╯
Error: assembly failed
```

#### Features
- A command line application for assembling [Little Computer 3 (LC-3)](https://en.wikipedia.org/wiki/Little_Computer_3) assembly programs
- A Rust library for parsing, analyzing, and assembling LC-3 programs

### Command Line Application (CLI)

The CLI is an LC-3 assembler that can be installed and used from your command line.

#### Installation

Run the following in your command line:

(TODO)

#### Usage

Give the CLI a path to a file containing an LC-3 assembly program:

(TODO: command)

If it is valid, the CLI will assemble the program into LC-3 machine code
and store it in a new binary `.mem` file (in this case, `foo.mem`).
You can then use the [UTP TUI](https://github.com/ut-utp/tui/) to load the binary file into an LC-3 emulator and run it.

If the program is invalid, the CLI will instead print error messages indicating what is wrong (to `stderr`).

For more options and usage information, run using the `--help` option:

(TODO: command)

### Library

The library provides Rust functions for assembling LC-3 programs.

These functions are split into modules, most of which encapsulate typical [compiler phases](https://en.wikipedia.org/wiki/Compiler#Three-stage_compiler_structure).
These are intended for use in order:

![A diagram indicating the order of data flow through the main modules: lex, parse, analyze, assemble, link, then layer.](https://raw.githubusercontent.com/ut-utp/assembler/master/assembler/docs/images/main_workflow.png)

Together, the first three modules, `lex`, `parse`, and `analyze`, check that the input is valid LC-3 assembly
and parse it into a data structure (called `parse::File`, or "the syntax tree") which can be more easily assembled.
The last three modules, `assemble`, `link`, and `layer`,  generate the machine code for the program and store it
as an LC-3 memory image.

Each of these modules provides one main public function. You can use them individually,
or use the functions in the top-level module which already combine the steps as shown in the diagram above.

For examples and more detailed information, see the API documentation for each function and module.

#### Design

Our goals when designing the library are, in order of priority:
1. *No "False Negatives"* -- Correctly assemble any valid LC-3 program.
2. *No "False Positives"* -- Reject any input which is not a valid LC-3 program.
3. *Maintainability* -- Provide developer documentation and flexible, debuggable abstractions.
4. *User Experience* -- Provide user documentation and high-quality error messages.
5. *Performance (Speed)* -- Run quickly.
6. *Performance (Memory)* -- Use little memory.

Goals 1 and 2 ensure that assembly behaves as expected and provides a minimum level of feedback.

Goal 3 is to make sure any bugs can be fixed for the foreseeable future; maintenance will likely fall to the TAs
of UT Austin's Electrical and Computer Engineering department, who may change each semester.

Goal 4 is a priority because LC-3 assembly is an educational language.
We want to help students identify and correct their assembly errors in a way that reinforces *why* the errors occurred.
With this support, we hope to help students continue more quickly and confidently to debugging semantic errors.

Goals 5 and 6 aim to make the tool accessible to a wide audience and provide a good experience,
no matter the power of their computers. Of course, assembly is a simple task for almost any PC today,
so these are our lowest priorities.


### Minimum Supported Rust Version (MSRV)

This crate is currently guaranteed to compile on stable Rust 1.56.1 and newer. We offer no guarantees that this will remain true in future releases but do promise to always support (at minimum) the latest stable Rust version and to document changes to the MSRV in the [changelog](CHANGELOG.md).
