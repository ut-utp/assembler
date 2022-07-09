## Undergraduate Teaching Platform: Tooling! 👷

[![Build Status](https://img.shields.io/endpoint.svg?url=https%3A%2F%2Factions-badge.atrox.dev%2Fut-utp%2Fassembler%2Fbadge&style=for-the-badge)](https://github.com/ut-utp/assembler/actions) [![License: MPL-2.0](https://img.shields.io/github/license/ut-utp/assembler?color=orange&style=for-the-badge)](https://opensource.org/licenses/MPL-2.0)
--
[![](https://tokei.rs/b1/github/ut-utp/assembler)](https://github.com/ut-utp/assembler) [![codecov](https://codecov.io/gh/ut-utp/assembler/branch/master/graph/badge.svg)](https://codecov.io/gh/ut-utp/assembler)

Where the _fancy_ tools live.

🚧 🚧 This is very much not stable yet! 🚧 🚧

### Assembler

An LC-3 assembler designed with a focus on helpful feedback.

See [`./assembler/README.md`](assembler/README.md).

### Language Server

A server which uses the Language Server Protocol (LSP) to provide
tools for analyzing and assembling LC-3 assembly code to text editors
supporting LSP.



### Language Client

A Visual Studio Code (VSCode) extension and client for the Language Server.

#### Prerequisites

- [Install `pnpm`.](https://pnpm.io/installation#using-npm)
- [Install `cargo`.](https://rustup.rs/)

#### Building

Starting in the top level of the repo:

```sh
pnpm i
```

Then build the language server:
```sh
cd language_server
cargo build
```

#### Testing

Open the repo folder in VSCode and launch using "Start Debugging" (<kbd>F5</kbd>).