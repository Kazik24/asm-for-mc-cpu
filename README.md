# asm for mc cpu

Simple toy assembly to machine code compiler and cpu emulator.
___
This compiler is designed to work from command line, take single text file
and produce binary or hexadecimal output.

[Assembly Language Specification](doc/assembly.md)

[Binary Instruction Representation](doc/binary_opcodes.md)

### Build / Installation
Building the package
```sh
$ git clone https://github.com/Kazik24/asm-for-mc-cpu.git
$ cd asm-for-mc-cpu
$ cargo build
```
Installing as cargo module
```sh
$ cargo install --git https://github.com/Kazik24/asm-for-mc-cpu.git
```
