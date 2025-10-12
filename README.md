# coco

why? because i wanted to learn about compilers and ocaml

## Quick start

Prerequisites:

- OCaml (recommended 4.08+)
- Dune (build system)
- A system assembler/linker (clang/ld or gcc) able to assemble the generated .s files. On macOS the system toolchain is used by the tutorial harness.

Build the compiler:

```sh
dune build
```

Run the compiler binary on a C file to generate an assembly file:

```sh
dune exec ./bin/main.exe -- path/to/input.c
# This will print the path to the generated .s file (in the same directory as the C file).
```

Assemble and link the generated assembly yourself (optional):

```sh
# assemble and link with clang
clang -o output path/to/generated.s
# then run
./output
```

## Notes and troubleshooting

- Assembler dialect and externs
  - The generator emits NASM-style directives (`section`, `dq`, `resq`) and uses explicit `extern` for undefined functions and `global` for symbols the compiler defines (for example `_main`). The test harness expects this form on macOS. If you see assembler/linker errors about undefined symbols (for example `_putchar`), ensure the generated assembly includes an `extern _putchar` line.

- macOS specifics
  - On macOS the assembler and linker can be more strict about relocations. This project uses RIP-relative references for global memory accesses where necessary to avoid text-relocation errors. If you change the generator, be careful to keep consistent RIP-relative addressing (or use the system assembler's accepted syntax).

- Debugging validation errors
  - If the validator reports errors like "Variable declared twice in same scope" or "Undeclared variable", check the corresponding C input and the AST printed by the compiler. The validator enforces declaration-before-use rules and proper block scoping.

## References

- [Nora Sandler's Write-a-Compiler series](https://norasandler.com/2017/11/29/Write-a-Compiler.html)
- [Tour of OCaml](https://ocaml.org/docs/tour-of-ocaml)
- [15-150](https://brandonspark.github.io/150/)
