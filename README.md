# coco

why? because i wanted to learn about compilers and ocaml

## What the compiler supports now

- A tiny subset of C:
	- function definitions with return statements
	- integer literal expressions only
	- `int` as the only type
- Multiple functions per file are supported

Example accepted input:

```c
int main() {
		return 2;
}
```

The generator will produce an assembly file (Intel/NASM-like) with a `global _<name>` and a label `_<name>:`; inside it will `mov rax, <const>` and `ret`.

## How to build

Prerequisites:
- OCaml (4.12+ recommended)
- dune (the project uses dune; the `dune-project` requires 3.19+)
- opam is helpful for installing `re` and managing OCaml toolchain

Build the project with dune from the `coco/` directory:

```zsh
dune build
```

This will compile the library and the `coco` executable. The built executable is available as:

- `dune exec coco <input.c>` (recommended for development)
- or the artifact `_build/default/bin/main.exe` (you can run it directly)

## Run / Example

Create a small test file `sample.c`:

```zsh
cat > sample.c <<'EOF'
int main() {
	return 2;
}
EOF
```

Generate assembly with the compiler:

```zsh
dune exec coco sample.c
# or: _build/default/bin/main.exe sample.c
```

This will create `sample.s` (same base name, `.s` extension) containing the generated assembly.

Note: the generated assembly is Intel-style / NASM-like. It is intended as a readable intermediate and not yet fully portable across toolchains. To turn the `.s` into an executable you will need to adapt the output for your assembler/linker or extend the generator to emit platform-specific prologue/epilogue and directives.

## Limitations (TODO)

- Expressions: only integer constants are implemented. No binary ops, no unary ops, no variables or local storage.
- Types: only `int` is recognised; there is no type checking beyond the parser structure.
- Calling convention: generator emits a minimal `mov rax, <const>; ret` sequence and does not handle parameters, prologue/epilogue, stack frames, or ABI-conformance.
- Tests: `test/test_coco.ml` exists but is empty. There are no automated unit tests yet.
- Platform/tooling: generated assembly is NASM-like Intel syntax. Cross-platform assembly or automatic linking is not yet implemented.

## Next steps

1. Add more expression support (arithmetic, variables).
2. Implement prologue/epilogue and calling conventions for the target platform.
3. Add codegen tests and unit tests for lexer/parser/generator.
4. Provide an option to emit AT&T or GAS-compatible assembly or driver to assemble+link on macOS and Linux.
5. Add CI (dune runtest) and small test suite in `test/`.

## Contributing

Feel free to open issues or PRs. If you want to add features, the easiest places to start are:
- `lib/lexer.ml` — expand tokenization
- `lib/parser.ml` — add more expression forms
- `lib/generator.ml` — add prologue/epilogue and support for more instructions

## References

- [Nora Sandler's Write-a-Compiler series](https://norasandler.com/2017/11/29/Write-a-Compiler.html)
- [Tour of OCaml](https://ocaml.org/docs/tour-of-ocaml)