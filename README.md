# Requirements

Need LLVM installed for OCaml
```
opam install llvm
```

Lilypond, the library we use to produce the sheet music PDF must also be installed. Follow instructions [here](https://lilypond.org/)

# Usage

Compile project
```
ocamlbuild -use-ocamlfind -pkg "llvm,str" bachend.native
```

To compile bachend code into a PDF, run
```
./bachend.native <filename.bach>
```

For example,
```
./bachend.native final_tests/example1.bach
```

This will generate a PDF called output.pdf. If the user wants to specify the name of the PDF, instead of the command above, run

```
./bachend.native -o <output_filename> final_tests/example1.bach
```

For example, to generate a PDF file called twinkle.pdf, run
```
./bachend.native -o twinkle final_tests/example1.bach
```
