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
./bachend.native final_tests/example_all_combined.bach
```

This will generate a PDF called output.pdf. If the user wants to specify the name of the PDF, instead of the command above, run

```
./bachend.native -o <output_filename> final_tests/example_all_combined.bach
```

For example, to generate a PDF file called sample_output.pdf, run
```
./bachend.native -o sample_output final_tests/example_all_combined.bach
```

Then, it should produce a file similar to [this PDF](sample_output.pdf). It will also produce a file called [sample_output.ly](sample_output.ly), which shows the Lilypond source code used to generate the PDF.
