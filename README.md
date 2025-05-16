# Requirements

Need LLVM installed for OCaml
```
opam install llvm
```


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

# To delete later

TESTING semant.ml: 
```
ocamlbuild test_semant.native
./test_semant.native
```


Run lilypond on the output
```
lilypond example.out
```

This will produce a .pdf file containing the sheet music specified

`bachend.ml` should be the final file that runs all of the steps of our compiler, and should output IR code that can then be input into LilyPond

TODOs for IRGen
- SFor
