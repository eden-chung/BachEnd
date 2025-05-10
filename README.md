# Requirements

Need LLVM installed for OCaml
```
opam install llvm
```


TESTING semant.ml: 
```
ocamlbuild test_semant.native
./test_semant.native
```

Compile project
```
ocamlbuild -use-ocamlfind -pkgs llvm bachend.native
```

```
./bachend.native -l example.bach > example.out
```

Run lilypond on the output
```
lilypond example.out
```

This will produce a .pdf file containing the sheet music specified

`bachend.ml` should be the final file that runs all of the steps of our compiler, and should output IR code that can then be input into LilyPond

TODOs for IRGen
- SFor

Transpose needs to be added to scanner and parser