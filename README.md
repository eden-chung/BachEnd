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

`bachend.ml` should be the final file that runs all of the steps of our compiler, and should output IR code that can then be input into LilyPond