RESULT =reversi
SOURCES= const.ml parameter.ml utils.ml flip.ml joseki.ml eval.ml search.ml main.ml 
LIBS=unix 
all: native-code

-include OCamlMakefile 
