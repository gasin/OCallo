RESULT =reversi
SOURCES=color.ml command.ml commandParser.mly commandLexer.mll const.ml parameter.ml utils.ml joseki.ml flip.ml eval.ml search.ml extra.ml play.ml main.ml 
LIBS=unix 
all: native-code

-include OCamlMakefile
