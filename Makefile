RESULT =reversi
SOURCES=color.ml command.ml commandParser.mly commandLexer.mll const.ml utils.ml flip.ml eval.ml play.ml main.ml 
LIBS=unix 
all: native-code

-include OCamlMakefile 
