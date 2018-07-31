RESULT =reversi
SOURCES=color.ml command.ml commandParser.mly commandLexer.mll play.ml main.ml 
LIBS=unix 
all: native-code

-include OCamlMakefile 
