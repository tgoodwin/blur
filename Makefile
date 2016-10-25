#!/bin/bash

filename=$(date | cut -f 4 -d " ")
mkdir $filename
cp * ./$filename
cd $filename

ocamllex scanner.mll
ocamlyacc parser.mly
ocamlc -c ast.ml
ocamlc -c parser.mli
ocamlc -c scanner.ml
ocamlc -c parser.ml
ocamlc -o blur parser.cmo scanner.cmo 

