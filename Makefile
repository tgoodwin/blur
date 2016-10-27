OBJS = parser.cmo scanner.cmo prettyprint.cmo

prog : $(OBJS)
	ocamlc -o prog $(OBJS)

scanner.ml : scanner.mll
	ocamllex scanner.mll

parser.ml parser.mli : parser.mly
	ocamllyacc parser.mly

%.cmo : %.ml
	ocamlc -c $<

%.cmi : %.mli
	ocamlc -c $<
