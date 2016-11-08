OBJS = parser.cmo scanner.cmo prettyprint.cmo blur.cmo

prog : $(OBJS)
	ocamlc -o prog $(OBJS)

scanner.ml : scanner.mll
	ocamllex scanner.mll

parser.ml parser.mli : parser.mly
	ocamlyacc parser.mly

%.cmo : %.ml
	ocamlc -c $<

%.cmi : %.mli
	ocamlc -c $<

prog.cmo: scanner.cmo parser.cmi ast.cmi prettyprint.cmo
parser.cmo: ast.cmi parser.cmi
scanner.cmo: parser.cmi
parser.cmi: ast.cmi

.PHONY : clean
clean :
	rm -rf prog scanner.ml parser.ml parser.mli
	rm -rf *.cmo *.cmi
