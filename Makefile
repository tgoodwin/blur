OBJS = parser.cmo scanner.cmo prettyprint.cmo

prog : $(OBJS)
	ocamlc -o prog $(OBJS)

scanner.ml : scanner.mll
	ocamllex scanner.mll

parser.ml parser.mli : parser.mly
	ocamlyacc parser.mly

ast.cmi : ast.mli
	ocamlc -c ast.mli

parser.cmi : parser.mli
	ocamlc -c parser.mli

parser.cmo : parser.ml
	ocamlc -c parser.ml

scanner.cmo : scanner.ml
	ocamlc -c scanner.ml

prettyprint.cmo : prettyprint.ml
	ocamlc -c prettyprint.ml

prog.cmo: scanner.cmo parser.cmi ast.cmi prettyprint.cmo
parser.cmo: ast.cmi parser.cmi
scanner.cmo: parser.cmi
parser.cmi: ast.cmi

.PHONY : clean
clean :
	rm -rf prog scanner.ml parser.ml parser.mli
	rm -rf *.cmo *.cmi

