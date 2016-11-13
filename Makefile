OBJS = parser.cmo scanner.cmo semantic_analyzer.cmo prettyprint.cmo blur.cmo

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

ast.cmo : 
generator.cmo :  ast.cmo
prog.cmo: scanner.cmo parser.cmi ast.cmo generator.cmo sast.cmi prettyprint.cmo semantic_analyzer.cmo
parser.cmo: ast.cmo parser.cmi
scanner.cmo: parser.cmi
semantic_analyzer.cmo : ast.cmo sast.cmo
parser.cmi: ast.cmo

.PHONY : clean
clean :
	rm -rf prog scanner.ml parser.ml parser.mli
	rm -rf *.cmo *.cmi
	rm -f *~
