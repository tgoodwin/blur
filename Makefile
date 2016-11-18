OBJS = ast.cmx parser.cmx scanner.cmx semantic_analyzer.cmx exceptions.cmx generator.cmx prettyprint.cmx blur.cmx

prog : $(OBJS)
	ocamlfind ocamlopt -linkpkg -package llvm -package llvm.analysis $(OBJS) -o prog

scanner.ml : scanner.mll
	ocamllex scanner.mll

parser.ml parser.mli : parser.mly
	ocamlyacc parser.mly

%.cmo : %.ml
	ocamlc -c $<

%.cmi : %.mli
	ocamlc -c $<

%.cmx : %.ml
	ocamlfind ocamlopt -c -package llvm $<

ast.cmo : 
ast.cmx :
exceptions.cmo :
exceptions.cmx :
generator.cmo : ast.cmo exceptions.cmo
generator.cmx : ast.cmx exceptions.cmx
prog.cmo: scanner.cmo parser.cmi ast.cmo exceptions.cmo generator.cmo sast.cmi prettyprint.cmo semantic_analyzer.cmo
prog.cmx : scanner.cmx parser.cmx ast.cmx exceptions.cmx generator.cmx sast.cmx prettyprint.cmx semantic_analyzer.cmx
parser.cmo : ast.cmo parser.cmi
parser.cmx : ast.cmx parser.cmi
scanner.cmo: parser.cmi
scanner.cmx : parser.cmx
semantic_analyzer.cmo : ast.cmo sast.cmo
semantic_analyzer.cmx : ast.cmx sast.cmx
parser.cmi: ast.cmo

.PHONY : clean
clean :
	rm -rf prog scanner.ml parser.ml parser.mli
	rm -rf *.cmo *.cmi *.cmx *.o
	rm -f *~
