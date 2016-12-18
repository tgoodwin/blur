LIBDIR = clib

OBJS = ast.cmx parser.cmx scanner.cmx semantic_analyzer.cmx exceptions.cmx configuration.cmx generator.cmx prettyprint.cmx blur.cmx

blur: $(OBJS)
	ocamlfind ocamlopt -linkpkg -package llvm -package llvm.analysis -package llvm.bitwriter -package llvm.bitreader -package llvm.linker $(OBJS) -o blur
		
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

.PHONY: %.ll
%-ll:
	./blur -l < $(*F).blr > $(*F).ll
	make libs
	llc $(*F).ll > $(*F).s
	gcc -I ${LIBDIR} -o $(*F).blx $(*F).s -L${LIBDIR} -lclib -lGL -lglut -lGLU -lIL

# for including the Blur standard library.
.PHONY: %.ll
%-ls:
	./blur -ls < $(*F).blr > $(*F).ll
	cd ${LIBDIR} && make stdlib && cd ../
	llc $(*F).ll > $(*F).s

	gcc -I ${LIBDIR} -o $(*F).blx $(*F).s -L${LIBDIR} -lclib -lGL -lglut -lGLU -lIL

.PHONY : libs
libs :
	cd ${LIBDIR} && make clib

.PHONY : clean
clean :
	rm -f prog scanner.ml parser.ml parser.mli blur
	rm -f *.cmo *.cmi *.cmx *.o *.bc *.ll *.s *.out *.blx
	rm -f *~
