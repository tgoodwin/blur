clib:
	gcc -c bindings.c -lGL -lglut -lGLU -lIL
	ar -cvq libclib.a bindings.o builtins.o

stdlib: 
	gcc -c bindings.c -lGL -lglut -lGLU -lIL
	ar -cvq libclib.a bindings.o builtin.o stdlib.o

exec:
	gcc bindings.c -lGL -lglut -lGLU -lIL -o test

.PHONY: clean
clean:
	rm -f bindings.o
	rm libclib.a

.PHONY: cleantest
cleantest:
	rm -f bindings.o stdlib.o
	rm libclib.a test
