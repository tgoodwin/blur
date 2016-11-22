blur

Blur is a programming language for creation and modificaiton of ASCII art created from image files.

You must have clang installed

sudo apt-get install clang 

You must have the OCaml llvm library installed. Install version 3.6 using the following commands:

sudo apt-get install -y ocaml m4 llvm opam
opam init
opam install llvm.3.6 ocamlfind
eval `opam config env`
make
./testall.sh

Linking the c files.

Easel
compile c down to .o and llvm down to .o and link them.
link them gcc

the prog file is what takes a blur file and produces llvm
then in the makefile you need one more call that will link the llvm emitted from prog with the .o emitted from the c code.

