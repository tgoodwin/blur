# blur

Blur is a programming language for creation and modificaiton of ASCII art created from image files.

You must have the OCaml llvm library installed. Install version 3.6 using the following commands:

sudo apt-get install -y ocaml m4 llvm opam
opam init
opam install llvm.3.6 ocamlfind
eval `opam config env`
make
./testall.sh


