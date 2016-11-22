blur

Blur is a programming language for creation and modificaiton of ASCII art created from image files.

Dependencies: 
sudo apt-get install freeglut3-dev
sudo apt-get install binutils-gold ( for ubuntu >= 11.10 )
sudo apt-get install libdevil-dev


Linking the c files.

Easel
compile c down to .o and llvm down to .o and link them.
link them gcc

the prog file is what takes a blur file and produces llvm
then in the makefile you need one more call that will link the llvm emitted from prog with the .o emitted from the c code.

