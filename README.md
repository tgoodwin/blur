#blur

Blur is a programming language for creation and modificaiton of ASCII art created from image files.

Dependencies: 
sudo apt-get install freeglut3-dev
sudo apt-get install binutils-gold ( for ubuntu >= 11.10 )
sudo apt-get install libdevil-dev


Bindings functions for use in blur code:

int** readImage(char * filename)

This reads an image file from a filename and returns an array of two pointers. The first pointer is an array containing {image_width, image_height}. The second pointer is a pointer to the RGB value image pixels. To iterate over the pixels use the following for loop:

for(int i = 0; i < height; i++){
      for(int j = 0; j < width; j++){
        printf(" Red: %d\n", bytes[(i*width +j)*4 + 0]);
        printf(" Green: %d\n", bytes[(i*width +j)*4 + 1]);
        printf(" Blue: %d\n", bytes[(i*width +j)*4 + 2]);
      }
   }
