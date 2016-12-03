

#define DEFAULT_WIDTH  640
#define DEFAULT_HEIGHT 480
 
//int width  = DEFAULT_WIDTH;
//int height = DEFAULT_HEIGHT;

int glutInitialized = 0;    // Ensure glutInit() is not called twice
                            // the only function that calls it is readDimensions()
                            //
int foo(int x) {
    return x + 2;
}
 

/* Handler for window-repaint event. Called back when the window first appears and
   whenever the window needs to be re-painted. */


int* readDimensions(char *filename){

  // Initialize GL and DevIL
  initializeGlDevIL(filename);

  // Load the image into DevIL
  int image;
  image = LoadImage(filename);
  if ( image == -1 ){
    printf("Can't load picture file %s by DevIL \n", filename);
  }

  // Get a data pointer to the image 
  ILubyte * bytes = getImageData(filename);

  // Get the dimensions of the image
  ILuint width, height;
  width = ilGetInteger(IL_IMAGE_WIDTH);
  height = ilGetInteger(IL_IMAGE_HEIGHT);

  int *dimensions = (int*) malloc(sizeof(int) *2);
  dimensions[0] = width;
  dimensions[1] = height;
  return dimensions;
}
 
int** readColorImage(char *filename){

    // Get the dimensions of the image
  int* dimensions = readDimensions(filename);   
  int width = dimensions[0];
  int height = dimensions[1];

  // Load the image into DevIL
  int image;
  image = LoadImage(filename);
  if ( image == -1 ){
    printf("Can't load picture file %s by DevIL \n", filename);
  }

  // Get a data pointer to the image 
  ILubyte * bytes = getImageData(filename);

  int **pixel_array = (int**) malloc(width * height * sizeof(int *));
  for(int i=0; i<width*height; i++){ pixel_array[i] = (int *) malloc(sizeof(int) * 3); } 
  // 3 for rgb

  for(int i = 0; i < height; i++){
    for(int j = 0; j < width; j++){
      pixel_array[(i*width)+j][0] = bytes[(i*width +j)*4 + 0];
      pixel_array[(i*width)+j][1] = bytes[(i*width +j)*4 + 1];
      pixel_array[(i*width)+j][2] = bytes[(i*width +j)*4 + 2];
    }
  }

  return pixel_array;
}

int** readGrayscaleImage(char* filename){

  int** colorImage = readColorImage(filename);
  int* dimensions = readDimensions(filename);
  int width = dimensions[0];
  int height = dimensions[1];
  int** grayImage = (int **) malloc(width * height * sizeof(int *));
  for(int i=0; i<width*height; i++){ grayImage[i] = (int *) malloc(sizeof(int) * 1); } 
  // 1 intensity value

  for(int i = 0; i < height; i++){
    for(int j = 0; j < width; j++){
      grayImage[(i*width)+j][0] = (colorImage[(i*width)+j][0] * .33) + 
                                  (colorImage[(i*width)+j][1] * .33) + 
                                  (colorImage[(i*width)+j][1] * .34);
    }
  }
  return grayImage;
}

/*
int main(int argc, char **argv) 
{
    GLuint texid;
    int    image;
 
    if ( argc < 1){ return -1;  }

    int** colorimg = readColorImage(argv[1]);
    int* dimensions = readDimensions(argv[1]);
    int width = dimensions[0];
    int height = dimensions[1];
    int** gsimg = readGrayscaleImage(argv[1]);

    // Test color image
    for(int i = 0; i < height; i++){
      for(int j = 0; j < width; j++){
        printf(" R: %d\n", colorimg[(i*width) +j][0] );
        printf(" G: %d\n", colorimg[(i*width) +j][1] );
        printf(" B: %d\n", colorimg[(i*width) +j][2] );
      }
    }
  
    // Test Gray scale image
    for(int i=0; i<width; i++){
      for(int j=0; j<height; j++){
        printf("Intensity: %d\n", gsimg[(width*i+j)][0] );
      }
    }
    
     s
    // OpenGL texture binding of the image loaded by DevIL
       glGenTextures(1, &texid); // Texture name generation 
       glBindTexture(GL_TEXTURE_2D, texid); // Binding of texture name
       glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR); // We will use linear interpolation for magnification filter
       glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR); // We will use linear interpolation for minifying filter
       glTexImage2D(GL_TEXTURE_2D, 0, ilGetInteger(IL_IMAGE_BPP), ilGetInteger(IL_IMAGE_WIDTH), ilGetInteger(IL_IMAGE_HEIGHT), 
        0, ilGetInteger(IL_IMAGE_FORMAT), GL_UNSIGNED_BYTE, ilGetData()); // Texture specification
      

    // Main loop 
    glutMainLoop();
 
    // Delete used resources and quit
     ilDeleteImages(1, &image); // Because we have already copied image data into texture data we can release memory used by image.
     glDeleteTextures(1, &texid);
    
     return 0;
     
} */
