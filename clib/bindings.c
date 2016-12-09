#include <stdio.h>
#include <string.h>
#include <GL/glut.h>
#include <IL/il.h>


#define DEFAULT_WIDTH  640
#define DEFAULT_HEIGHT 480
 
//int width  = DEFAULT_WIDTH;
//int height = DEFAULT_HEIGHT;

int glutInitialized = 0;    // Ensure glutInit() is not called twice
                            // the only function that calls it is readDimensions()


struct imgData {
    int width;
    int height;
    int depth;
    int *data;
};

int foo(int x) {
    return x + 2;
}

int *getArr() {
    int * arr = malloc(4 * sizeof(int));
    arr[0] = 7;
    arr[1] = 10;
    arr[2] = 77;
    arr[3] = 400;
    return arr;
}


/*
struct canvasData {
    int width;
    int height;
    char *data;
}; */

struct imgData getImg() {
    int *arr = malloc(4 * sizeof(int));
    arr[0] = 5;
    arr[1] = 10;
    struct imgData img;
    img.width = 640;
    img.height = 400;
    img.depth = 23;
    img.data = getArr();
    return img;
}

/* Handler for window-repaint event. Called back when the window first appears and
   whenever the window needs to be re-painted. */
void display() 
{
    // Clear color and depth buffers
       glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT); 
       glMatrixMode(GL_MODELVIEW);     // Operate on model-view matrix
 
    /* Draw a quad */
       glBegin(GL_QUADS);
           glTexCoord2i(0, 0); glVertex2i(0,   0);
           glTexCoord2i(0, 1); glVertex2i(0,   DEFAULT_HEIGHT);
           glTexCoord2i(1, 1); glVertex2i(DEFAULT_WIDTH, DEFAULT_HEIGHT);
           glTexCoord2i(1, 0); glVertex2i(DEFAULT_WIDTH, 0);
       glEnd();
 
    glutSwapBuffers();
} 
 
/* Handler for window re-size event. Called back when the window first appears and
   whenever the window is re-sized with its new width and height */
/*
void reshape(GLsizei newwidth, GLsizei newheight) 
{  
    // Set the viewport to cover the new window
       glViewport(0, 0, width=newwidth, height=newheight);
     glMatrixMode(GL_PROJECTION);
     glLoadIdentity();
     glOrtho(0.0, width, height, 0.0, 0.0, 100.0);
     glMatrixMode(GL_MODELVIEW);
 
    glutPostRedisplay();
}
*/
 
/* Initialize OpenGL Graphics */
void initGL(int w, int h) 
{
     glViewport(0, 0, w, h); // use a screen size of WIDTH x HEIGHT
     glEnable(GL_TEXTURE_2D);     // Enable 2D texturing
 
    glMatrixMode(GL_PROJECTION);     // Make a simple 2D projection on the entire window
     glLoadIdentity();
     glOrtho(0.0, w, h, 0.0, 0.0, 100.0);
 
     glMatrixMode(GL_MODELVIEW);    // Set the matrix mode to object modeling
 
     glClearColor(0.0f, 0.0f, 0.0f, 0.0f); 
     glClearDepth(0.0f);
     glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT); // Clear the window
}
 
/* Load an image using DevIL and return the devIL handle (-1 if failure) */
int LoadImage(char *filename)
{
    ILboolean success; 
     ILuint image; 
 
    ilGenImages(1, &image); /* Generation of one image name */
     ilBindImage(image); /* Binding of image name */
     success = ilLoadImage(filename); /* Loading of the image filename by DevIL */

    if (success) /* If no error occured: */
    {
        /* Convert every colour component into unsigned byte. If your image contains alpha channel you can replace IL_RGB with IL_RGBA */
           success = ilConvertImage(IL_RGBA, IL_UNSIGNED_BYTE); 
 
        if (!success)
           {
                 return -1;
           }
    }
    else
        return -1;
 
    return image;
}

ILubyte * getImageData(char *filename)
{
    ILboolean success; 
     ILuint image; 
 
    ilGenImages(1, &image); /* Generation of one image name */
     ilBindImage(image); /* Binding of image name */
     success = ilLoadImage(filename); /* Loading of the image filename by DevIL */

    if (success) /* If no error occured: */
    {
        /* Convert every colour component into unsigned byte. If your image contains alpha channel you can replace IL_RGB with IL_RGBA */
           success = ilConvertImage(IL_RGBA, IL_UNSIGNED_BYTE); 
 
        if (!success){ "IMAGE FAILED TO BE SUCCESFULLY READ!"; }
    }

    ILubyte * bytes = ilGetData();
    return bytes; 
}

void initializeGlDevIL(char *filename){

  // GLUT init if not already initialized
  if( glutInitialized == 0){

    int *num_files_to_read = (int *) malloc(sizeof(int));
    *num_files_to_read = 1;

    glutInit(num_files_to_read, &filename);            // Initialize GLUT
    glutInitDisplayMode(GLUT_DOUBLE); // Enable double buffered mode
    /*
    glutInitWindowSize(DEFAULT_WIDTH, DEFAULT_HEIGHT);   // Set the window's initial width & height
    glutCreateWindow(filename);      // Create window with the name of the executable
    glutDisplayFunc(display);       // Register callback handler for window re-paint event
    glutReshapeFunc(reshape);       // Register callback handler for window re-size event
    */

    /* OpenGL 2D generic init */
    initGL(DEFAULT_WIDTH, DEFAULT_HEIGHT);
 
    // Initialization of DevIL 
     if (ilGetInteger(IL_VERSION_NUM) < IL_VERSION)
     {
           printf("wrong DevIL version \n");
           exit(0);
     }
     ilInit(); 
  }

  glutInitialized = 1;
}

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

struct ImageStruct{
  int *width;
  int *height;
  int *imageData;
};
 
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

struct ImageStruct *readGrayscaleImage(char* filename){

  int** colorImage = readColorImage(filename);
  int* dimensions = readDimensions(filename);
  int width = dimensions[0];
  int height = dimensions[1]; 
  int *width_ptr = (int *) malloc(sizeof(int));  
  int *height_ptr = (int *) malloc(sizeof(int));  
  *width_ptr = dimensions[0];
  *height_ptr = dimensions[1];
  
  int* grayImage = (int *) malloc(width * height * sizeof(int));

  for(int i = 0; i < height; i++){
    for(int j = 0; j < width; j++){
      grayImage[(i*width)+j] = (colorImage[(i*width)+j][0] * .33) + 
                               (colorImage[(i*width)+j][1] * .33) + 
                               (colorImage[(i*width)+j][1] * .34);
    }
  }

  struct ImageStruct *is = malloc( sizeof(struct ImageStruct) );
  is->width = width_ptr;
  is->height = height_ptr; 
  is->imageData = grayImage;
  return is;
}

int** canvas(char *filename, char* option){

  int* dimensions = readDimensions(filename);
  int width = dimensions[0];
  int height = dimensions[1];
  int **canvas = (int **) malloc(width * height * sizeof(int *));
  
  if( strcmp( option, "color") == 0 ){
    for(int i=0; i<width*height; i++){ canvas[i] = (int *) malloc(sizeof(int) * 3); } 
    canvas = readColorImage(filename); 
  }
  else if( strcmp(option, "grayscale") == 0 ){
    for(int i=0; i<width*height; i++){ canvas[i] = (int *) malloc(sizeof(int) * 1); } 
    canvas = readColorImage(filename); 
  }
  else{
	printf("invalid argument to command <canvas>");
	exit(1);
  } 
  return canvas;
}

/*
int main(int argc, char **argv) 
{
    GLuint texid;
    int    image;
 
    if ( argc < 1){ return -1; }

    int* dimensions = readDimensions(argv[1]);
    int width = dimensions[0];
    int height = dimensions[1];
    struct ImageStruct *s = readGrayscaleImage(argv[1]);
    printf("width: %d\n",*(s->width));
    printf("height: %d\n",*(s->height));
    for(int i=0; i<width*height; i++){
        printf("%d\n", s->imageData[i]);
    }
 
    int** canvass = canvas(argv[1],"grayscale");

    for(int i = 0; i < height; i++){
      for(int j = 0; j < width; j++){
        printf(" Intensity: %d\n", canvass[(i*width) +j][0] );
      }
    }

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
} 
*/
