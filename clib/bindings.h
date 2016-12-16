#ifndef BINDINGS_H
#define BINDINGS_H

void initGL(int w, int h);

int LoadImage(char *filename);

struct imgData {
    int width;
    int height;
    int sad;
    int *data;
};

int foo(int x);

int *getArr();

struct imgData getImg();

ILubyte * getImageData(char *filename);

void initializeGlDevIL(char *filename);

struct ImageStruct readColorImage(char *filename);

int* readDimensions(char *filename);

struct ImageStruct readGrayscaleImage(char *filename);

struct CanvasStruct canvas(char *filename, char* option);

#endif
