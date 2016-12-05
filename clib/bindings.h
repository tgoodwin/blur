#ifndef BINDINGS_H
#define BINDINGS_H

void initGL(int w, int h);

int LoadImage(char *filename);

int foo(int x);

ILubyte * getImageData(char *filename);

void initializeGlDevIL(char *filename);

int** readColorImage(char *filename);

int* readDimensions(char *filename);

int** readGrayscaleImage(char *filename);

int** canvas(char *filename, char* option);

#endif
