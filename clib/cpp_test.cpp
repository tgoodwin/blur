#include "bindings.cpp"

//vector<char> a = readBMP("goldenmushie.bmp");

int main(int argc, char **argv){
    int* dimensions = readImage("Golden_Mushroom.jpg");
    printf("width: %d",dimensions);
    printf("height:%d",++dimensions);
}

