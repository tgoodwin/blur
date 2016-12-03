

#define DEFAULT_WIDTH  640
#define DEFAULT_HEIGHT 480
 
//int width  = DEFAULT_WIDTH;
//int height = DEFAULT_HEIGHT;

int glutInitialized = 0;    // Ensure glutInit() is not called twice
                            // the only function that calls it is readDimensions()
 
int foo(int x){
    return x + 3;
}

/* Handler for window-repaint event. Called back when the window first appears and
   whenever the window needs to be re-painted. */

