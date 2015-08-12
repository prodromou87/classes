#include "lib.h"

#define ITERATIONS 2
#define STRIDE 4096

#include <sys/stat.h>


int
main(int argc, char *argv[]){

    if (argc!=2){
	printf("\nERROR! Invalid number of parameters!\n");
	printf("Please use: ./filcache <filename>\n");
	exit(1);
    }

  int pFile;
  FILE* fout;
  char* buffer;
  unsigned int* pages;
  int x, y, limit;
    long size;
  ticks start, end;

    char *filename = argv[1];

    struct stat st;
    
    stat(filename, &st);
    size = st.st_size;
  
    buffer = (char*)malloc((2 * STRIDE) * sizeof(char));
  
  if ((unsigned long)buffer % STRIDE) {
    buffer += STRIDE - (unsigned long)buffer % STRIDE;
  }

    limit = (size / STRIDE);
    pFile = open(filename, O_RDONLY);

    //Eval
    start = getticks();

    for(x = 0 ; x < ITERATIONS ; x++){
      lseek(pFile, 0, SEEK_SET);

      for(y = 0 ; y < limit ; y++){
        read(pFile, buffer, STRIDE);
      }
    }

    end = getticks();
    close(pFile);

    ticks total = end - start;
    double time = (total / ((double) limit));
    int mb = size / (1024 * 1024);
    double speed = (3.5e9 * limit * ITERATIONS * STRIDE) / total;
    speed /= (1024 * 1024);

    printf("%dM %.3lf %.3lf\n", mb, time, speed);
  return 0;
}
