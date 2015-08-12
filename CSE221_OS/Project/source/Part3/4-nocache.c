#include "lib.h"

#define O_DIRECT 040000
#define MIN_CACHE (128 * 1024)
#define MAX_CACHE (1.5 * 1024 * 1024)
#define STOP_SIZE (32 * 1024 * 1024)
#define ITERATIONS 100
#define STRIDE 4096

int
main(int argc, char *argv[]){
  FILE* pFile;
  char* buffer;
  int x, y, limit, fd;
  ticks csize;
  ticks start, end, stop;
  unsigned int* pages;
  char buf[40];
  int seq = atoi(argv[1]);

  buffer = (char*)malloc((2 * STRIDE) * sizeof(char));
  
  if ((unsigned long)buffer % STRIDE) {
    buffer += STRIDE - (unsigned long)buffer % STRIDE;
  }
  
  for(csize = MIN_CACHE ; csize <= MAX_CACHE ; csize += (128*1024)){
    //sprintf(buf, "tmp%d.txt", atoi(argv[2]));
    //pFile = fopen(&buf[0], "w");
    pFile = fopen("tmp.txt", "w");

    limit = (csize / STRIDE);

    for(x = 0 ; x < limit ; x++){
      fwrite (buffer, 1, STRIDE, pFile);
    }

    fclose(pFile);
    
    fd = open("tmp.txt", O_DIRECT);

    pages = permutation(limit, STRIDE);
    stop = STOP_SIZE / STRIDE;

    //Eval
    //start = getticks();
   
    ticks sum = 0; 
    for(x = 0 ; x < ITERATIONS ; x++){
      start = getticks();

      if(seq == 1)
	lseek(fd, 0, SEEK_SET);

      for(y = 0 ; y < limit ; y++){
	if(seq == 0)
	  lseek(fd, pages[y], SEEK_SET);

	read(fd, buffer, STRIDE);

	//if(seq == 0 && y == stop)
	//  break;
      }

      end = getticks();
	sum += (end-start);
      /*
      ticks total = end - start;
      double speed = (3.5e9 * limit * STRIDE) / total;
      speed /= (1024 * 1024);

      printf("%lldM: %.3lf\n", (csize/1024), speed);      
      */
    }
    ticks avg = sum/ITERATIONS;
      double speed = (3.5e9 * limit * STRIDE) / avg;
      speed /= (1024 * 1024);

      printf("%lldK: %.3lf\n", (csize/1024), speed);

    //end = getticks();
    close(fd);
    unlink("tmp.txt");

    /*
    ticks total = end - start - O_READ - ((ITERATIONS * limit) * O_LOOP);
    double time = (total / ((double) limit));
    int mb = csize / (1024 * 1024);
    double speed = (2.4e9 * limit * ITERATIONS * STRIDE) / total;
    speed /= (1024 * 1024);
    */

    //printf("%dM %.3lf %.3lf\n", mb, time, speed);
  }
}
