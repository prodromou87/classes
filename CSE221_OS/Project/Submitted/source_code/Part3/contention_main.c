#include <time.h>
#include <sys/time.h>

#define O_DIRECT 040000
#define CACHE_LOW (64 * 1024 * 1024)
#define CACHE_HIGH (64 * 1024 * 1024)

typedef unsigned long long ticks;

static __inline__ ticks getticks(void)
{
     unsigned a, d;
     asm("cpuid");
     asm volatile("rdtsc" : "=a" (a), "=d" (d));

     return (((ticks)a) | (((ticks)d) << 32));
}

unsigned int*
permutation(int max, int scale)
{
  unsigned int i, v;
  static unsigned int r = 0;
  unsigned int* result = (unsigned int*)malloc(max * sizeof(int));
  
  if (result == NULL) return NULL;
  
  for (i = 0; i < max; ++i) {
    result[i] = i * (unsigned int)scale;
  }
  
  if (r == 0)
    r = (getpid()<<6) ^ getppid() ^ rand() ^ (rand()<<10);
  
  for (i = max - 1; i > 0; --i) {
    r = (r << 1) ^ rand();
    v = result[r % (i + 1)];
    result[r % (i + 1)] = result[i];
    result[i] = v;
  }
  
  return (result);
}

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

  buffer = (char*)malloc((2 * 4096) * sizeof(char));
  
  if ((unsigned long)buffer % 4096) {
    buffer += 4096 - (unsigned long)buffer % 4096;
  }
  
  for(csize = CACHE_LOW ; csize <= CACHE_HIGH ; csize += (128*1024)){
    pFile = fopen("tmp.txt", "w");

    limit = (csize / 4096);

    for(x = 0 ; x < limit ; x++){
      fwrite (buffer, 1, 4096, pFile);
    }

    fclose(pFile);
    
    fd = open("tmp.txt", O_DIRECT);

    pages = permutation(limit, 4096);
   
    ticks sum = 0; 
    for(x = 0 ; x < 1000 ; x++){
      start = getticks();

      if(seq == 1)
	lseek(fd, 0, SEEK_SET);

      for(y = 0 ; y < 1 ; y++){
	if(seq == 0)
	  lseek(fd, pages[y], SEEK_SET);

	read(fd, buffer, 4096);

      }

      end = getticks();
	sum += (end-start);
    }
    ticks avg = sum/1000;
      double speed = (3.5e9 * 4096) / avg;
      speed /= (1024 * 1024);

      printf("%lldK: %.3lf\n", (csize/1024), speed);

    close(fd);
    unlink("tmp.txt");

  }
}
