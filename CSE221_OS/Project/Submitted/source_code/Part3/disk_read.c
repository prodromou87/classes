#include <time.h>
#include <sys/time.h>

#define O_DIRECT 040000
#define CACHE_LOW (128 * 1024)
#define CACHE_HIGH (1.5 * 1024 * 1024)

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
  int x, y, limit, fd;
  char* buf;
  unsigned int* pages;
  int seq = atoi(argv[1]);
  ticks size;
  char buf[40];
  ticks start, end;
  FILE* myFile;

  buf = (char*)malloc((2 * 4096) * sizeof(char));
  
  if ((unsigned long)buf % 4096) {
    buf += 4096 - (unsigned long)buf % 4096;
  }
  
  for(size = CACHE_LOW ; size <= CACHE_HIGH ; size += (128*1024)){
    myFile = fopen("tmp.txt", "w");

    limit = (size / 4096);

    for(x = 0 ; x < limit ; x++){
      fwrite (buf, 1, 4096, myFile);
    }

    fclose(myFile);
    
    fd = open("tmp.txt", O_DIRECT);

    pages = permutation(limit, 4096);

   
    ticks sum = 0; 
    for(x = 0 ; x < 100 ; x++){
      start = getticks();

      if(seq == 1)
	lseek(fd, 0, SEEK_SET);

      for(y = 0 ; y < limit ; y++){
	if(seq == 0)
	  lseek(fd, pages[y], SEEK_SET);

	read(fd, buf, 4096);

      }

      end = getticks();
	sum += (end-start);
    }
    ticks avg = sum/100;
      double speed = (3.5e9 * limit * 4096) / avg;
      speed /= (1024 * 1024);

      printf("%lldK: %.3lf\n", (size/1024), speed);

    close(fd);
    unlink("tmp.txt");

  }
}
