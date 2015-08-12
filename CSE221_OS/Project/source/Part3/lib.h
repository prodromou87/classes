#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#include <fcntl.h>
#include <malloc.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <sys/types.h>

#define REP 100000
#define REP_PIPES 100
#define CTX_REP 100000

#define O_LOOP 10
#define O_READ 420
#define O_WRITE_PIPE 2141
#define O_READ_PIPE 2173

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
  
  //randomize the sequence
  for (i = max - 1; i > 0; --i) {
    r = (r << 1) ^ rand();
    v = result[r % (i + 1)];
    result[r % (i + 1)] = result[i];
    result[i] = v;
  }
  
  return (result);
}
