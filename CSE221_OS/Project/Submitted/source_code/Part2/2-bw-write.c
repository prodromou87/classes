#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <sys/time.h>

#define CACHE (64 * 1024 * 1024)

typedef unsigned long long ticks;
static __inline__ ticks getticks(void)
{
     unsigned a, d;
     asm("cpuid");
     asm volatile("rdtsc" : "=a" (a), "=d" (d));

     return (((ticks)a) | (((ticks)d) << 32));
}

int
main(){
  register volatile int sum;
  ticks start, end;
  int i, z, j;
  int limit = (CACHE >> 5);

  int* x = (int*)malloc(CACHE * sizeof(int));
  register int* p;

  for(z = 0 ; z < 1 ; z++){
    p = x;

    for(i = 0 ; i < limit ; i++){
        for (j=0; j<32; j++) {
            p[i] = 1;
        }
        p += 32;
    }
  }

  start = getticks();

  for(z = 0 ; z < 10 ; z++){
    p = x;

    for(i = 0 ; i < limit ; i++){
        for (j=0; j<32; j++) {
            p[i] = 1;
        }
        p += 32;
    }
  }

  end = getticks();

  ticks cycles = end - start;
  cycles = cycles - O_READ - ((10 * limit) * O_LOOP);

  unsigned long long bytes = (CACHE * sizeof(int) * 10);

  double bpc = ((double) bytes) / cycles;
  double bw = (bpc * 3.5e9) / (1024 * 1024);

  printf("%.8lf %.8lf\n", bpc, bw);

  return 1;
}
