#include <stdio.h>
#include <time.h>
#include <sys/time.h>
typedef unsigned long long ticks;
static __inline__ ticks getticks(void)
{
     unsigned a, d;
     asm("cpuid");
     asm volatile("rdtsc" : "=a" (a), "=d" (d));

     return (((ticks)a) | (((ticks)d) << 32));
}

void main(int argc, char **argv) {

    ticks loopTimer;
    int i;
    int loop_length = 1000000000;
    double timeInNano;

    loopTimer = -getticks();
    for (i=0; i<loop_length; i++);
    loopTimer += getticks();

    timeInNano = ((double)(loopTimer)) / 3500000000 * 1000000; // x1M for nano
    printf ("Time per loop (ns): %lf\n", timeInNano/loop_length);

}
