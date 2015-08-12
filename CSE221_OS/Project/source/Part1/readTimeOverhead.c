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

int main() {
    ticks timer, timer2;
    double timeInNano;

    timer = getticks();
    timer2 = getticks();

    timer = timer2 - timer;

    timeInNano = ((double)(timer))/3500000000 * 1000000;
    printf ("Time per time read (ns): %lf\n", timeInNano);
}

