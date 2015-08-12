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

void main () {

    ticks forkTimer;
    double timeInNano;
    int pid;
    int i;
    int loops = 100;

    forkTimer = -getticks();
    for (i=0; i<loops; i++) {
        if (fork() == 0) return; // Child processes don't loop
    }
    forkTimer += getticks();

    timeInNano = ((double)(forkTimer)) / 3500000000 * 1000000;
    printf ("Time per fork (ns): %lf\n", timeInNano/loops);
    

}
