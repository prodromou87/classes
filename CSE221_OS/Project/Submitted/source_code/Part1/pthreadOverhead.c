#include <stdio.h>       
#include <pthread.h>
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

void* nullFunction (void* data)
{
    pthread_exit(NULL);		/* terminate the thread */
}

int main(int argc, char* argv[])
{
    pthread_t  thread_id;
    ticks threadTimer;
    double timeInNano;
    int i;
    int loops = 100000;

    threadTimer = -getticks();
    for (i=0; i<loops; i++) {
	pthread_create(&thread_id, NULL, nullFunction, NULL);  
    }
    threadTimer += getticks();

    timeInNano = ((double)(threadTimer)) / 3500000000 * 1000000;
    printf ("Time per pthread_create (ns): %lf\n", timeInNano / loops);
}
