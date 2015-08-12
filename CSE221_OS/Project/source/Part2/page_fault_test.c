#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h> /* mmap() is defined in this header */
#include <fcntl.h>
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

int main (int argc, char *argv[]) {
    int fdin;
    char *src;
    struct stat statbuf;
    ticks timer;
    double timeInNano;
    
    if ((fdin = open ("file.txt", O_RDONLY)) < 0) {
	printf ("can't open %s for reading", argv[1]);
	return -1;
    }

    if (fstat (fdin,&statbuf) < 0) {
	printf ("fstat error");
	return -1;
    }

    int size = (int)statbuf.st_size;

    if ((src = mmap (0, statbuf.st_size, PROT_READ, MAP_SHARED, fdin, 0)) 
	== (caddr_t) -1) {
	    printf ("mmap error for input");
    }

    srand(time(NULL));

    timer = -getticks();
    //This is a page fault
    char c = src[rand() % 42949672960];
    timer += getticks();

    timeInNano = ((double)(timer)) / 3500000000 * 1000000;
    printf ("Time per page fault (ns): %lf\n", timeInNano);
    printf ("Time per page fault (cycles): %lli\n", timer);


   return 0;
}
