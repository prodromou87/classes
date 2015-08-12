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

void main (int argc, char** argv) {
    long int i;
    ticks timer;
    double timeInNano;
    int numOfLoops;
    char *syscall;

    if (argc != 2) {
	printf ("Usage: ./a.out <syscall>\n");
	printf ("Available syscalls: (0) time, (1) getpid, (2) open\n");
	return;
    }

    int choice = atoi (argv[1]);

    if (choice == 0) {
	timer = -getticks();
	for (i=0; i<100000000; i++) {
	    time(NULL);
	}
	timer += getticks();
	syscall = "time";
	numOfLoops = 100000000;
    }
    else if (choice == 1) {
	timer = -getticks();
	for (i=0; i<100000000; i++) {
	    getpid();
	}
	timer += getticks();
	syscall = "getpid";
	numOfLoops = 100000000;
    }
    else {
	int filedesc;
	timer = -getticks();
	for (i=0; i<10000000; i++) {
	    filedesc = open("testfile.txt");
	}
	timer += getticks();
	numOfLoops = 10000000;
	syscall = "open";
    }
    timeInNano = ((double)(timer)) / 3500000000 * 1000000;
    printf ("Time per syscall (%s) (ns): %lf\n", syscall, timeInNano/numOfLoops);
}
