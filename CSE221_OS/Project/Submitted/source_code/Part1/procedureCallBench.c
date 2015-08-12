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

ticks procCallTimer;
double timeInNano;

void f0 () {
}

void f1 (float a1) {
}

void f2 (float a1, float a2) {
}

void f3 (float a1, float a2, float a3) {
}

void f4 (float a1, float a2, float a3, float a4) {
}

void f5 (float a1, float a2, float a3, float a4, float a5) {
}

void f6 (float a1, float a2, float a3, float a4, float a5, float a6) {
}

void f7 (float a1, float a2, float a3, float a4, float a5, float a6, float a7) {
}

void d0 () {
}

void d1 (double a1) {
}

void d2 (double a1, double a2) {
}

void d3 (double a1, double a2, double a3) {
}

void d4 (double a1, double a2, double a3, double a4) {
}

void d5 (double a1, double a2, double a3, double a4, double a5) {
}

void d6 (double a1, double a2, double a3, double a4, double a5, double a6) {
}

void d7 (double a1, double a2, double a3, double a4, double a5, double a6, double a7) {
}


void main (int argc, char** argv) {
    int i;
    float a1,a2,a3,a4,a5,a6,a7;
    double b1,b2,b3,b4,b5,b6,b7;

    if (argc != 3) {
	printf ("Usage: ./a.out <number of arguments> <floats(0)/doubles(1)>\n");
	return;
    }

    int numOfArgs = atoi (argv[1]);
    int benchType = atoi (argv[2]);

    if (benchType == 0) {
	if (numOfArgs == 0) {
	    procCallTimer = -getticks();
	    for (i=0; i<1000000000; i++) {
		f0();
	    }
	    procCallTimer += getticks();
	}
	else if (numOfArgs == 1) {
	    procCallTimer = -getticks();
	    for (i=0; i<1000000000; i++) {
		f1(a1);
	    }
	    procCallTimer += getticks();
	}
	else if (numOfArgs == 2) {
	    procCallTimer = -getticks();
	    for (i=0; i<1000000000; i++) {
		f2(a1,a2);
	    }
	    procCallTimer += getticks();
	}
	else if (numOfArgs == 3) {
	    procCallTimer = -getticks();
	    for (i=0; i<1000000000; i++) {
		f3(a1,a2,a3);
	    }
	    procCallTimer += getticks();
	}
	else if (numOfArgs == 4) {
	    procCallTimer = -getticks();
	    for (i=0; i<1000000000; i++) {
		f4(a1,a2,a3,a4);
	    }
	    procCallTimer += getticks();
	}
	else if (numOfArgs == 5) {
	    procCallTimer = -getticks();
	    for (i=0; i<1000000000; i++) {
		f5(a1,a2,a3,a4,a5);
	    }
	    procCallTimer += getticks();
	}
	else if (numOfArgs == 6) {
	    procCallTimer = -getticks();
	    for (i=0; i<1000000000; i++) {
		f6(a1,a2,a3,a4,a5,a6);
	    }
	    procCallTimer += getticks();
	}
	else {
	    procCallTimer = -getticks();
	    for (i=0; i<1000000000; i++) {
		f7(a1,a2,a3,a4,a5,a6,a7);
	    }
	    procCallTimer += getticks();
	}
    }
    else {
	if (numOfArgs == 0) {
	    procCallTimer = -getticks();
	    for (i=0; i<1000000000; i++) {
		d0();
	    }
	    procCallTimer += getticks();
	}
	else if (numOfArgs == 1) {
	    procCallTimer = -getticks();
	    for (i=0; i<1000000000; i++) {
		d1(b1);
	    }
	    procCallTimer += getticks();
	}
	else if (numOfArgs == 2) {
	    procCallTimer = -getticks();
	    for (i=0; i<1000000000; i++) {
		d2(b1,b2);
	    }
	    procCallTimer += getticks();
	}
	else if (numOfArgs == 3) {
	    procCallTimer = -getticks();
	    for (i=0; i<1000000000; i++) {
		d3(b1,b2,b3);
	    }
	    procCallTimer += getticks();
	}
	else if (numOfArgs == 4) {
	    procCallTimer = -getticks();
	    for (i=0; i<1000000000; i++) {
		d4(b1,b2,b3,b4);
	    }
	    procCallTimer += getticks();
	}
	else if (numOfArgs == 5) {
	    procCallTimer = -getticks();
	    for (i=0; i<1000000000; i++) {
		d5(b1,b2,b3,b4,b5);
	    }
	    procCallTimer += getticks();
	}
	else if (numOfArgs == 6) {
	    procCallTimer = -getticks();
	    for (i=0; i<1000000000; i++) {
		d6(b1,b2,b3,b4,b5,b6);
	    }
	    procCallTimer += getticks();
	}
	else {
	    procCallTimer = -getticks();
	    for (i=0; i<1000000000; i++) {
		d7(b1,b2,b3,b4,b5,b6,b7);
	    }
	    procCallTimer += getticks();
	}
    }
    timeInNano = ((double)(procCallTimer)) / 3500000000 * 1000000; // x1M for nano
    printf ("Time per call (ns): %lf\n", timeInNano/1000000000);
}
