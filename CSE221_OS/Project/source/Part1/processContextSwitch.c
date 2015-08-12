#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
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

    int data_processed, data_processed_2;
    int file_pipes[2];
    int file_pipes_2[2];
    char some_data[] = "a";
    char buffer [BUFSIZ + 1], buffer2 [BUFSIZ+1];
    int fork_result;

    int loops = 10;
    int i,j;
    ticks contextSwitchTimer;
    double timeInNano;

    memset (buffer, '\0', sizeof(buffer));

pipe(file_pipes);
pipe(file_pipes_2);

	fork_result = fork();
	if (fork_result == 0) { //Child
	    for (i=0; i<loops; i++) {
		data_processed = read(file_pipes[0], buffer, BUFSIZ);
		data_processed_2 = write (file_pipes_2[1], some_data, 1);
	    }
	    	    
	    return 0;
	}
	else { //Parent
	    contextSwitchTimer = -getticks();
	    for (j=0; j<loops; j++) {
		data_processed = write (file_pipes[1], some_data, 1);
		data_processed_2 = read(file_pipes_2[0], buffer2, BUFSIZ);
	    }
	}
	
	contextSwitchTimer += getticks();
	    
	timeInNano = ((double)(contextSwitchTimer)) / 3500000000 * 1000000;
	printf ("Time per context switch (ns): %lf\n", timeInNano / loops);


    return 0;

}
