#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>

typedef unsigned long long ticks;

int data_processed, data_processed_2;
int file_pipes[2];
int file_pipes_2[2];
char some_data[] = "a";
char buffer [BUFSIZ + 1], buffer2 [BUFSIZ+1];
int fork_result;
int loops = 10;
int i,j;
ticks threadContextSwitchTimer;

static __inline__ ticks getticks(void)
{
     unsigned a, d;
     asm("cpuid");
     asm volatile("rdtsc" : "=a" (a), "=d" (d));

     return (((ticks)a) | (((ticks)d) << 32));
}

void *f1(void *arg) {
    threadContextSwitchTimer = -getticks();
    for (i=0; i<loops; i++) {
	data_processed = write (file_pipes[1], some_data, 1);
	data_processed_2 = read(file_pipes_2[0], buffer2, BUFSIZ);
    }
}

void *f2(void *arg) {
    for (j=0; j<loops; j++) {
	data_processed = read(file_pipes[0], buffer, BUFSIZ);
	data_processed_2 = write (file_pipes_2[1], some_data, 1);
    }
    threadContextSwitchTimer += getticks();
}

int main() {

    pthread_t thread1, thread2;
    void *status1, *status2;

    double timeInNano;

    memset (buffer, '\0', sizeof(buffer));
    memset (buffer2, '\0', sizeof(buffer2));

    pipe(file_pipes);
    pipe(file_pipes_2);

    pthread_create(&thread1, NULL, f1, (void*) NULL);
    pthread_create(&thread2, NULL, f2, (void*) NULL);

    int rc1 = pthread_join (thread1, &status1);
    int rc2 = pthread_join (thread2, &status2);

    timeInNano = ((double)(threadContextSwitchTimer)) / 3500000000 * 1000000;
    printf ("Time per context switch (ns): %lf\n", timeInNano / loops);

} 
