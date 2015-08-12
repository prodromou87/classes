#include <stdio.h> 
#include <time.h>
#include <stdint.h>
#include <sys/time.h>
#include <unistd.h>

void bench_syscall () {
    long int i;
    clock_t timer;

    timer = -clock();
    for (i=0; i<1000000000; i++) {
	time(NULL);
    }
    timer += clock();
    printf ("Time for 10M minimal Syscalls (time) (ms): %f\n", ((double)(timer)) / CLOCKS_PER_SEC * 1000);

    timer = -clock();
    for (i=0; i<1000000000; i++) {
	getpid();
    }
    timer += clock();
    printf ("Time for 10M minimal Syscalls (getpid) (ms): %f\n", ((double)(timer)) / CLOCKS_PER_SEC * 1000);

    int filedesc;
    timer = -clock();
    for (i=0; i<10000000; i++) {
	filedesc = open("testfile.txt");
    }
    timer += clock();
    printf ("Time for 10M minimal Syscalls (open) (ms): %f\n", ((double)(timer)) / CLOCKS_PER_SEC * 1000);
}

void bench_time()
{

    clock_t t_initial, t_final, t_temp1, t_temp2;


    // measuring time - get the time before calling our time function
     t_initial = clock(); 

    // call the time function again, this time it's the one we are measuring
    // we will call the get time function twice to represent the overhead for actually
    // measuring the start and finish times of the code (ie: call time twice!)
     t_temp1 = clock();
     t_temp2 = clock();

    // finally, get the time and we will know the overhead for measuring time :) 
     t_final = clock();

    printf("Time to get two times is: %f.\n", (double) ((double) t_final - t_initial) / CLOCKS_PER_SEC / 1000);

    // repeat, this time convert into time and subtract 
    
    // measuring time - get the time before calling our time function
     t_initial = clock(); 
    
    // call the time function again, this time it's the one we are measuring
    // we will call the get time function twice to represent the overhead for actually
    // measuring the start and finish times of the code (ie: call time twice!)
     t_temp1 = clock();
     t_temp2 = (clock()-t_temp1) / CLOCKS_PER_SEC / 1000;

    // finally, get the time and we will know the overhead for measuring time :) 
     t_final = clock();

     printf("Time to get two times (and subtract/divide) is: %f.\n", (double) ((double) t_final - t_initial) / CLOCKS_PER_SEC / 1000);

}


int main(int argc, char** argv) {

//    bench_time(); 
    bench_syscall();


    return 0;

}




