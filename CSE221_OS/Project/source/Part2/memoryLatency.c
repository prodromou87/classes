#include <stdbool.h> 
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>

// global variables used
long numArrayElements = 1024*1024*16; // (1024*1024) = 1MB * 16 = 16MB worth of cachelin elements, 64 bytes per cache line for a total of 16MB * 64 = 1 GB
int lineSize = 64*8;
int sizeOfPtr = 8;
int numPtersPerCacheLine = 64;
int numIterations = 10;
int loopIterations = 1024*1024*16;
//int numArrayElements, lineSize, numPtersPerCacheLine, sizeOfPtr, totalSize, numIterations;

// define the structures needed to keep track of time
typedef struct _cacheLine { struct _cacheLine *cacheLinePtr[64] } cacheLine;
typedef unsigned long long ticks;

// inline assembly used to actually get time measurements on x86
static __inline__ ticks getticks(void)
{
     unsigned a, d;
     asm("cpuid");
     asm volatile("rdtsc" : "=a" (a), "=d" (d));
     return (((ticks)a) | (((ticks)d) << 32));
}

long increment(long d) {

    if( d < 1024)
        d *= 2;
    else if( d < 4*1024)
    {
        d +=1024;
    } else {
        long e;
        for(e = 4*1024; e <= d; e *= 2)
            ;
        d += e/4;
    }
    return (d);
}

int main() {

int sizes[] = {4,8,16,32,56,64,72,80,88,96,104,120,128,160,192,224,256,288,320,384,512,640,768,896,1024,1280,2048,3072,4096,6144,8192,12288,16384,20480,24576,28672,32768,34*1024,36*1024,40960,49152,65536,131072,192*1024,256*1024,320*1024,384*1024,512*1024,768*1024,1<<20,(1024+256)*1024,(1024+512)*1024,(1024+768)*1024,1<<21,(2048+256)*1024,(2048+512)*1024,(2048+768)*1024,3072*1024,3407872,3*1024*1024+1024*512,1<<22,5242880,6291456,7*1024*1024,8*1024*1024,9*1024*1024,10*1024*1024,12*1024*1024,14*1024*1024,15*1024*1024,16*1024*1024,20*1024*1024,21*1024*1024,32*1024*1024,48*1024*1024,64*1024*1024,72*1024*1024,96*1024*1024,128*1024*1024,0};


    long i, j, k = 0;
    ticks timer;
    FILE *file = fopen("out.txt", "w");
   
    long d;
    int repetitions = 50;
    
    // create the array we will be working with. (note this is a 1GB array!)
    cacheLine * array = (cacheLine*) malloc(numArrayElements * lineSize);
    if(array == NULL) return -1;
    //printf("created the array\n");

    // iteratively increase the array size
    int index;
    for(index = 1; index < 22; index++)
    {
        d = sizes[index];
        // now that we have created our array, we need to set up the array pointers such that prefectching doesn't work..
        // first step will to be to make sure to initialize everything to NULL
        for(i = 0; i < d; i++)
            array[i].cacheLinePtr[0] = NULL;

/*        // now we make it so the pointers will effectively swap between the first and second  halves of the array
        long firstFourth = 0;
        long secondFourth = d / 4;
        long thirdFourth = d / 2;
        long fourthFourth = 3 * d / 4;
        
        //printf("d: %li, %li, %li, %li, %li.\n", d, firstFourth, secondFourth, thirdFourth, fourthFourth);
        
        for(i = 0; i < d / 4; i++)
        {  
            array[firstFourth++].cacheLinePtr[0] = &array[fourthFourth];
            array[fourthFourth++].cacheLinePtr[0] = &array[secondFourth];
            array[secondFourth++].cacheLinePtr[0] = &array[thirdFourth];
            array[thirdFourth++].cacheLinePtr[0] = &array[firstFourth];
        }
        
        // the last will be used as our termination point
        ////printf("indexArr[%li] is getting set to NULL.\n", indexArr[d-1]);
        array[thirdFourth-1].cacheLinePtr[0] = &array[0];
  */

        long firstHalf = 0;
        long secondHalf = d/2;

        for(i = 0; i < d/2; i++)
        {
            array[firstHalf++].cacheLinePtr[0] = &array[secondHalf];
            array[secondHalf++].cacheLinePtr[0] = &array[firstHalf];
        }

        array[secondHalf-1].cacheLinePtr[0] = &array[0];
        // warm up the caches first before taking any measurements
        // we will do a simple pointer chasing implementation to measure the latencies
        cacheLine * ptr = array;
        for(k = 0; k < d; k++)
        {
            ptr = ptr->cacheLinePtr[0];
        }


        // now that we have a constant array size from this point forward
        // and the cache's have been warmed up sufficiently
        // we need to run a number of repetitions to make sure we are getting the correct metrics
        ticks timer2;
        double timeAccumulator = 0; //timer accumulator
        for(k = 0; k < repetitions; k++)
        {
            // declare needed variables
            cacheLine * ptr = array;            
            
            // start the clock...
            timer = getticks();
            for(i = 0; i < loopIterations; i++)
                ptr = ptr->cacheLinePtr[0];
           
            //stop the clock and store it.
            timer2 = getticks();
            timeAccumulator += (timer2 - timer) / loopIterations; //compensate for the loop overhead..
        }
        timeAccumulator = timeAccumulator / repetitions;
        fprintf(file, "%lf\n", timeAccumulator);
        printf("%li\n", d);
    }
}
