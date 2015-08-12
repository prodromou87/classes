#!/bin/sh

gcc loopOverhead.c -o loopOverhead -O0
gcc procedureCallBench.c -o procedureCallOverhead -O0
gcc processCreationOverhead.c -o processCreationOverhead -O0
gcc pthreadOverhead.c -pthread -o pthreadOverhead -O0
gcc readTimeOverhead.c -o readTimeOverhead -O0
gcc syscallOverhead.c -o syscallOverhead -O0
gcc pthreadContextSwitch.c -pthread -o pthreadContextSwitch -O0
gcc processContextSwitch.c -o processContextSwitch -O0
