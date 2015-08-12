// Process command line arguments
// 
// Do not change the code in this file, as doing so
// could cause your submission to be graded incorrectly
//

#include <stdlib.h> // For: exit, drand48, malloc, free, NULL, EXIT_FAILURE
#include <stdio.h>  // For: perror
#include <string.h> // For: memset
#include <getopt.h>

void cmdLine(int argc, char *argv[], int* n, int* bs1, int* bs2){
/// Command line arguments
 // Default value of the matrix size
 static struct option long_options[] = {
        {"n", required_argument, 0, 'n'},
        {"s", required_argument, 0, 's'},
        {"t", required_argument, 0, 't'},
    };

 // Set default values
    *n=0;
    // Process command line arguments
 int ac;
 for(ac=1;ac<argc;ac++) {
    int c;
    while ((c=getopt_long(argc,argv,"n:s:t:",long_options,NULL)) != -1){
        switch (c) {

	    // Size of the matrix
            case 'n':
                *n = atoi(optarg);
                break;

            case 's':
                *bs1 = atoi(optarg);
		break;

	    case 't':
		*bs2 = atoi(optarg);
		break;

	    // Error
            default:
                printf("Usage: mmpy [-n <matrix dim>]\n");
                exit(-1);
            }
    }
 }

}
