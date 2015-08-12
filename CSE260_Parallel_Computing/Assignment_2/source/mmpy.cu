/*
 * Simplest matrix multiplication in CUDA
 *
 * Scott B. Baden, University of California, San Diego
 * April 2010
 *
 * We compute C = A * B
 *
 * This code assumes that the  matrices are square though there
 * are hooks to facilitate  extending the code to non-square matrices
 *
 */

// system includes
#include <stdio.h>
#include <assert.h>

//  include the kernel
#include "mmpy_kernel.cu"

#include "types.h"
#include "utils.h"

#ifndef TYPE
#define TYPE 6
#endif

// External function definitions
void genMatrix( _DOUBLE_ *a, unsigned int m, unsigned int n);
void genMatrixTranspose( _DOUBLE_ *a, unsigned int m, unsigned int n);
void verify( _DOUBLE_ *c, unsigned int m, unsigned int n, _DOUBLE_ eps, char *mesg);
void verify( _DOUBLE_ *c_d, _DOUBLE_ *c_h,  unsigned int m, unsigned int n, _DOUBLE_ eps, char *mesg);
void printMatrix( _DOUBLE_ *a, unsigned int m, unsigned int n);
void cmdLine(int argc, char *argv[], int& n, int& reps, int& ntx, int& nty, _DOUBLE_ & eps, int& do_host, int& prefer_l1);
void perfString(int n, int ntx, int nty, int reps, double t_h, double gflops_h, double t_d, double gflops_d);
// extern "C"{
    double getTime();
    double gflops(int n, int niter, double time);
//}
void matMulHost(_DOUBLE_ *, const _DOUBLE_ *, const _DOUBLE_ *, unsigned int, unsigned int);
void setGrid(int n, dim3 &blockDim, dim3 &gridDim);

int
main(int argc, char** argv) {
    // To improve repeatabilty of measurements taken on the device,
    // we multiply the number of reps by this scale factor
    // Adjust as needed
    const int SCALE = 10;

// Read in the command line elements
    int n, reps, ntx, nty, do_host, prefer_l1;
    _DOUBLE_ eps;

    cmdLine(argc, argv, n, reps, ntx, nty, eps, do_host, prefer_l1);

   // The thread geometry must evenly divide N
   /*if ((n % ntx != 0) || (n % nty != 0) )
   {
        printf("Thread geometry: %d x %d\n",ntx, nty);
        printf("The length of the thread geometry axis ");
        printf("[ %d x %d]\n",ntx, nty);
        printf("  nust divide N [%d] evenly\n",n);
        exit(-1);
   }
   */

    // Total amount of storage for entries
    unsigned int n2 = n*n*sizeof(_DOUBLE_);

    // Select the fastest device and report characteristics
    int major, minor;
    selectAndReport(&major, &minor);
#ifdef _DOUBLE
    if ((major == 1) && (minor == 2)){
        printf("   You are running on a capability 1.2 device.\n");
        printf("   This code has been compiled with double precision arithmetic.\n");
	printf("   Recompile with single precision.\n\n");
	exit(-1);
    }
#endif

    // setup execution configurations
    int _ntx, _nty;
#if TYPE >= 4
    //_ntx = ntx;
    //_nty = nty;
    _ntx = 16;
    _nty = 4;
#else
    _ntx = 32;
    _nty = 32;
#endif

    dim3 threads(_ntx, _nty,1);
    int numblocksX = n/_ntx;
    int numblocksY = n/_nty;

    if( n % _ntx != 0  )
        numblocksX++;

    if( n % _nty != 0  )
        numblocksY++;

#if TYPE >= 4
    numblocksX = n/64;
    numblocksY = n/16;
    dim3 grid(numblocksX, numblocksY, 1);
#else
    dim3 grid(numblocksX, numblocksY, 1);
    //setGrid(n, threads, grid);
#endif

    // print configurations
    printf("n: %d, tx: %d, ty: %d, gridX: %d, gridY: %d, reps: %d, epsilon: %g\n\n", n, threads.x, threads.y, grid.x, grid.y, reps, eps);

  
#ifndef _DOUBLE
    printf("Using Single precision arithmetic\n\n");
#else
    printf("Using Double precision arithmetic\n\n");
#endif

#if (TYPE == 0)
    printf("Using naive GPU algorithm\n\n");
#elif (TYPE == 1) 
    printf("Using Tiled GPU algorithm\n\n");
#elif (TYPE == 2) 
    printf("Using +Coalesced GPU algorithm\n\n");
#elif (TYPE == 3) 
    printf("Using +NoBankComflicts GPU algorithm\n\n");
#elif (TYPE == 4)
    printf("Using +outerProduct GPU algorithm\n\n");
#elif (TYPE == 5)
    printf("Using +prefetch GPU algorithm\n\n");
#elif (TYPE == 6) 
    printf("Using +unroll GPU algorithm\n\n");
#endif

    if (do_host)
        printf("Doing host computation for comparison\n");

    // allocate an initialize host memory for A and B matrices
    _DOUBLE_ *h_A = (_DOUBLE_ *) malloc(n2);
    assert(h_A);
    _DOUBLE_ *h_B = (_DOUBLE_ *) malloc(n2);
    assert(h_B);
    genMatrix(h_A, n, n);
#if (TYPE == 0 || TYPE == 1)
    genMatrix(h_B, n, n);
#else
    genMatrixTranspose(h_B, n, n);
#endif

    if (n <= 8){
        printf("\nA:\n");
        printMatrix( h_A, n,n);
        printf("\nB:\n");
        printMatrix( h_B, n,n);
    }

    _DOUBLE_  *hostC;
    double t_host=0.0, gflops_h=0.0;
    if (do_host){
        // compute matrix product on the host
        hostC = (_DOUBLE_ *) malloc(n2);
        t_host = -getTime();
        for (int r=0; r< reps; r++)
            matMulHost(hostC, h_A, h_B, n, n);
        t_host += getTime();
        gflops_h = gflops(n, reps, t_host );
        printf("Host computation time: %f sec. [%f gflops]\n",t_host,gflops_h);

        // Verify host result
        verify( hostC,n,n,eps, "Host result");

        if (n <= 8){
            printf("\nC:\n");
            printMatrix( hostC, n,n);
        }
    }

    // allocate device memory
    _DOUBLE_ *d_A, *d_B, *d_C;
    cudaMalloc((void**) &d_A, n2);
    checkCUDAError("Error allocating device memory for matrix A");
    cudaMalloc((void**) &d_B, n2);
    checkCUDAError("Error allocating device memory for matrix B");
    cudaMalloc((void**) &d_C, n2);
    checkCUDAError("Error allocating device memory for matrix C");
    cudaMemset((void **) d_A,-99,n2);
    checkCUDAError("Error initializing device memory matrix A");
    cudaMemset((void **) d_B,-99,n2);
    checkCUDAError("Error initializing device memory matrix B");
    cudaMemset((void **) d_C,0,n2);
    checkCUDAError("Error clearing device memory matrix C");

    // copy host memory to device
    cudaMemcpy(d_A, h_A, n2, cudaMemcpyHostToDevice);
    checkCUDAError("Error copying matrix A to device");
    cudaMemcpy(d_B, h_B, n2, cudaMemcpyHostToDevice);
    checkCUDAError("Error copying matrix B to device");


    // allocate host memory for the result
    _DOUBLE_  *h_C = (_DOUBLE_ *) malloc(n2);
    assert(h_C);


// If we set the preference for L1 cache, rather than
// shared memory, we may run slightly faster on devices that have the capability
    cudaFuncCache Preference;
    if (prefer_l1){
        Preference = cudaFuncCachePreferL1;
    }
    else{
        Preference = cudaFuncCachePreferShared;
    }
#if (TYPE == 0)
    cudaFuncSetCacheConfig(matMulNaive,Preference);
#elif (TYPE == 1)
    cudaFuncSetCacheConfig(matMulTiled,Preference);
#elif (TYPE == 2)
    cudaFuncSetCacheConfig(matMulCoalesced,Preference);
#elif (TYPE == 3)
    cudaFuncSetCacheConfig(matMulBankConflict,Preference);
#elif (TYPE == 4)
    cudaFuncSetCacheConfig(matMulOuterProduct,Preference);
#elif (TYPE == 5)
    cudaFuncSetCacheConfig(matMulPrefetch,Preference);
#elif (TYPE == 6)
    cudaFuncSetCacheConfig(matMulUnroll,Preference);
#endif


    // Start the timer
#ifdef CUDA_TIMER
    cudaEvent_t start_event, stop_event;
    cudaEventCreate(&start_event) ;
    cudaEventCreate(&stop_event);
#endif

#ifdef CUDA_TIMER
    cudaEventRecord(start_event, 0);
    float t_device;
#else
    cudaThreadSynchronize();
    double t_device = -getTime();
#endif

    // execute the kernel
    for (int r=0; r< SCALE*reps; r++) {
#if (TYPE == 0)        
	matMulNaive<<< grid, threads >>>(n, d_C, d_A, d_B);
#elif (TYPE == 1)
        matMulTiled<<< grid, threads >>>(n, d_C, d_A, d_B);
#elif (TYPE == 2)
        matMulCoalesced<<< grid, threads >>>(n, d_C, d_A, d_B);
#elif (TYPE == 3)
        matMulBankConflict<<< grid, threads >>>(n, d_C, d_A, d_B);
#elif (TYPE == 4)
        matMulOuterProduct<<< grid, threads >>>(n, d_C, d_A, d_B);
#elif (TYPE == 5)
        matMulPrefetch<<< grid, threads >>>(n, d_C, d_A, d_B);
#elif (TYPE == 6)
        matMulUnroll<<< grid, threads >>>(n, d_C, d_A, d_B);
#endif
}

#ifdef CUDA_TIMER
    cudaEventRecord(stop_event, 0);
    cudaEventSynchronize(stop_event);
    cudaEventElapsedTime(&t_device, start_event, stop_event);
    t_device /= 1000.0;

#else
    // block until the device has finished
    cudaThreadSynchronize();
    // Stop the timer
    t_device +=getTime();
#endif

    checkCUDAError("Error in matrixMul kernel");

    // copy result from device to host
    cudaMemcpy(h_C, d_C, n2, cudaMemcpyDeviceToHost);
    checkCUDAError("Unable to retrieve result from device");



    double gflops_d = gflops(n, SCALE*reps, t_device );
    printf("Device computation time: %f sec. [%f gflops]\n",t_device,gflops_d);
    perfString(n, ntx, nty, reps, t_host, gflops_h, t_device, gflops_d);

    // Verify the device result
    verify( h_C,n,n,eps, "Device result");

    if (do_host)
        // Compare host and device results
        verify( h_C, hostC, n, n,eps,"Device vs. host");

    // clean up memory
    free(h_A);
    free(h_B);
    free(h_C);
    if (do_host)
        free(hostC);

    assert(cudaSuccess ==cudaFree(d_A));
    assert(cudaSuccess ==cudaFree(d_B));
    assert(cudaSuccess ==cudaFree(d_C));

    cudaThreadExit();
}
