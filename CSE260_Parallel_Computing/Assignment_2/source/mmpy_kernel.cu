// Matrix multiply device code
#include <assert.h>
#include <math.h>
#include "utils.h"
#include "types.h"
using namespace std;


#ifndef TYPE 
#define TYPE 6
#endif

#if TYPE >= 4
#define BLOCK_SIZE 16
#else
#define BLOCK_SIZE 32
#endif

#define ASUB(i, j) Asub[i][j]
#define BSUB(i, j) Bsub[i][j]

//NOTE: B MUST NOT be transposed
__global__ void matMulNaive(int N, _DOUBLE_ *C, _DOUBLE_ *A, _DOUBLE_ *B) {

    int I =  blockIdx.y*blockDim.y + threadIdx.y; // Row i of C
    int J =  blockIdx.x*blockDim.x + threadIdx.x; // Row J of C

    if((I < N) && (J < N)){
        _DOUBLE_ _c = 0;
        for (unsigned int k = 0; k < N; k++) {
            _DOUBLE_ a = A[I * N + k];
            _DOUBLE_ b = B[k * N + J];
            _c += a * b;
        }
        C[I * N + J] = _c;
    }
}

//NOTE: B must NOT be transposed
__global__ void matMulTiled(int N, _DOUBLE_ *C, _DOUBLE_ *A, _DOUBLE_ *B) {
    
    // Block && thread index
    int bx = blockIdx.x;
    int by = blockIdx.y;
    int tx = threadIdx.x;
    int ty = threadIdx.y;
    
    // shared partial memory for subPartialroblems of A & B
    __shared__ _DOUBLE_ Asub[BLOCK_SIZE][BLOCK_SIZE];
    __shared__ _DOUBLE_ Bsub[BLOCK_SIZE][BLOCK_SIZE];

    // temp variable
    _DOUBLE_ partialC = 0;

    // pull out termination
    int terminate = (N * BLOCK_SIZE * by) + N - 1;
    int b = BLOCK_SIZE * bx;
    int bInc = BLOCK_SIZE * N;

    // Loop over all the subblocks
    for (int a = N * BLOCK_SIZE * by; a <= terminate;a += BLOCK_SIZE, b += bInc) {

        // Loading the arrays from memory
        ASUB(ty, tx) = A[a + N * ty + tx];
        BSUB(tx, ty) = B[b + N * tx + ty];
        __syncthreads();

        // Perform the multiply
        for (int k = 0; k < BLOCK_SIZE; ++k)
            partialC += ASUB(ty, k) * BSUB(k,tx);
        
        // Wait for all to finish before moving to next iteration 
        __syncthreads();
    }

    // Finally write out the result in c to C in memory
    C[(N * BLOCK_SIZE * by + BLOCK_SIZE * bx) + N * ty + tx] = partialC;
}

//Note: B MUST be transposed
__global__ void matMulCoalesced(int N, _DOUBLE_ *C, _DOUBLE_ *A, _DOUBLE_ *B) {

    // Block && thread index
    int bx = blockIdx.x;
    int by = blockIdx.y;
    int tx = threadIdx.x;
    int ty = threadIdx.y;

    // shared partial memory for subPartialroblems of A & B
    __shared__ _DOUBLE_ Asub[BLOCK_SIZE][BLOCK_SIZE];
    __shared__ _DOUBLE_ Bsub[BLOCK_SIZE][BLOCK_SIZE];

    // temp variable
    _DOUBLE_ partialC = 0;

    // pull out termination
    int terminate = (N * BLOCK_SIZE * by) + N - 1;
    int b = BLOCK_SIZE * bx;
    int bInc = BLOCK_SIZE * N;

    // Loop over all the subblocks
    for (int a = N * BLOCK_SIZE * by; a <= terminate; a += BLOCK_SIZE, b += bInc) {

        // Loading the arrays from memory
        ASUB(ty, tx) = A[a + N * ty + tx];
        BSUB(tx, ty) = B[b + N * ty + tx];
        __syncthreads();
	
	// Perform the multiply
        for (int k = 0; k < BLOCK_SIZE; ++k)
            partialC += ASUB(ty, k) * BSUB(tx,k);

        // Wait for all to finish before moving to next iteration 
        __syncthreads();
    }

    // Finally write out the result in c to C in memory
    C[( N * BLOCK_SIZE * by + BLOCK_SIZE * bx) + N * ty + tx] = partialC;
}

//NOTE: B MUST be transposed
__global__ void matMulBankConflict(int N, _DOUBLE_ *C, _DOUBLE_ *A, _DOUBLE_ *B) {

    // Block && thread index
    int bx = blockIdx.x;
    int by = blockIdx.y;
    int tx = threadIdx.x;
    int ty = threadIdx.y;

    // shared partial memory for subPartialroblems of A & B
    __shared__ _DOUBLE_ Asub[BLOCK_SIZE][BLOCK_SIZE];
    __shared__ _DOUBLE_ Bsub[BLOCK_SIZE][BLOCK_SIZE];

    // temp variable
    _DOUBLE_ partialC = 0;

    // pull out termination
    int terminate = (N * BLOCK_SIZE * by) + N - 1;
    int b = BLOCK_SIZE * bx;
    int bInc = BLOCK_SIZE * N;

    // Loop over all the subblocks
    for (int a = N * BLOCK_SIZE * by; a <= terminate; a += BLOCK_SIZE, b += bInc){

        // Loading the arrays from memory
        ASUB(ty, tx) = A[a + N * ty + tx];
        BSUB(ty, tx) = B[b + N * ty + tx];
        __syncthreads();

        // Perform the multiply
	for (int k = 0; k < BLOCK_SIZE; ++k)
            partialC += ASUB(ty, k) * BSUB(k,tx);
        
        // Wait for all to finish before moving to next iteration 
        __syncthreads();

    }

    // Finally write out the result in c to C in memory
    C[(N * BLOCK_SIZE * by + BLOCK_SIZE * bx) + N * ty + tx] = partialC;
}

//NOTE: B MUST be transposed
__global__ void
matMulOuterProduct(int N, _DOUBLE_* C, _DOUBLE_* A, _DOUBLE_* B)
{
    // Block && thread index
    int bx = blockIdx.x;
    int by = blockIdx.y;
    int tx = threadIdx.x;
    int ty = threadIdx.y;

    // shared partial memory for subPartialroblems of A
    __shared__ _DOUBLE_ Asub[BLOCK_SIZE * BLOCK_SIZE];

    // create a temporary array to store partial C
    _DOUBLE_ cArr[BLOCK_SIZE] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
    
    // pull out termination
    int terminate = (N * BLOCK_SIZE * by) + N -1;
    int b = BLOCK_SIZE * 4 * bx;
    int bInc  = BLOCK_SIZE * N;

    // loop of all sub blocks
    for (int a = N * BLOCK_SIZE * by; a <= terminate; a += BLOCK_SIZE, b += bInc) {
      // load the sub block of A
      for(int i = 0; i < 4; i++){
        Asub[ (i*4+ty) + BLOCK_SIZE * tx] = A[a + N * (i*4+ty) + tx];
      }
      __syncthreads();

      _DOUBLE_ *aPartial = &Asub[0];
      _DOUBLE_ *bPartial = &B[b + BLOCK_SIZE * ty + tx];
     
      // compute the partial sums
      for(int i = 0; i < BLOCK_SIZE; i++){
        _DOUBLE_ bVal = bPartial[0];
        cArr[0] +=  aPartial[0] * bVal;
        cArr[1] +=  aPartial[1] * bVal;
        cArr[2] +=  aPartial[2] * bVal;
        cArr[3] +=  aPartial[3] * bVal;
        cArr[4] +=  aPartial[4] * bVal;
        cArr[5] +=  aPartial[5] * bVal;
        cArr[6] +=  aPartial[6] * bVal;
        cArr[7] +=  aPartial[7] * bVal;
        cArr[8] +=  aPartial[8] * bVal;
        cArr[9] +=  aPartial[9] * bVal;
        cArr[10] +=  aPartial[10] * bVal;
        cArr[11] +=  aPartial[11] * bVal;
        cArr[12] +=  aPartial[12] * bVal;
        cArr[13] +=  aPartial[13] * bVal;
        cArr[14] +=  aPartial[14] * bVal;
        cArr[15] +=  aPartial[15] * bVal;
        aPartial += BLOCK_SIZE;
        bPartial += N;
      }

      // sync all threads
      __syncthreads();
    }

    // store back to memory
    for(int i = 0; i < BLOCK_SIZE; i++){
      C[((N * BLOCK_SIZE * by + 4 * BLOCK_SIZE * bx) + BLOCK_SIZE * ty + tx) + N * i] = cArr[i];
    }

}

//NOTE: B MUST be transposed
__global__ void
matMulPrefetch( int N, _DOUBLE_* C, _DOUBLE_* A, _DOUBLE_* B)
{

    // Block && thread index
    int bx = blockIdx.x;
    int by = blockIdx.y;
    int tx = threadIdx.x;
    int ty = threadIdx.y;

    // shared partial memory for subPartialroblems of A & B
    __shared__ _DOUBLE_ Asub[BLOCK_SIZE * BLOCK_SIZE];
    __shared__ _DOUBLE_ Asub2[BLOCK_SIZE * BLOCK_SIZE];

    _DOUBLE_ *prefetch = Asub;
    _DOUBLE_ *prefetch2 = Asub2;

    _DOUBLE_ cArr[BLOCK_SIZE] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};


    // pull out terminate
    int terminate = (N * BLOCK_SIZE * by) + N -1;
    int b = BLOCK_SIZE * 4 * bx;
    int bInc  = BLOCK_SIZE * N;
    int cStart = N * BLOCK_SIZE * by + 4 * BLOCK_SIZE * bx;

    //prefetch A
      _DOUBLE_ *Aprefetch = &A[(N * BLOCK_SIZE * by) + N * ty +tx];
      _DOUBLE_ *aPartial = &prefetch[ty + BLOCK_SIZE * tx];
      
      for(int i = 0; i < 16; i+=4){
        aPartial[i] = Aprefetch[N * i];
      }
      __syncthreads();

    // loop of all sub blocks
    for (int a = N * BLOCK_SIZE * by; a <= terminate; a += BLOCK_SIZE, b += bInc) {
      // load the sub block of A
      Aprefetch = &A[a + BLOCK_SIZE + N * ty +tx];
      _DOUBLE_ *aPartial2 = &prefetch2[ty + BLOCK_SIZE * tx];

      for(int i = 0; i < 16; i+=4){
        aPartial2[i] = Aprefetch[N * i];
      }

      aPartial = &prefetch[0];
      _DOUBLE_ *bPartial = &B[b + BLOCK_SIZE * ty + tx];

      for(int i = 0; i < BLOCK_SIZE; i++){
        _DOUBLE_ bVal = bPartial[0];
        cArr[0] +=  aPartial[0] * bVal;
        cArr[1] +=  aPartial[1] * bVal;
        cArr[2] +=  aPartial[2] * bVal;
        cArr[3] +=  aPartial[3] * bVal;
        cArr[4] +=  aPartial[4] * bVal;
        cArr[5] +=  aPartial[5] * bVal;
        cArr[6] +=  aPartial[6] * bVal;
        cArr[7] +=  aPartial[7] * bVal;
        cArr[8] +=  aPartial[8] * bVal;
        cArr[9] +=  aPartial[9] * bVal;
        cArr[10] +=  aPartial[10] * bVal;
        cArr[11] +=  aPartial[11] * bVal;
        cArr[12] +=  aPartial[12] * bVal;
        cArr[13] +=  aPartial[13] * bVal;
        cArr[14] +=  aPartial[14] * bVal;
        cArr[15] +=  aPartial[15] * bVal;
        aPartial += BLOCK_SIZE;
        bPartial += N;
      }

      // Synchronize to make sure the matrices are loaded
      __syncthreads();

      // swaPartial Asub and Asub2
      _DOUBLE_ *prefetch_temp = prefetch;
      prefetch = prefetch2;
      prefetch2 = prefetch_temp;

    }

    // Write the block sub-matrix to device memory;
    // each thread writes one element
    _DOUBLE_ *Cpartial = &C[cStart];
    Cpartial += BLOCK_SIZE * ty + tx;
    for(int i=0; i<BLOCK_SIZE; i++){
      Cpartial[0] = cArr[i]; 
      Cpartial += N;
    }

}

//NOTE: B MUST BE transposed
__global__ void
matMulUnroll( int N, _DOUBLE_* C, _DOUBLE_* A, _DOUBLE_* B)
{
    // Block && thread index
    int bx = blockIdx.x;
    int by = blockIdx.y;
    int tx = threadIdx.x;
    int ty = threadIdx.y;

    // shared partial memory for subPartialroblems of A & B
    __shared__ _DOUBLE_ Asub[BLOCK_SIZE * BLOCK_SIZE];
    __shared__ _DOUBLE_ Asub2[BLOCK_SIZE * BLOCK_SIZE];

    _DOUBLE_ *prefetch = Asub;
    _DOUBLE_ *prefetch2 = Asub2;

    _DOUBLE_ cArr[BLOCK_SIZE] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};

    // pull out terminate
    int terminate = (N * BLOCK_SIZE * by) + N - 1;
    int b = BLOCK_SIZE * 4 * bx;
    int bInc  = BLOCK_SIZE * N;
    int cStart = N * BLOCK_SIZE * by + 4 * BLOCK_SIZE * bx;

    // prefetch
      _DOUBLE_ *Aprefetch = &A[(N * BLOCK_SIZE * by) + N * ty +tx];
      _DOUBLE_ *aPartial = &prefetch[ty + BLOCK_SIZE * tx];
#pragma unroll
      for(int i = 0; i < 16; i+=4){
        aPartial[i] = Aprefetch[N * i];
      }
      __syncthreads();

    // loop of all sub blocks
    for (int a = N * BLOCK_SIZE * by; a <= terminate; a += BLOCK_SIZE, b += bInc) {

      Aprefetch = &A[a + BLOCK_SIZE + N * ty +tx];
      _DOUBLE_ *aPartial2 = &prefetch2[ty + BLOCK_SIZE * tx];
#pragma unroll
      for(int i = 0; i < 16; i+=4){
        aPartial2[i] = Aprefetch[N * i];
      }

      aPartial = &prefetch[0];
      _DOUBLE_ *bPartial = &B[b + BLOCK_SIZE * ty + tx];

#pragma unroll
      for(int i = 0; i < BLOCK_SIZE; i++){
        _DOUBLE_ bVal = bPartial[0];
        cArr[0] +=  aPartial[0] * bVal;
        cArr[1] +=  aPartial[1] * bVal;
        cArr[2] +=  aPartial[2] * bVal;
        cArr[3] +=  aPartial[3] * bVal;
        cArr[4] +=  aPartial[4] * bVal;
        cArr[5] +=  aPartial[5] * bVal;
        cArr[6] +=  aPartial[6] * bVal;
        cArr[7] +=  aPartial[7] * bVal;
        cArr[8] +=  aPartial[8] * bVal;
        cArr[9] +=  aPartial[9] * bVal;
        cArr[10] +=  aPartial[10] * bVal;
        cArr[11] +=  aPartial[11] * bVal;
        cArr[12] +=  aPartial[12] * bVal;
        cArr[13] +=  aPartial[13] * bVal;
        cArr[14] +=  aPartial[14] * bVal;
        cArr[15] +=  aPartial[15] * bVal;
        aPartial += BLOCK_SIZE;
        bPartial += N;
      }

      __syncthreads();

      // swap Asub and Asub2
      _DOUBLE_ *prefetch_temp = prefetch;
      prefetch = prefetch2;
      prefetch2 = prefetch_temp;

    }

    //write back to memory
    _DOUBLE_ *Cpartial = &C[cStart];
    Cpartial += BLOCK_SIZE * ty + tx;
#pragma unroll
    for(int i=0; i<BLOCK_SIZE; i++){
      Cpartial[0] = cArr[i]; 
      Cpartial += N;
    }

}
