/*
 *  A simple blocked implementation of matrix multiply
 *  Provided by Jim Demmel at UC Berkeley
 *  Modified by Scott B. Baden at UC San Diego to
 *    Enable user to select one problem size only via the -n option
 *    Support CBLAS interface
 */

//TODO: Make sure that the block size for second level blocking is divisible by 4, this will make things smoother (I think)

const char* dgemm_desc = "Simple blocked dgemm.";

extern int BLOCK_SIZE;
extern int BLOCK_SIZE_2;

#include <emmintrin.h>
#include <stdio.h>

#ifdef __RESTRICT
#define RESTRICT
#else
#define RESTRICT __restrict__
#endif

#if !defined(PRODROMOU)
#define PRODROMOU 0
#define UNROLLING 1
#define INTRINSIC_UNROLL 0
#endif

#if !PRODROMOU

//#if !defined(BLOCK_SIZE)
//#define BLOCK_SIZE 187
//#define BLOCK_SIZE_2 32
//#endif

#else

//#define BLOCK_SIZE 209
//#define BLOCK_SIZE_2 36

#endif

#define min(a,b) (((a)<(b))?(a):(b))

static void do_matrix_mult(int lda, int M, int N, int K, double* A, double* B, double* C)
{
	//TODO: This seg faults when loading from an odd N sized C array!!!
	__m128d c1 = _mm_loadu_pd(C); 		//loads 2-doubles, thus if we are doing a 2x2 we are loading C[0,0] and C[0,1]
	__m128d c2 = _mm_loadu_pd(C+2);		//likewise, we are loading C[0,2] and C[1,3] here
	__m128d c3 = _mm_loadu_pd(C+lda); 	//likewise, we are loading C[1,0] and C[1,1] here
	__m128d c4 = _mm_loadu_pd(C+2+lda); 	//likewise, we are loading C[1,2] and C[1,3] here
	__m128d c5 = _mm_loadu_pd(C+2*lda); 	//likewise, we are loading C[2,0] and C[2,1] here
	__m128d c6 = _mm_loadu_pd(C+2+2*lda); 	//likewise, we are loading C[2,2] and C[2,3] here
	__m128d c7 = _mm_loadu_pd(C+3*lda); 	//likewise, we are loading C[3,0] and C[3,1] here
	__m128d c8 = _mm_loadu_pd(C+2+3*lda); 	//likewise, we are loading C[3,2] and C[3,3] here

#if INTRINSIC_UNROLL
	for(int l=0; l<4; l+=2)
	{
#else
	for(int l=0; l<4; l+=1)
	{ 
#endif
//	printf("done loading.. l:%i\n", l);
		__m128d a1 = _mm_load1_pd(A+l);		// A[0][0]
		__m128d a2 = _mm_load1_pd(A+l+lda); 	// A[1][0]
		__m128d a3 = _mm_load1_pd(A+l+2*lda); 	// A[2][0]
		__m128d a4 = _mm_load1_pd(A+l+3*lda); 	// A[3][0]
//printf("loaded a's\n");
		//TODO: when N is odd..  this is _mm_load_pd( ) this is causing a seg fault!!!!!!
		//	but, this shouldn't be an issue when I get everything fixed.. because this funciton hopefully will only be called when we have 4x4 matrixes..
		__m128d b1 = _mm_loadu_pd(B+l*lda); 		// B[0][0], B[0][1]
		__m128d b2 = _mm_loadu_pd(B+l*lda+2);  	 	// B[0][2], B[0][3]
//printf("loaded b's\n");
#if INTRINSIC_UNROLL
		__m128d a5 = _mm_load1_pd(A+l+1); 	// A[0][1]
		__m128d a6 = _mm_load1_pd(A+l+1+lda); 	// A[1][1]
		__m128d a7 = _mm_load1_pd(A+l+1+2*lda);	// A[2][1]
		__m128d a8 = _mm_load1_pd(A+l+1+3*lda);	// A[3][1]

		__m128d b3 = _mm_loadu_pd(B+(l+1)*lda);   	// B[1][0], B[1][1]
		__m128d b4 = _mm_loadu_pd(B+(1+l)*lda+2);   	// B[1][2], B[1][3]
#endif
		c1 = _mm_add_pd(c1, _mm_mul_pd(a1, b1));
		c2 = _mm_add_pd(c2, _mm_mul_pd(a1, b2));
		c3 = _mm_add_pd(c3, _mm_mul_pd(a2, b1));
		c4 = _mm_add_pd(c4, _mm_mul_pd(a2, b2));
		c5 = _mm_add_pd(c5, _mm_mul_pd(a3, b1));
		c6 = _mm_add_pd(c6, _mm_mul_pd(a3, b2));
		c7 = _mm_add_pd(c7, _mm_mul_pd(a4, b1));
		c8 = _mm_add_pd(c8, _mm_mul_pd(a4, b2));

#if INTRINSIC_UNROLL		
		c1 = _mm_add_pd(c1, _mm_mul_pd(a5, b3));
		c2 = _mm_add_pd(c2, _mm_mul_pd(a5, b4));
		c3 = _mm_add_pd(c3, _mm_mul_pd(a6, b3));
		c4 = _mm_add_pd(c4, _mm_mul_pd(a6, b4));
		c5 = _mm_add_pd(c5, _mm_mul_pd(a7, b3));
		c6 = _mm_add_pd(c6, _mm_mul_pd(a7, b4));
		c7 = _mm_add_pd(c7, _mm_mul_pd(a8, b3));
		c8 = _mm_add_pd(c8, _mm_mul_pd(a8, b4));
#endif
	}
//	printf("storing");
	_mm_storeu_pd(C, c1);
	_mm_storeu_pd(C+2, c2); 
	_mm_storeu_pd(C+lda, c3); 
	_mm_storeu_pd(C+2+lda, c4); 
	_mm_storeu_pd(C+2*lda, c5); 
	_mm_storeu_pd(C+2+2*lda, c6); 
	_mm_storeu_pd(C+3*lda, c7); 
	_mm_storeu_pd(C+2+3*lda, c8); 
//	printf("done storing.\n");
}	

/* This auxiliary subroutine performs a smaller dgemm operation
 *  C := C + A * B
 * where C is M-by-N, A is M-by-K, and B is K-by-N. */
static void do_block (int lda, int M, int N, int K, double* A, double* B, double* C)
{
//	printf("performing matrix multiply on a (%ix%i) * (%i,%i).\n", M, K, K, N);
	//now here we need to actually perform the multiplications!
	int i, j, k;
	i = j = k = 0;
	
	int mK_Lim = K % 4;
	int mM_Lim = M % 4;
	int mN_Lim = N % 4;

	// perform a 4x4 multiplaction on as many row of M and N as possible, if there isn't enough K, take care of this boundary case immediately.
	for(i = 0; i < M-3; i+=4)
		for(j = 0; j < N-3; j+=4) 
		{
			for(k = 0; k < K-3; k+=4)
		        	do_matrix_mult(lda, M, N, K, A + i*lda + k, B + k*lda + j, C + i*lda + j);
			for(k; k < K; k++)
				for(int ii = i; ii < i+4; ii++) 
					for(int jj = j; jj < j+4; jj++)
						C[ii*lda + jj] += A[ii*lda + k] * B[k*lda + jj];
		}		

	
	// take care of the rows in M that weren't able to have a 4x4 multiplication on them!	
	for(i; i < M; i++)
	{
		for(int j2 = 0; j2 < N-mN_Lim; j2++)
		{
			double mC = C[i*lda + j2];
			for(k = 0; k < K; k++)
			{ 
				mC += A[i*lda + k] * B[k*lda + j2];
			}
                        C[i*lda + j2] = mC;
		}
	}

	
	// take care of the collumns in N that weren't able to have a 4x4 multiplication on them!	
	for(j; j < N; j++)
	{
		for(int i2 = 0; i2 < M-mM_Lim; i2++)
		{
			double mC = C[i2*lda + j];
			for(k = 0; k < K; k++)
			{
				mC += A[i2*lda + k] * B[k*lda + j];
			}
                        C[i2*lda + j] = mC;
		}
	}

	// Finally, take care of the bottom right corner.
	for(i = M-mM_Lim; i < M; i++)
	{
		for(j = N - mN_Lim; j < N; j++)
		{
                        double mC = C[i*lda + j]; 
                        for(k = 0; k < K; k++)
                        {   
                                mC += A[i*lda + k] * B[k*lda + j]; 
                        }           
                        C[i*lda + j] = mC; 
		}
	}
	
	// TODO: Look at block loading an entire collumn?? not sure...a
	// TODO: This is breaking when I have exactly a BLOCK_SIZE_2 of 4!! Why...?
	// TODO: This is also breaking as soon as the BLOCK_SIZE_2 is less then the dimension. ie: my indexing isn't exactly correct.. but it's close!
				
				
/*	printf("going into to this loop we have i2: %i, j2: %i, and mk_Lim: %i, mM_Lim: %i, mN_Lim: %i. Further, mM: %i, mN: %i.\n", i, j, mK_Lim, mM_Lim, mN_Lim, M, N);


	for(int k2 = 0; k2 < mK_Lim; k2++)
        {
		printf("taking care of the left over indexes that weren't able to be multiplied by the 4x4. Iteration: %i. Limit: %i\n", k2, mK_Lim); 
		for(int i2 = 0; i2 < M; i2++)
		{
			for(int j2 = 0; j2 < N; j2++)
			{
				printf("C before: %lf, A: %lf, B%lf.\n", C[i2*lda + j2], A[i2*lda + k2], B[k2*lda + j2]);
				C[i2*lda + j2] += A[i2*lda + k2] * B[k2*lda + j2];
			}
		}
						
	}

*/
// old do block, has been replaced...
/*
  int k;

  // F r each row i of A 
  for (int i = 0; i < M; i=i+2)
  { 
    // For :each column j of B  
    for (int j = 0; j < N; j=j+2) 
    {
      // Compute C(i,j) 

#if UNROLLING
	// this is a 2x2 matrix multiplication with intrinsics
	for (k = 0; k < 2; k+=2)
	{
		__m128d c1 = _mm_load_pd(C); //loads 2-doubles, thus if we are doing a 2x2 we are loading C[0,0] and C[0,1]
	 	__m128d c2 = _mm_load_pd(C+lda); //likewise, we are loading C[1,0] and C[1,1] here
		for(int l=0; l<2; l++)
		{
			__m128d a1 = _mm_load1_pd(A+l);
			__m128d a2 = _mm_load1_pd(A+l+lda); 
			__m128d  b = _mm_load_pd(B+l*lda); //TODO: when this is _mm_load_pd( ) this is causing a seg fault!!!!!!
			c1 = _mm_add_pd(c1, _mm_mul_pd( a1, b ) ); //rank-1 update
			c2 = _mm_add_pd(c2, _mm_mul_pd( a2, b ) ); 
		}
		_mm_storeu_pd(C, c1);
		_mm_storeu_pd(C+lda, c2); 
	}
	double cij = C[i*lda+j];

	//for (k = 0; k < K-1; k=k+2)
	//{
	//	cij += A[i*lda+k+0] * B[(k+0)*lda+j];
	//	cij += A[i*lda+k+1] * B[(k+1)*lda+j];
	//} 

	if (k!=K) {

		//PRODROMOU: Substitute k with K-1 (The only loop that was not executed)
		for (int g=0; g<K-k; g++) {
			cij += A[i*lda+(K-1-g)] * B[(K-1-g)*lda+j]; 
		}
	}
#else 
	for (k=0; k<K; k++) 
		cij += A[i*lda+k] * B[k*lda+j];
#endif
      C[i*lda+j] = cij; //TODO: check for write combining here!
    }
  }

*/
}

void more_caching (int lda, int M, int N, int K, double* A, double* B, double* C) {

	int mM, mN, mK;

	/* For each block-row of A */
	for (int i = 0; i < M; i += BLOCK_SIZE_2)
	{
		/* For each block-column of B */
		for (int j = 0; j < N; j += BLOCK_SIZE_2)
		{
			/* Accumulate block dgemms into block of C */
			for (int k = 0; k < K; k += BLOCK_SIZE_2)
			{
				/* Correct block dimensions if block "goes off edge of" the matrix */
				mM = min (BLOCK_SIZE_2, M-i);
				mN = min (BLOCK_SIZE_2, N-j);
				mK = min (BLOCK_SIZE_2, K-k);

//				printf("\nperforming matrix multiply on A(%ix%i) and B(%ix%i) matrixes.\n", mM, mK, mK, mN);
				do_block (lda, mM, mN, mK, A + i*lda + k, B + j + k*lda, C + i*lda + j);
			}
		}
	}	
}
/* This routine performs a dgemm operation
*  C := C + A * B
 * where A, B, and C are lda-by-lda matrices stored in row-major order
 * On exit, A and B maintain their input values. */  
void square_dgemm (int lda, double* A, double* B, double* C)
{

//TODO: transpose B!
//TODO: align memory?
//TODO: TLB misses?
//TODO: Write Combining?



#if PRODROMOU
/*
		int my_lda = 32;
		double my_A [1024];
		double my_B [1024];
		double my_C [1024];

		for (int i=0; i<1024; i++) {
			my_A[i] = i;
			my_B[i] = i;
			my_C[i] = 0;
		}

		for (int i=0; i< 1024; i++) {
			if ((i+1)%32 ==0) printf ("%.1lf;", my_A[i]);
			else printf ("%.1lf,", my_A[i]);
		}
*/

//	int my_lda = 4;
//	double my_A[16] __attribute__ ((aligned(16))) = {2,4,8,2,5,3,1,4,6,2,2,1,4,3,2,1}; // __attribute__ ((aligned(64)));
//	double my_B[16] __attribute__ ((aligned(16))) = {1,2,3,4,2,2,1,3,6,4,5,3,2,1,2,1};
//	double my_C[16] __attribute__ ((aligned(16))) = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};


//	int my_lda = 5;
//	double my_A[25] = {2,4,8,2,5,3,1,4,6,2,2,1,4,3,2,1,3,3,3,3,3,3,3,3,3}; // __attribute__ ((aligned(64)));
//	double my_B[25] = {1,2,3,4,2,2,1,3,6,4,5,3,2,1,2,1,2,2,2,2,2,2,2,2,2};
//	double my_C[25] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};

//	double my_B[25] = {2,3,2,1,3,4,1,1,3,3,8,4,4,3,3,2,6,3,3,3,5,2,2,3,3};
//	double my_A[4] = {1,2,3,4};
//	double my_B[4] = {4,3,2,1};	
//	double my_C[4] = {0,0,0,0};

	int my_lda = 7;
	double my_A[my_lda*my_lda];
	double my_B[my_lda*my_lda];
	double my_C[my_lda*my_lda];
	for(int i = 0; i < my_lda*my_lda; i++)
	{
		my_A[i] = i;
                my_B[i] = i;
                my_C[i] = 0;
        }

        for (int i=0; i< my_lda*my_lda; i++) {
                if ((i+1)%my_lda*my_lda ==0) printf ("%.1lf;", my_A[i]);
                else printf ("%.1lf,", my_A[i]);
        }

	printf("\n");
	
		// For each block-row of A 
		for (int i = 0; i < my_lda; i += BLOCK_SIZE)
			// For each block-column of B 
			for (int j = 0; j < my_lda; j += BLOCK_SIZE) {
				// Accumulate block dgemms into block of C 
				for (int k = 0; k < my_lda; k += BLOCK_SIZE)
				{	
					// Correct block dimensions if block "goes off edge of" the matrix
					int M = min (BLOCK_SIZE, my_lda-i);
					int N = min (BLOCK_SIZE, my_lda-j);
					int K = min (BLOCK_SIZE, my_lda-k);

					//Prodromou: Instead of performing individual block dgemm break even further

					// Perform individual block dgemm
					//do_block(my_lda, M, N, K, my_A + i*my_lda + k, my_B + k*my_lda + j, my_C + i*my_lda + j);
					more_caching(my_lda, M, N, K, my_A + i*my_lda + k, my_B + k*my_lda + j, my_C + i*my_lda + j);
				}
		}

		for (int i=0; i<my_lda * my_lda; i++) {
			if (i%my_lda==0) printf ("\n");
			printf ("%1.lf\t",my_C[i]);
		}
		printf ("\n\n");
		exit(-1);
#else
	  /* For each block-row of A */ 
	  for (int i = 0; i < lda; i += BLOCK_SIZE)
	    /* For each block-column of B */
	    for (int j = 0; j < lda; j += BLOCK_SIZE)
	      /* Accumulate block dgemms into block of C */
	      for (int k = 0; k < lda; k += BLOCK_SIZE)
	      {
		/* Correct block dimensions if block "goes off edge of" the matrix */
		int M = min (BLOCK_SIZE, lda-i);
		int N = min (BLOCK_SIZE, lda-j);
		int K = min (BLOCK_SIZE, lda-k);

		/* Perform individual block dgemm */
		//do_block(lda, M, N, K, A + i*lda + k, B + k*lda + j, C + i*lda + j);
		more_caching(lda, M, N, K, A + i*lda + k, B + k*lda + j, C + i*lda + j);
	      }
#endif
}
