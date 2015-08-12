/*
 *  A simple blocked implementation of matrix multiply
 *  Provided by Jim Demmel at UC Berkeley
 *  Modified by Scott B. Baden at UC San Diego to
 *    Enable user to select one problem size only via the -n option
 *    Support CBLAS interface
 */

const char* dgemm_desc = "Simple blocked dgemm.";

#if !defined(BLOCK_SIZE)
#define BLOCK_SIZE 192
#define BLOCK_SIZE_2 32
#define BLOCK_SIZE_3 4
#endif

#define min(a,b) (((a)<(b))?(a):(b))
#include <emmintrin.h>

void do_matrix_mult(int lda, double* A, double* B, double* C)
{

/*
	double* C = (double*) malloc(16*sizeof(double));
	int offset = 0;
	for (int j=0; j<4; j++) {
		for (int i=0; i<4; i++) {
			C[j*4+i] = C_t[i+offset];
		}
		offset += lda;
	}

	C[0] = C_t[0];
	C[1] = C_t[1];
	C[2] = C_t[2];
	C[3] = C_t[3];
	int offset = lda;
	C[4] = C_t[offset];
	C[5] = C_t[offset+1];
	C[6] = C_t[offset+2];
	C[7] = C_t[offset+3];
	offset += offset;
	C[8] = C_t[offset];
	C[9] = C_t[offset+1];
	C[10] = C_t[offset+2];
	C[11] = C_t[offset+3];
	offset += offset;
	C[12] = C_t[offset];
	C[13] = C_t[offset+1];
	C[14] = C_t[offset+2];
	C[15] = C_t[offset+3];
*/
	__m128d c1 = _mm_load_pd(C);
	__m128d c2 = _mm_load_pd(C+2);         //likewise, we are loading C[0,2] and C[0,3] here
	__m128d c3 = _mm_load_pd(C+lda);       //likewise, we are loading C[1,0] and C[1,1] here
	__m128d c4 = _mm_load_pd(C+lda+2);     //likewise, we are loading C[1,2] and C[1,3] here
	__m128d c5 = _mm_load_pd(C+2*lda);     //likewise, we are loading C[2,0] and C[2,1] here
	__m128d c6 = _mm_load_pd(C+2*lda+2);   //likewise, we are loading C[2,2] and C[2,3] here
	__m128d c7 = _mm_load_pd(C+3*lda);     //likewise, we are loading C[3,0] and C[3,1] here
	__m128d c8 = _mm_load_pd(C+3*lda +2);   //likewise, we are loading C[3,2] and C[3,3] here

	//__m128d r1, r2, r3, r4, r5, r6, r7, r8;

        for(int l=0; l<4; l+=1)
        {
		__m128d a1 = _mm_load1_pd(A+l);         
		__m128d a2 = _mm_load1_pd(A+l+lda);     
		__m128d a3 = _mm_load1_pd(A+l+2*lda);   
                __m128d a4 = _mm_load1_pd(A+l+3*lda);   
		__m128d b1 = _mm_load_pd(B+l*lda);     
                __m128d b2 = _mm_load_pd(B+l*lda+2);   
		c1 = _mm_add_pd(c1, _mm_mul_pd(a1, b1));
		c2 = _mm_add_pd(c2, _mm_mul_pd(a1, b2));
		c3 = _mm_add_pd(c3, _mm_mul_pd(a2, b1));
		c4 = _mm_add_pd(c4, _mm_mul_pd(a2, b2));
		c5 = _mm_add_pd(c5, _mm_mul_pd(a3, b1));
		c6 = _mm_add_pd(c6, _mm_mul_pd(a3, b2));
		c7 = _mm_add_pd(c7, _mm_mul_pd(a4, b1));
		c8 = _mm_add_pd(c8, _mm_mul_pd(a4, b2));

	}

	_mm_store_pd(C, c1);
	_mm_store_pd(C+2, c2);
	_mm_store_pd(C+lda, c3);
	_mm_store_pd(C+2+lda, c4);
	_mm_store_pd(C+2*lda, c5);
	_mm_store_pd(C+2+2*lda, c6);
	_mm_store_pd(C+3*lda, c7);
	_mm_store_pd(C+2+3*lda, c8);

}

void even_more (int lda, double* A, double* B, double* C) {
for (int i = 0; i < BLOCK_SIZE_2; i += BLOCK_SIZE_3)
                /* For each block-column of B */
                for (int j = 0; j < BLOCK_SIZE_2; j += BLOCK_SIZE_3) {
                        /* Accumulate block dgemms into block of C */
                        for (int k = 0; k < BLOCK_SIZE_2; k += BLOCK_SIZE_3)
                        {
				do_matrix_mult (lda, A + i*lda + k, B + k*lda + j, C + i*lda + j);
			}
		}
}

void more_caching (int lda, double* A, double* B, double* C) {

        for (int i = 0; i < BLOCK_SIZE; i += BLOCK_SIZE_2)
                for (int j = 0; j < BLOCK_SIZE; j += BLOCK_SIZE_2)
                        for (int k = 0; k < BLOCK_SIZE; k += BLOCK_SIZE_2)
                        {
				even_more (lda, A + i*lda + k, B + k*lda + j, C + i*lda + j);
				//do_matrix_mult (lda, A + i*lda + k, B + k*lda + j, C + i*lda + j);
                        }
}

/* This routine performs a dgemm operation
 *  C := C + A * B
 * where A, B, and C are lda-by-lda matrices stored in row-major order
 * On exit, A and B maintain their input values. */  
void square_dgemm (int lda, double* A, double* B, double* C)
{
	/* Align the vectors and pad empty spaces */
	//Find the nearest BLOCK_SIZE_1 limit
	int temp = BLOCK_SIZE;
	while (1) {
		if (temp >= lda) break;
		temp += BLOCK_SIZE;
	}

	double *new_A = (double*) malloc(temp*temp*sizeof(double));
	double *new_B = (double*) malloc(temp*temp*sizeof(double));
	double *new_C = (double*) malloc(temp*temp*sizeof(double));

	int t;
	int tt;
	for (int i=0; i<lda; i++) {
		t = i*temp;
		tt = i*lda;
		//Copy the vectors
		for (int j=0; j<lda; j++) {
			new_A[t+j] = A[tt+j];
		}
		//pad zeros
		for (int j=lda; j<temp; j++) {
			new_A[t+j] = 0;
		}
	}
	for (int i=lda; i<temp; i++) {
		t = i*temp;
		for (int j=0; j<temp; j++){
			new_A[t+j] = 0;
		}
	}

	for (int i=0; i<lda; i++) {
		t = i*temp;
		tt = i*lda;
		//Copy the vectors
		for (int j=0; j<lda; j++) {
			new_B[t+j] = B[tt+j];
		}
		//pad zeros
		for (int j=lda; j<temp; j++) {
			new_B[t+j] = 0;
		}
	}
	for (int i=lda; i<temp; i++) {
		t = i*temp;
		for (int j=0; j<temp; j++){
			new_B[t+j] = 0;
		}
	}

	for (int i=0; i<lda; i++) {
		t = i*temp;
		tt = i*lda;
		//Copy the vectors
		for (int j=0; j<lda; j++) {
			new_C[t+j] = C[tt+j];
		}
		//pad zeros
		for (int j=lda; j<temp; j++) {
			new_C[t+j] = 0;
		}
	}
	for (int i=lda; i<temp; i++) {
		t = i*temp;
		for (int j=0; j<temp; j++){
			new_C[t+j] = 0;
		}
	}

  /* For each block-row of A */ 
  for (int i = 0; i < temp; i += BLOCK_SIZE)
    /* For each block-column of B */
    for (int j = 0; j < temp; j += BLOCK_SIZE)
      /* Accumulate block dgemms into block of C */
      for (int k = 0; k < temp; k += BLOCK_SIZE)
      {
	/* Perform individual block dgemm */
	more_caching(temp, new_A + i*temp + k, new_B + k*temp + j, new_C + i*temp + j);
      }

	for (int i=0; i<lda; i++) {
               //Copy the vectors
		for (int j=0; j<lda; j++) {
			C[i*lda+j] = new_C[i*temp+j];
		}
	}
}
