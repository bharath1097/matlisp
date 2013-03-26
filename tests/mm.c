#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>

int main(int argc, char *argv[]){
  int n;
  if(argc < 2){
    fprintf(stderr, "Expected argument!\n");
    exit(2);
  }
  n = atoi(argv[1]);

  //This doesn't really help.
  /* double *restrict a, *restrict b, *restrict c; */
  double *a, *b, *c;

  a = calloc(n * n, sizeof(double));
  b = calloc(n * n, sizeof(double));
  c = calloc(n * n, sizeof(double));;
  
  int i, j, k, l;
  printf("N: %d\n", n);
  
  for(i = 0; i < n; i++)
    for(j = 0; j < n; j++){
      a[n * i + j] = ((double) i + j) * 1e-3;
      b[n * i + j] = ((double) i + j) * 1e-3;
    }

  double tmp;
  int of_a = 0, of_b = 0, of_c = 0;

  clock_t co, ci;
  ci = clock();

  of_a = 0;
  of_b = 0;
  of_c = 0;
  for(i = 0; i < n; i++){
    for(k = 0; k < n; k++){
      tmp = a[of_a];
      for(j = 0; j < n; j++){
 	c[of_c] += tmp  * b[of_b];
 	of_c += 1;
 	of_b += 1;
      }
      of_c -= n;
      of_a += 1;
    }
    of_c += n;
    of_b = 0;
  }  
  co = clock();

  //GCC is lazy!
  FILE *fdump = fopen("/dev/null", "w");
  fprintf(fdump, "%lf\n", c[n * (n - 1) + (n - 1)]);
  fclose(fdump);
  
  fprintf(stdout, "Time: %lf\n", (double) (co - ci) / CLOCKS_PER_SEC);
  free(a); free(b); free(c);
}
