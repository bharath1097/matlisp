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
  /* double a[] = {1, 2, 3, 4}, */
  /*   b[] = {4, 5, 6, 7}, */
  /*     c[] = {0, 0, 0, 0}; */
  
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
  
  /* for(i = 0; i < n; i++){ */
  /*   for(j = 0; j < n; j++){ */
  /*     tmp = 0.; */
  /*     for(k = 0; k < n; k++){ */
  /* 	/\* if((of_c > n * n) || (of_c < 0) || \ *\/ */
  /* 	/\*    (of_b > n * n) || (of_b < 0) || \ *\/ */
  /* 	/\*    (of_a > n * n) || (of_a < 0)){ *\/ */
  /* 	/\*   fprintf(stderr, "%d %d %d\n", i, j, k); *\/ */
  /* 	/\*   fprintf(stderr, "of_a: %d\nof_b: %d\nof_c: %d\n", of_a, of_b, of_c); *\/ */
  /* 	/\*   exit(2); *\/ */
  /* 	/\* } *\/ */
  /* 	tmp += a[of_a] * b[of_b]; */
  /* 	of_a += 1; // k */
  /* 	of_b += n; // k */
  /*     } */
  /*     c[of_c] += tmp; */
  /*     of_b -= n * n; */
  /*     of_a -= 1 * n; */
  /*     // */
  /*     of_c += 1; // j */
  /*     of_b += 1; // j */
  /*   } */
  /*   of_c -= 1 * n; */
  /*   // */
  /*   of_c += n;//i j */
  /*   of_a += n;//i k */
  /*   of_b = 0;// k j */
  /* } */


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
      /* for(l = 0; l < n * n; l++) */
      /* 	fprintf(stdout, "%lf\t", c[l]); */
      /* fprintf(stdout, "\n", c[l]); */
      of_c -= n;
      of_a += 1;
    }
    of_c += n;
    of_b = 0;
 }

  /* double err = 0.0; */
  /* for(i = 0; i < n * n; i++)     */
  /*   err += fabs(c[i]); */
  /* fprintf(stdout, "err: %lf\n", err); */

  /* for(i = 0; i < n * n; i++)     */
  /*   fprintf(stdout, "%lf\n", c[i]); */
  
  co = clock();

  //GCC is lazy!
  FILE *fdump = fopen("/dev/null", "w");
  fprintf(fdump, "%lf\n", c[n * (n - 1) + (n - 1)]);
  fclose(fdump);
  
  fprintf(stdout, "Time: %lf\n", (double) (co - ci) / CLOCKS_PER_SEC);
  /* FILE *out; */

  /* out = fopen("a", "w"); */
  /* for(i = 0; i < n; i++){ */
  /*   for(j = 0; j < n; j++) */
  /*     fprintf(out, "%lf\t", a[n * i + j]); */
  /*   fprintf(out, "\n"); */
  /* }   */
  /* fclose(out); */
  
  /* out = fopen("c", "w"); */
  /* for(i = 0; i < n; i++){ */
  /*   for(j = 0; j < n; j++) */
  /*     fprintf(out, "%lf\t", c[n * i + j]); */
  /*   fprintf(out, "\n"); */
  /* } */
  /* fclose(out); */

  free(a); free(b); free(c);
}
