#include <R.h>
#include <Rinternals.h>

SEXP add(SEXP x, SEXP y) {
  int i, n = length(x);
  SEXP z = PROTECT(allocVector(REALSXP, n));
  double *rx = REAL(x), *ry = REAL(y), *rz = REAL(z);
  for (i = 0; i < n; i++) rz[i] = rx[i] + ry[i];
  UNPROTECT(1);
  return z;
}