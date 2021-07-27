#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

SEXP crs2crs_c_trans_explicit_new(SEXP xy, SEXP use_z, SEXP use_m);

static const R_CallMethodDef CallEntries[] = {
  {"crs2crs_c_trans_explicit_new", (DL_FUNC) &crs2crs_c_trans_explicit_new, 3},
  {NULL, NULL, 0}
};

void R_init_crs2crs(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
