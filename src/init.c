#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> /* for NULL */
#include <R_ext/Rdynload.h>

/* extern definitions */

extern void F77_NAME(dsvdis)(void*,void*,void*,void*,void*,void*,void*,void*,void*);
extern void F77_NAME(euclid)(void*,void*);
extern void F77_NAME(duleg)(void*,void*,void*,void*,void*,void*,void*,void*,void*,
                            void*,void*,void*,void*,void*,void*,void*,void*,void*);
extern void F77_NAME(metric)(void*,void*);
extern void F77_NAME(ismetric)(void*,void*);
extern void F77_NAME(pip)(void*,void*,void*,void*,void*,void*,void*);
extern void F77_NAME(stepdist)(void*,void*);
extern void F77_NAME(thull)(void*,void*,void*,void*,void*,void*,void*,void*,void*,
                           void*,void*);

/* FORTRAN  {"name", MACRO cast, number of arguments}*/

static const R_FortranMethodDef FortranCalls[] = {
    {"dsvdis",   (DL_FUNC) &F77_NAME(dsvdis),    9},
    {"euclid",   (DL_FUNC) &F77_NAME(euclid),    2},
    {"duleg",    (DL_FUNC) &F77_NAME(duleg),    18},
    {"metric",   (DL_FUNC) &F77_NAME(metric),    2},
    {"ismetric", (DL_FUNC) &F77_NAME(ismetric), 18},
    {"pip",      (DL_FUNC) &F77_NAME(pip),       7},
    {"stepdist", (DL_FUNC) &F77_NAME(duleg),     2},
    {"thull",    (DL_FUNC) &F77_NAME(thull),    11},
    {NULL, NULL, 0}
};

/* registration */

void R_init_optpart(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, NULL, FortranCalls, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
