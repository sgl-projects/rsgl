#include <R.h>
#define exit(status) Rf_error("Fatal scanner error (exit code %d)", status)
#undef stdout
#undef stderr
#define stdout NULL
#define stderr NULL
