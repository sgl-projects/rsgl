#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

void Rf_error(const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    va_end(args);
    fprintf(stderr, "\n");
    abort();
}
