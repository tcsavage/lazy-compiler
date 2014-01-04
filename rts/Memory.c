#include "Memory.h"

void* wrappedMalloc(size_t size, const char *file, int line, const char *func) {
    void *p = malloc(size);
    printf ("Allocated = %s, %i, %s, %p[%d]\n", file, line, func, p, size);

    return p;
}

void wrappedFree(void *p, const char *file, int line, const char *func) {
    free(p);
    printf ("Freed = %s, %i, %s, %p\n", file, line, func, p);
}
