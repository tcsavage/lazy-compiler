#include "Memory.h"

void* wrappedMalloc(size_t size, const char *file, int line, const char *func) {
    void *p = malloc(size);
    printf ("Allocated %d bytes (%p) in %s (%s:%i)\n", size, p, func, file, line);

    return p;
}

void wrappedFree(void *p, const char *file, int line, const char *func) {
    free(p);
    printf ("Freed %p in %s (%s:%i)\n", p, func, file, line);
}
