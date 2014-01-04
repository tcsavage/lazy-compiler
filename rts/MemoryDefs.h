#ifndef RTS_MEMORY_DEFS_H
#define RTS_MEMORY_DEFS_H

#ifdef DEBUGMM
#define malloc(X) wrappedMalloc(X, __FILE__, __LINE__, __FUNCTION__)
#define free(X) wrappedFree(X, __FILE__, __LINE__, __FUNCTION__)
#endif

#endif
