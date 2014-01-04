#include "Env.h"

#include "Memory.h"
#include "MemoryDefs.h"

Env *mkEnv() {
    Env *e = (Env *) malloc(sizeof(Env));
    return e;
}

void freeEnv(Env *e) {
    freeStack(e->stack);
    free(e);
}

Env *bindEnv(Env *e) {
    Env *old = activeEnv;
    activeEnv = e;
    activeStack = activeEnv->stack;
    return old;
}
