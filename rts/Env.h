#ifndef RTS_ENV_H
#define RTS_ENV_H

#include "Instruction.h"
#include "Stack.h"

typedef struct Env
{
    Instruction *code;
    int offset;
    Stack *stack;
} Env;

Env *mkEnv();
void freeEnv(Env *e);
Env *bindEnv(Env *e);

#endif
