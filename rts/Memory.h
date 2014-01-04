#ifndef RTS_MEMORY_H
#define RTS_MEMORY_H

#include <stdlib.h>
#include <stdio.h>

#include "Dump.h"
#include "Env.h"
#include "Instruction.h"
#include "Node.h"
#include "Stack.h"

void* wrappedMalloc(size_t size, const char *file, int line, const char *func);

void wrappedFree(void *p, const char *file, int line, const char *func);

// Stack.
static int stackSize = 256;
Stack *activeStack;  // Should always point to `activeEnv->stack`.

// Dump.
static int dumpSize = 256;
Dump *dump;

// Env.
Env *activeEnv;

// Globals.
Node **globalTable;

#endif
