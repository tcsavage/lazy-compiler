#ifndef RTS_H
#define RTS_H

#include "Dump.h"
#include "Env.h"
#include "Instruction.h"
#include "Memory.h"
#include "Node.h"
#include "Stack.h"

int run(Instruction *insStart, Node **globals);

#endif
