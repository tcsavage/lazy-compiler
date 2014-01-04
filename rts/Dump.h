#ifndef RTS_DUMP_H
#define RTS_DUMP_H

#include "Env.h"

typedef struct Dump
{
    Env **data;
    int head;
    int size;
} Dump;

Dump *mkDump(int size);
void dumpPush();
void dumpRestore();

#endif
