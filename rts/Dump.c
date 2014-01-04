#include "Dump.h"

#include "Memory.h"
#include "MemoryDefs.h"

#include "Env.h"
#include "Instruction.h"
#include "Node.h"
#include "Stack.h"

Dump *mkDump(int size) {
    Dump *d = (Dump *) malloc(sizeof(Dump));
    d->data = (Env **) malloc(size);
    d->head = 0;
    d->size = size;
    return d;
}

void dumpPush() {
    // Pop head of old stack.
    Node *head = sPop();

    // Build new environment.
    Env *e = mkEnv();
    e->stack = mkStack(activeStack->size);
    e->code = (Instruction *) malloc(2*sizeof(Instruction));
    e->code[0] = insUnwind();
    e->code[1] = insEnd();
    e->offset = -1;

    // Bind new environment and push old head onto stack.
    Env *old = bindEnv(e);
    sPush(head);

    // Push old head onto dump.
    dump->data[dump->head] = old;
    ++dump->head;
}

void dumpRestore() {
    if (dump->head > 0) {
        Env *old = activeEnv;
        Node *head = sPeek();
        Env *e = dump->data[dump->head-1];
        bindEnv(e);
        dump->data[dump->head-1] = NULL;
        --dump->head;
        sPush(head);
        freeEnv(old);
    }
}
