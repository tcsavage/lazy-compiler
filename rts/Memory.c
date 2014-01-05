#include "Memory.h"

#include <stdio.h>

void* wrappedMalloc(size_t size, const char *file, int line, const char *func) {
    void *p = malloc(size);
    printf ("Allocated %d bytes (%p) in %s (%s:%i)\n", size, p, func, file, line);

    return p;
}

void wrappedFree(void *p, const char *file, int line, const char *func) {
    free(p);
    printf ("Freed %p in %s (%s:%i)\n", p, func, file, line);
}

void initMemoryManager() {
    allocList = mkList();
}

void quitMemoryManager() {
    freeList(allocList);
}

Node *mmAlloc() {
    Node *n = (Node *) malloc(sizeof(Node));
    listInsertTail(n, allocList);
    return n;
}

void gcMark() {
    Env *e;
    Stack *s;
    int i;
    // Loop over active stack.
    for (i = 0; i < activeStack->head; ++i) {
        gcMarkNode(activeStack->data[i]);
    }
    // Loop over dump.
    int j;
    for (i = 0; i < dump->head; ++i) {
        e = dump->data[i];
        s = e->stack;
        // Loop over dumped stack.
        for (j = 0; j < s->head; ++j) {
            gcMarkNode(s->data[j]);
        }
    }
}

void gcMarkNode(Node *node) {
    // Set flag.
    node->visited = visitedFlag;

    // Traverse children. Not all nodes have children.
    switch (node->nodeType) {
        case NODE_AP:
            ;
            NodeAp *nodeAp = (NodeAp *) node->addr;
            gcMarkNode(nodeAp->lhs);
            gcMarkNode(nodeAp->rhs);
            break;
        case NODE_IND:
            ;
            NodeInd *nodeInd = (NodeInd *) node->addr;
            gcMarkNode(nodeInd->ptr);
            break;
    }
}

void gcSweep() {
    if (allocList != NULL) {
        ListNode *ln = allocList->head;
        ListNode *next;
        while (ln != NULL) {
            next = ln->next;
            if (ln->data->nodeType != NODE_GLOBAL && ln->data->visited != visitedFlag) {
                listDelete(ln);
            }
            ln = next;
        }
    }
    visitedFlag = 1 & (~visitedFlag);
}
