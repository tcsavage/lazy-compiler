#include <stdio.h>
#include "Memory.h"
#include "MemoryDefs.h"
#include "RTS.h"

// Stack environment.

void printNode(Node *node);

void doUnwind(Instruction *ins);

void printNode(Node *node) {
    switch (node->nodeType) {
        case NODE_NUM:
            ;
            NodeInt *ni = (NodeInt *) node->addr;
            printf("Integer node: %d\n", ni->value);
            break;
        case NODE_AP:
            printf("Application node\n");
            break;
        case NODE_GLOBAL:
            ;
            NodeGlobal *ng = (NodeGlobal *) node->addr;
            printf("Global node (arity %d)\n", ng->arity);
            break;
        case NODE_IND:
            ;
            NodeInd *nin = (NodeInd *) node->addr;
            printf("Indirection node... ");
            printNode(nin->ptr);
            break;
        default:
            printf("Error: Unknown node type.\n");
            exit(1);
    }
}

void pdfStack() {
    Node *n = sPop();
    while (n != NULL) {
        printNode(n);
        freeNode(n);
        n = sPop();
    }
}

Node *getArg(Node *ap) {
    if (ap->nodeType == NODE_AP) {
        NodeAp *apn = (NodeAp *) ap->addr;
        return apn->rhs;
    } else {
        printf("Tried to get arg of non-function.\n");
        exit(1);
    }
}

void decodeAndRun(Instruction *ins) {
    int i;
    switch (ins->instType) {
        case INS_END:
            printf("Attempted to run END instruction. This shouldn't ever happen.\n");
            exit(1);
            break;
        case INS_PUSHGLOBAL:
            sPush(globalTable[ins->arg]);
            break;
        case INS_PUSHINT:
            ;
            Node *newInt = mkNodeInt(ins->arg);
            sPush(newInt);
            break;
        case INS_PUSH:
            ;
            Node *arg = sIndex(ins->arg);
            sPush(arg);
            break;
        case INS_MKAP:
            ;
            Node *a = sPop();
            Node *b = sPop();
            sPush(mkNodeAp(a, b));
            break;
        case INS_SLIDE:
            ;
            Node *x = sPop();
            for (i = 0; i < ins->arg; ++i) {
                sPop();
            }
            sPush(x);
            break;
        case INS_ALLOC:
            ;
            for (i = 0; i < ins->arg; ++i) {
                sPush(mkNodeInd(NULL));
            }
            break;
        case INS_UPDATE:
            ;
            if (ins->arg > 0) {
                Node *top = sPop();
                sReplace(ins->arg, mkNodeInd(top));
            }
            break;
        case INS_POP:
            ;
            for (i = 0; i < ins->arg; ++i) {
                sPop();
            }
            break;
        case INS_UNWIND:
            doUnwind(ins);
            break;
        case INS_EVAL:
            dumpPush();
            break;
        case INS_ADD:
            ;
            Node *l1 = sPop();
            Node *r1 = sPop();
            if (l1->nodeType == NODE_NUM) {
                NodeInt *li1 = (NodeInt *) l1->addr;
                if (r1->nodeType == NODE_NUM) {
                    NodeInt *ri1 = (NodeInt *) r1->addr;
                    sPush(mkNodeInt(li1->value + ri1->value));
                } else {
                    printf("Tried to perform addition on non-integer node.\n");
                }
            } else {
                printf("Tried to perform addition on non-integer node.\n");
            }
            break;
        case INS_MUL:
            ;
            Node *l2 = sPop();
            Node *r2 = sPop();
            if (l2->nodeType == NODE_NUM) {
                NodeInt *li2 = (NodeInt *) l2->addr;
                if (r2->nodeType == NODE_NUM) {
                    NodeInt *ri2 = (NodeInt *) r2->addr;
                    sPush(mkNodeInt(li2->value * ri2->value));
                } else {
                    printf("Tried to perform multiplication on non-integer node.\n");
                }
            } else {
                printf("Tried to perform multiplication on non-integer node.\n");
            }
            break;
        default:
            printf("Unknown instruction.\n");
            exit(1);
    }
}

// Rearrange the stack.
void rearrange(int n) {
    int i;
    for (i = 0; i < n; ++i) {
        Node *n = sIndex(i+1);
        sReplace(i, getArg(n));
    }
}

void doUnwind(Instruction *ins) {
    // printf("Unwinding...\n");
    Node *head = sPeek();
    switch (head->nodeType) {
        case NODE_NUM:
            dumpRestore();
            break;
        case NODE_AP:
            // printf("AP.\n");
            while (head->nodeType == NODE_AP) {
                NodeAp *ap = (NodeAp *) head->addr;
                sPush(ap->lhs);
                head = ap->lhs;
            }
            doUnwind(ins);
            break;
        case NODE_GLOBAL:
            ;
            // printf("GLOBAL\n");
            NodeGlobal *global = (NodeGlobal *) head->addr;
            rearrange(global->arity);
            activeEnv->code = global->code;
            activeEnv->offset = -1;
            break;
        case NODE_IND:
            ;
            // printf("IND\n");
            NodeInd *ind = (NodeInd *) head->addr;
            sPop();
            sPush(ind->ptr);
            doUnwind(ins);
    }
}

int run(Instruction *insStart, Node *globals[]) {
    // Init dump.
    dump = mkDump(stackSize);

    // Init env.
    activeEnv = mkEnv();
    activeStack = mkStack(stackSize);
    activeEnv->stack = activeStack;
    activeEnv->code = insStart;
    activeEnv->offset = 0;

    globalTable = globals;

    while (activeEnv->code[activeEnv->offset].instType != INS_END) {
        // printf("%d : %d\n", activeEnv->code[activeEnv->offset].instType, activeEnv->code[activeEnv->offset].arg);
        decodeAndRun(&activeEnv->code[activeEnv->offset]);
        activeEnv->offset++;
        // gcMark();
        // gcSweep();
    }

    printNode(sPeek());
    int allocSize = listSize(allocList);
    printf("Final memory usage: %d nodes (%d bytes)\n", allocSize, allocSize*sizeof(Node));

    // Cleanup.
    // pdfStack();
    quitMemoryManager();
    freeEnv(activeEnv);
    return 0;
}
