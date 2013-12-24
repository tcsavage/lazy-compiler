#include <stdlib.h>
#include <stdio.h>
#include "RTS.h"

// Stack environment.
int stackSize = 256;
Node **stack;
int stackHead;

// Instruction environment.
Instruction *currInstBase;
int currInstOffset = 0;

Node **globalTable;

Node *mkNodeRef(int type, void *node);
void freeNode(Node *ref);

void doUnwind(Instruction *ins);

void sPush(Node *node) {
    stack[stackHead] = node;
    stackHead++;
}

Node *sPop() {
    if (stackHead > 0) {
        stackHead--;
        Node *tmp = stack[stackHead];
        stack[stackHead] = NULL;
        return tmp;
    } else {
        return NULL;
    }
}

Node *sPeek() {
    if (stackHead > 0) {
        return stack[stackHead-1];
    } else {
        return NULL;
    }
}

Node *sIndex(int i) {
    if (i < 0) {
        i = 0;
    }
    return stack[stackHead-i-1];
}

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
        default:
            printf("Error: Unknown node type.\n");
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

Node *mkNodeInt(int n) {
    NodeInt *ni = (NodeInt *) malloc(sizeof(NodeInt));
    ni->value = n;
    return mkNodeRef(NODE_NUM, (void *) ni);
}

Node *mkNodeAp(Node *l, Node *r) {
    NodeAp *na = (NodeAp *) malloc(sizeof(NodeAp));
    na->lhs = l;
    na->rhs = r;
    return mkNodeRef(NODE_AP, (void *) na);
}

Node *mkNodeGlobal(int arity, Instruction *code) {
    NodeGlobal *ng = (NodeGlobal *) malloc(sizeof(NodeGlobal));
    ng->arity = arity;
    ng->code = code;
    return mkNodeRef(NODE_GLOBAL, (void *) ng);
}

Node *mkNodeRef(int type, void *node) {
    Node *ref = (Node *) malloc(sizeof(Node));
    ref->nodeType = type;
    ref->addr = node;
    return ref;
}

void freeNode(Node *ref) {
    free(ref->addr);
    free(ref);
}

Instruction insEnd() {
    Instruction ins;
    ins.instType = INS_END;
    ins.arg = 0;
    return ins;
}

Instruction insPushGlobal(int n) {
    Instruction ins;
    ins.instType = INS_PUSHGLOBAL;
    ins.arg = n;
    return ins;
}

Instruction insPushInt(int n) {
    Instruction ins;
    ins.instType = INS_PUSHINT;
    ins.arg = n;
    return ins;
}

Instruction insPush(int n) {
    Instruction ins;
    ins.instType = INS_PUSH;
    ins.arg = n;
    return ins;
}

Instruction insMkAp() {
    Instruction ins;
    ins.instType = INS_MKAP;
    ins.arg = 0;
    return ins;
}

Instruction insSlide(int n) {
    Instruction ins;
    ins.instType = INS_SLIDE;
    ins.arg = n;
    return ins;
}

Instruction insUnwind() {
    Instruction ins;
    ins.instType = INS_UNWIND;
    ins.arg = 0;
    return ins;
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
            Node *stackItem = sIndex(ins->arg+1);
            Node *arg = getArg(stackItem);
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
            int i;
            for (i = 0; i < ins->arg; ++i) {
                sPop();
            }
            sPush(x);
            break;
        case INS_UNWIND:
            doUnwind(ins);
            break;
        default:
            printf("Unknown instruction.\n");
    }
}

void doUnwind(Instruction *ins) {
    printf("Unwinding...\n");
    Node *head = sPeek();
    switch (head->nodeType) {
        case NODE_AP:
            printf("AP.\n");
            while (head->nodeType == NODE_AP) {
                NodeAp *ap = (NodeAp *) head->addr;
                sPush(ap->lhs);
                head = ap->lhs;
            }
            doUnwind(ins);
            break;
        case NODE_GLOBAL:
            printf("GLOBAL\n");
            ;
            NodeGlobal *global = (NodeGlobal *) head->addr;
            currInstBase = global->code;
            currInstOffset = -1;
    }
}

int run(Instruction *insStart, Node *globals[]) {
    // Init stack.
    stack = (Node **) malloc(stackSize);
    stackHead = 0;

    globalTable = globals;
    currInstBase = insStart;
    currInstOffset = 0;

    while (currInstBase[currInstOffset].instType != INS_END) {
        printf("%d : %d\n", currInstBase[currInstOffset].instType, currInstBase[currInstOffset].arg);
        decodeAndRun(&currInstBase[currInstOffset]);
        currInstOffset++;
    }

    // Cleanup.
    pdfStack();
    free(stack);
    return 0;
}
