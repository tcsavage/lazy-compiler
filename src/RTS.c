#include <stdlib.h>
#include <stdio.h>

#define INS_PUSHGLOBAL 0
#define INS_PUSHINT 1
#define INS_PUSH 2
#define INS_MKAP 3
#define INS_SLIDE 4
#define INS_UNWIND 5

#define NODE_NUM 0
#define NODE_AP 1
#define NODE_GLOBAL 2

typedef struct Instruction {
    int instType;
    int arg;
} Instruction;

typedef struct Node {
    int nodeType;
    void *addr;
} Node;

typedef struct NodeInt {
    int value;
} NodeInt;

typedef struct NodeAp {
    Node *lhs;
    Node *rhs;
} NodeAp;

typedef struct NodeGlobal {
    int arity;
    Instruction *code;
} NodeGlobal;

Node *mkIntNode(int n);
Node *mkNodeRef(int type, void *node);
void freeNode(Node *ref);

int stackSize = 256;

Node **stack;
int stackHead;

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
        return stack[stackHead];
    } else {
        return NULL;
    }
}

Instruction *allocInst() {
    return (Instruction *) malloc(sizeof(Instruction));
}

Instruction *mkInstPushGlobal(int offset) {
    Instruction *i = allocInst();
    i->instType = INS_PUSHGLOBAL;
    i->arg = offset;
    return i;
}

Instruction *mkInstPushInt(int n) {
    Instruction *i = allocInst();
    i->instType = INS_PUSHINT;
    i->arg = n;
    return i;
}

Instruction *mkInstPush(int n) {
    Instruction *i = allocInst();
    i->instType = INS_PUSH;
    i->arg = n;
    return i;
}

Instruction *mkInstMkAp() {
    Instruction *i = allocInst();
    i->instType = INS_MKAP;
    i->arg = 0;
    return i;
}

Instruction *mkInstSlide(int n) {
    Instruction *i = allocInst();
    i->instType = INS_SLIDE;
    i->arg = n;
    return i;
}

Instruction *mkInstUnwind() {
    Instruction *i = allocInst();
    i->instType = INS_UNWIND;
    i->arg = 0;
    return i;
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

int main(int argc, char **argv) {
    stack = (Node **) malloc(stackSize);
    stackHead = 0;

    Node *n1 = mkNodeInt(59);
    sPush(n1);
    Node *n2 = mkNodeInt(2);
    sPush(n2);
    sPush(mkNodeAp(n1, n2));

    pdfStack();

    free(stack);
    return 0;
}
