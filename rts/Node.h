#ifndef RTS_NODE_H
#define RTS_NODE_H

#include "Instruction.h"

#define NODE_NUM 0
#define NODE_AP 1
#define NODE_GLOBAL 2
#define NODE_IND 3

typedef struct Node {
    int nodeType;
    void *addr;
    char visited;  // Garbage collection flag.
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

typedef struct NodeInd {
    Node *ptr;
} NodeInd;

Node *mkNodeInt(int n);
Node *mkNodeAp(Node *l, Node *r);
Node *mkNodeGlobal(int arity, Instruction *code);
Node *mkNodeInd(Node *ptr);
Node *mkNodeRef(int type, void *node);
void freeNode(Node *ref);
void printNode(Node *node);

#endif
