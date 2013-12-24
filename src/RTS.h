#ifndef RTS_H
#define RTS_H

#define INS_END -1
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

Instruction insEnd();
Instruction insPushGlobal(int n);
Instruction insPushInt(int n);
Instruction insPush(int n);
Instruction insMkAp();
Instruction insSlide(int n);
Instruction insUnwind();

Node *mkNodeInt(int n);
Node *mkNodeAp(Node *l, Node *r);
Node *mkNodeGlobal(int arity, Instruction *code);
Node *mkNodeRef(int type, void *node);

int run(Instruction *insStart, Node **globals);

#endif
