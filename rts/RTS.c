#include <stdlib.h>
#include <stdio.h>
#include "RTS.h"

// Stack environment.
typedef struct Stack
{
    Node **data;
    int head;
    int size;
} Stack;

int stackSize = 256;

// Dump.
typedef struct Env
{
    Instruction *code;
    int offset;
    Stack *stack;
} Env;

typedef struct Dump
{
    Env **data;
    int head;
    int size;
} Dump;

int dumpSize = 256;;
Dump *dump;

Env *activeEnv;
Stack *activeStack;  // Should always point to `activeEnv->stack`.

Node **globalTable;

Node *mkNodeRef(int type, void *node);
void freeNode(Node *ref);
void printNode(Node *node);

void doUnwind(Instruction *ins);

void* wrappedMallock(size_t size, const char *file, int line, const char *func)
{

    void *p = malloc(size);
    printf ("Allocated = %s, %i, %s, %p[%d]\n", file, line, func, p, size);

    /*Link List functionality goes in here*/

    return p;
}

#define malloc(X) wrappedMallock( X, __FILE__, __LINE__, __FUNCTION__)

// Create a new, empty stack.
Stack *mkStack(int size) {
    Stack *stack = (Stack *) malloc(sizeof(Stack));
    stack->data = (Node **) malloc(size);
    stack->head = 0;
    stack->size = size;
    return stack;
}

// Clone an existing stack and its contents. Stack data is duplicated but the heap objects are not.
Stack *cloneStack(Stack *base) {
    Stack *stack = mkStack(base->size);
    int i;
    for (i = 0; i < base->head; ++i) {
        stack->data[i] = base->data[i];
    }
    stack->head = base->head;
    return stack;
}

// Frees a stack (but not items pointed to by stack items).
void freeStack(Stack *stack) {
    free(stack->data);
    free(stack);
}

void sPush(Node *node) {
    // printf("Pushing... ");
    // printNode(node);
    activeStack->data[activeStack->head] = node;
    activeStack->head++;
}

Node *sPop() {
    // printf("Pop\n");
    if (activeStack->head > 0) {
        activeStack->head--;
        Node *tmp = activeStack->data[activeStack->head];
        activeStack->data[activeStack->head] = NULL;
        return tmp;
    } else {
        return NULL;
    }
}

Node *sPeek() {
    if (activeStack->head > 0) {
        return activeStack->data[activeStack->head-1];
    } else {
        return NULL;
    }
}

// Get the nth item in the stack.
Node *sIndex(int i) {
    if (i < 0) {
        i = 0;
    }
    return activeStack->data[activeStack->head-i-1];
}

// Replace the nth item in the stack.
void sReplace(int i, Node *n) {
    activeStack->data[activeStack->head-i-1] = n;
}

Env *mkEnv() {
    Env * e = (Env *) malloc(sizeof(Env));
    return e;
}

void freeEnv(Env *e) {
    freeStack(e->stack);
    free(e);
}

Env *bindEnv(Env *e) {
    Env *old = activeEnv;
    activeEnv = e;
    activeStack = activeEnv->stack;
    return old;
}

// Create a new, empty dump.
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

Node *mkNodeInd(Node *ptr) {
    NodeInd *ni = (NodeInd *) malloc(sizeof(NodeInd));
    ni->ptr = ptr;
    return mkNodeRef(NODE_IND, (void *) ni);
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

Instruction insAlloc(int n) {
    Instruction ins;
    ins.instType = INS_ALLOC;
    ins.arg = n;
    return ins;
}

Instruction insUpdate(int n) {
    Instruction ins;
    ins.instType = INS_UPDATE;
    ins.arg = n;
    return ins;
}

Instruction insPop(int n) {
    Instruction ins;
    ins.instType = INS_POP;
    ins.arg = n;
    return ins;
}

Instruction insUnwind() {
    Instruction ins;
    ins.instType = INS_UNWIND;
    ins.arg = 0;
    return ins;
}

Instruction insEval() {
    Instruction ins;
    ins.instType = INS_EVAL;
    ins.arg = 0;
    return ins;
}

Instruction insAdd() {
    Instruction ins;
    ins.instType = INS_ADD;
    ins.arg = 0;
    return ins;
}

Instruction insMul() {
    Instruction ins;
    ins.instType = INS_MUL;
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
    }

    // Cleanup.
    pdfStack();
    freeEnv(activeEnv);
    return 0;
}
