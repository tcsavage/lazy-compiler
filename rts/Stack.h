#ifndef RTS_STACK_H
#define RTS_STACK_H

#include "Node.h"

typedef struct Stack
{
    Node **data;
    int head;
    int size;
} Stack;

// Create a new, empty stack.
Stack *mkStack(int size);

// Clone an existing stack and its contents. Stack data is duplicated but the heap objects are not.
Stack *cloneStack(Stack *base);

// Frees a stack (but not items pointed to by stack items).
void freeStack(Stack *stack);

void sPush(Node *node);

Node *sPop();

// Get the address on top of the stack. The same as sIndex(0) but a little faster.
Node *sPeek();

// Get the nth item in the stack. Use sPeek to het the head of the stack instead of sIndex(0).
Node *sIndex(int i);

// Replace the nth item in the stack.
void sReplace(int i, Node *n);

#endif
