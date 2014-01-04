#include "Stack.h"

#include "Memory.h"
#include "MemoryDefs.h"

#include "Node.h"

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
    activeStack->data[activeStack->head] = node;
    activeStack->head++;
}

Node *sPop() {
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
