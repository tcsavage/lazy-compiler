#include "LinkedList.h"

#include "Memory.h"
#include "MemoryDefs.h"

#include "Node.h"

ListNode *mkListNode(Node *n, ListNode *previous, ListNode *next) {
    ListNode *ln = (ListNode *) malloc(sizeof(ListNode));
    ln->data = n;
    ln->prev = previous;
    if (ln->prev != NULL) {
        ln->prev->next = ln;
    }
    ln->next = next;
    if (ln->next != NULL) {
        ln->next->prev = ln;
    }
    return ln;
}

// Doesn't fix links. Used when freeing entire list.
void freeListNode(ListNode *ln) {
    freeNode(ln->data);
    free(ln);
}

List *mkList() {
    List *l = (List *) malloc(sizeof(List));
    l->head = NULL;
    l->tail = NULL;
    return l;
}

void freeList(List *l) {
    if (l->head != NULL) {
        ListNode *ln = l->head;
        ListNode *next;
        while (ln != l->tail) {
            next = ln->next;
            freeListNode(ln);
            ln = next;
        }
        if (ln != NULL) {
            freeListNode(ln);
        }
    }
    free(l);
}

char listEmpty(List *l) {
    return l->head == NULL;
}

ListNode *listInsertAfter(Node *n, ListNode *ln) {
    return mkListNode(n, ln, ln->next);
}

ListNode *listInsertHead(Node *n, List *l) {
    ListNode * ln = mkListNode(n, NULL, l->head);
    l->head = ln;
    if (l->tail == NULL) {
        l->tail = ln;
    }
    return ln;
}

ListNode *listInsertTail(Node *n, List *l) {
    ListNode * ln = mkListNode(n, l->tail, NULL);
    l->tail = ln;
    if (l->head == NULL) {
        l->head = ln;
    }
    return ln;
}

// Delete a list node and link previous and next.
void listDelete(ListNode *ln) {
    if (ln->prev != NULL) {
        ln->prev->next = ln->next;
    }
    if (ln->next != NULL) {
        ln->next->prev = ln->prev;
    }
    freeListNode(ln);
}

int listSize(List *l) {
    ListNode *ln = l->head;
    int count = 0;
    while (ln != NULL) {
        ++count;
        ln = ln->next;
    }
    return count;
}
