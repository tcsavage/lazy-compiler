#ifndef RTS_LINKEDLIST_H
#define RTS_LINKEDLIST_H

#include "Node.h"

typedef struct ListNode
{
    Node *data;
    struct ListNode *next;
    struct ListNode *prev;
} ListNode;

typedef struct List
{
    ListNode *head;
    ListNode *tail;
} List;

List *mkList();
void freeList(List *l);
char listEmpty(List *l);
ListNode *listInsertAfter(Node *n, ListNode *ln);
ListNode *listInsertHead(Node *n, List *l);
ListNode *listInsertTail(Node *n, List *l);
void listDelete(ListNode *ln);
int listSize(List *l);

#endif
