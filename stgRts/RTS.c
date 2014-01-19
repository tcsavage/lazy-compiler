#include "RTS.h"
#include "stdlib.h"
#include "stdio.h"

StgFunPtr cont;

void run() {
    while (1) {
        cont = (StgFunPtr)(cont)();
    }
}

void initStacks(int size) {
    size_t realsize = size*sizeof(StgAddr);
    spALim = spABase = spA = malloc(realsize);
    spBLim = spBBase = spB = spALim + realsize;
}

void freeStacks() {
    free(spALim);
}

inline StgAddr allocClosure(StgWord *closure) {

}

void initHeap(int size) {
    size_t realsize = size*sizeof(StgAddr);
    hpLim = hp = malloc(realsize);
}

void freeHeap() {
    free(hpLim);
}

inline StgAddr popA() {
    --spA;
    return spA[0];
}

inline StgAddr peekA() {
    return spA[-1];
}

inline void pushA(StgAddr v) {
    spA[0] = v;
    ++spA;
}

inline StgAddr popB() {
    ++spB;
    return spB[0];
}

inline StgAddr peekB() {
    return spB[1];
}

inline void pushB(StgAddr v) {
    spB[0] = v;
    --spB;
}

Value *mkValueInt(StgInt data) {
    Value *val = (Value *) malloc(sizeof(Value));
    val->type = VALUE_PINT;
    val->data.primInt = data;
    return val;
}

Value *mkValueAddr(StgAddr data) {
    Value *val = (Value *) malloc(sizeof(Value));
    val->type = VALUE_ADDR;
    val->data.closure = data;
    return val;
}

void printValue(Value *val) {
    switch (val->type) {
        case VALUE_PINT:
            printf("Int value: %d\n", val->data.primInt);
            break;
        case VALUE_ADDR:
            printf("Closure value: %p\n", val->data.closure);
            break;
        default:
            printf("Unknown value type.\n");
    }
}

StgFunPtr black_hole_entry() {
    printf("Black hole error.\n");
    exit(1);
}

//////
// Test code
//////

void dumpInt(StgAddr x) {
    printf("dumpInt: %d\n", (StgInt)x);
    exit(0);
}

StgFunPtr h() {
    printf("In h\n");
    exit(0);
}

StgFunPtr g() {
    printf("In g\n");
    JUMP(h);
}

StgFunPtr f() {
    printf("In f\n");
    JUMP(g);
}

StgFunPtr test_entry() {
    printf("Test entry code. Closure value 1: %d\n", (StgInt)node[1]);
    JUMP(f);
}

StgWord test_info[] = {(StgWord)test_entry};

StgWord test_closure[] = {(StgWord)test_info, 12};

StgFunPtr dumpInt_entry() {
    dumpInt(popA());
}

StgWord dumpInt_info[] = {(StgWord)dumpInt_entry};

StgWord dumpInt_closure[] = {(StgWord)dumpInt_info};

StgFunPtr double_entry() {
    // (U) Argument satisfaction check.
    // Stack overflow check.
    // Heap overflow check.
    // (U) Info pointer update.
    // (U) Update frame construction.

    // Actual code:
    StgInt x = (StgInt)popA();
    pushA((StgAddr)(x*2));
    ENTER(dumpInt_closure);
}

StgWord double_info[] = {(StgWord)double_entry};

// double x = x*2
StgWord double_closure[] = {(StgWord)double_info};

StgFunPtr start() {
    pushA((StgAddr)678);
    node = double_closure;
    ENTER(node);
}

int main(int argc, char const *argv[])
{
    initStacks(256);
    initHeap(256);
    Value *val = mkValueInt(54);
    printValue(val);
    cont = (StgFunPtr)start;
    run();
    freeHeap();
    freeStacks();
    return 0;
}
