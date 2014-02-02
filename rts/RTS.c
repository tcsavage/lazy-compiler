#include "RTS.h"
#include "stdlib.h"
#include "stdio.h"

StgFunPtr cont;

void run() {
    while (1) {  // Can be replaced with infinite loop (main_cont will need to longjmp to cleanup code).
        cont = (StgFunPtr)(cont)();
    }
    UNREACHABLE("after `run` loop");
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

// Black hole entry code.
StgFunPtr _black_hole_entry() {
    printf("Black hole error.\n");
    exit(1);
}

StgWord _blackHole_info[] = {(StgWord)_black_hole_entry};

// An indirection closure stores a value pointer after the info table.
StgFunPtr _indirection_entry() {
    JUMP(node[1]);
}

StgWord _indirection_info[] = {(StgWord)_indirection_entry};

// Primitive addition.
StgFunPtr _primIntAdd_entry() {
    // Expects a primitive integer argument on the stack. Puts result into primitive integer return register.
    StgInt x = (StgInt)popA();  // Pop argument from stack.
    StgInt y = (StgInt)popA();  // Pop argument from stack.
    retInt = x+y;  // Set integer return register.
    JUMP(((StgAddr *)popB())[0]);  // Pop return vector and jump to the continuation.
}

// Primitive multiplication.
StgFunPtr _primIntMul_entry() {
    // Expects a primitive integer argument on the stack. Puts result into primitive integer return register.
    StgInt x = (StgInt)popA();  // Pop argument from stack.
    StgInt y = (StgInt)popA();  // Pop argument from stack.
    retInt = x*y;  // Set integer return register.
    JUMP(((StgAddr *)popB())[0]);  // Pop return vector and jump to the continuation.
}

// Wrapped integer entry.
StgFunPtr _wrappedInt_entry() {
    // Put enclosed integer into return register.
    retInt = node[1];
    // Jump to continuation.
    JUMP(((StgAddr *)popB())[0]);
}

StgWord _wrappedInt_info[] = {(StgWord)_wrappedInt_entry};

// Constructor 0.
StgFunPtr _constructor0_entry() {
    // Put tag into return register.
    rTag = node[1];
    // Jump to continuation.
    JUMP(((StgAddr *)popB())[0]);
}

StgWord _constructor0_info[] = {(StgWord)_constructor0_entry};

// Constructor 1.
StgFunPtr _constructor1_entry() {
    // Put tag into return register.
    rTag = node[1];
    // Jump to continuation.
    JUMP(((StgAddr *)popB())[0]);
}

StgWord _constructor1_info[] = {(StgWord)_constructor1_entry};

// Constructor 2.
StgFunPtr _constructor2_entry() {
    // Put tag into return register.
    rTag = node[1];
    // Jump to continuation.
    JUMP(((StgAddr *)popB())[0]);
}

StgWord _constructor2_info[] = {(StgWord)_constructor2_entry};

// Constructor 3.
StgFunPtr _constructor3_entry() {
    // Put tag into return register.
    rTag = node[1];
    // Jump to continuation.
    JUMP(((StgAddr *)popB())[0]);
}

StgWord _constructor3_info[] = {(StgWord)_constructor3_entry};

void dumpInt(StgInt x) {
    printf("dumpInt: %d\n", x);
}

StgFunPtr dumpInt_cont() {
    dumpInt(retInt);  // Get the value from the integer return register and dump it.
    JUMP(((StgAddr *)popB())[0]);  // Jump to the continuation in the return vector atop the stack.
}

StgWord dumpInt_retvec[] = {(StgWord)dumpInt_cont};

StgFunPtr dumpInt_entry() {
    node = popA();  // Pop arg.
    pushB((StgAddr)dumpInt_retvec);  // Push return vector.
    ENTER(node);  // Eval the arg.
}

StgWord dumpInt_info[] = {(StgWord)dumpInt_entry};

StgWord dumpInt_closure[] = {(StgWord)dumpInt_info};

StgFunPtr start() {
    // Enter main closure.
    ENTER(node);
}

int runRTS(StgWord *main_closure, int argc, char const *argv[])
{
    // Set-up the environment.
    initStacks(256);
    initHeap(256);
    node = main_closure;
    cont = (StgFunPtr)start;  // Set initial code label to `start`.

    int exiting;  // Variable for tracking longjmp.
    exiting = setjmp(jmpEnv);  // longjmp re-entry point.
    if (!exiting) {
        run();  // Run the mini-interpreter.
        UNREACHABLE("after `run`");
    }

    // Tidy-up and exit.
    freeHeap();
    freeStacks();
    return 0;
}
