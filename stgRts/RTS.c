#include "RTS.h"
#include "stdlib.h"
#include "stdio.h"
#include "setjmp.h"

StgFunPtr cont;
jmp_buf jmpEnv;

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

static StgWord _blackHole_info[] = {(StgWord)_black_hole_entry};

// An indirection closure stores a value pointer after the info table.
StgFunPtr _indirection_entry() {
    JUMP(node[1]);
}

static StgWord _indirection_info[] = {(StgWord)_indirection_entry};

//////
// Test code
//////

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

//////////

StgFunPtr apply3_entry() {
    node = popA();  // Pop f and set node.
    pushA(peekA());  // Duplicate top of the stack.
    pushA(peekA());  // And again.
    ENTER(node);
}

StgWord apply3_info[] = {(StgWord)apply3_entry};

// apply3 f x = f x x x
// apply3 = {} \n {f,x} -> f {x,x,x}
StgWord apply3_closure[] = {(StgWord)apply3_info};

//////////

StgFunPtr double_entry() {
    // Expects a primitive integer argument on the stack. Puts result into primitive integer return register.
    StgInt x = (StgInt)popA();  // Pop argument from stack.
    retInt = x*2;  // Set integer return register.
    JUMP(((StgAddr *)popB())[0]);  // Pop return vector and jump to the continuation.
}

StgWord double_info[] = {(StgWord)double_entry};

// double x = x*2
StgWord double_closure[] = {(StgWord)double_info};

//////////

StgFunPtr main_x_entry() {
    pushA((StgAddr)5);  // Push primitive integer into stack.
    ENTER(double_closure);
}

StgWord main_x_info[] = {(StgWord)main_x_entry};

//////////

StgFunPtr main_cont() {
    // Execution is finished. Escape the `run` function.
    longjmp(jmpEnv, 1);
}

StgWord main_retvec[] = {(StgWord)main_cont};

StgFunPtr main_entry() {
    // Create main_x_info closure on the heap.
    hp[0] = &main_x_info;

    // Increment hp by size of closure.
    ++hp;

    // Call dumpInt.
    node = dumpInt_closure;
    pushA(&hp[-1]); // Push x. 1 is the size of the closure.
    pushB((StgAddr)main_retvec);  // Push return vector.
    ENTER(node);
}

StgWord main_info[] = {(StgWord)main_entry};

// main = dumpInt (double 5)
// main = let x = {} \u {} -> double {5}
//        in dumpInt {x}
StgWord main_closure[] = {(StgWord)main_info};

//////////

StgFunPtr start() {
    // Enter main closure.
    node = main_closure;
    ENTER(node);
}

int main(int argc, char const *argv[])
{
    // Set-up the environment.
    initStacks(256);
    initHeap(256);
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
