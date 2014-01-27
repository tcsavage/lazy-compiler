#include "RTS.h"

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

StgFunPtr mul_entry() {
    // Expects a primitive integer argument on the stack. Puts result into primitive integer return register.
    StgInt x = (StgInt)popA();  // Pop argument from stack.
    StgInt y = (StgInt)popA();  // Pop argument from stack.
    retInt = x*y;  // Set integer return register.
    JUMP(((StgAddr *)popB())[0]);  // Pop return vector and jump to the continuation.
}

StgWord mul_info[] = {(StgWord)mul_entry};

// mul x y = x*y
StgWord mul_closure[] = {(StgWord)mul_info};

//////////

StgFunPtr double_entry() {
    // Pointfree partial application of mul.
    // For "pointful" version, could pop and immediately push arg off the stack.
    pushA((StgAddr)2);  // Push first arg onto stack.
    ENTER(mul_closure);  // Enter mul.
}

StgWord double_info[] = {(StgWord)double_entry};

// double = mul 2
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

int main(int argc, char const *argv[]) {
    runRTS(main_closure, argc, argv);
}
