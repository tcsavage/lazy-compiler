#include "Instruction.h"

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
