#ifndef RTS_INSTRUCTION_H
#define RTS_INSTRUCTION_H

#define INS_END -1
#define INS_PUSHGLOBAL 0
#define INS_PUSHINT 1
#define INS_PUSH 2
#define INS_MKAP 3
#define INS_SLIDE 4
#define INS_ALLOC 5
#define INS_UPDATE 6
#define INS_POP 7
#define INS_UNWIND 8
#define INS_EVAL 9
#define INS_ADD 10
#define INS_MUL 11

typedef struct Instruction {
    int instType;
    int arg;
} Instruction;

Instruction insEnd();
Instruction insPushGlobal(int n);
Instruction insPushInt(int n);
Instruction insPush(int n);
Instruction insMkAp();
Instruction insSlide(int n);
Instruction insAlloc(int n);
Instruction insUpdate(int n);
Instruction insPop(int n);
Instruction insUnwind();
Instruction insEval();
Instruction insAdd();
Instruction insMul();

#endif
