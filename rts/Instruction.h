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

#endif
