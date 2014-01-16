#ifndef RTS_H
#define RTS_H

#define UF_UPDATE 1
#define UF_NOUPDATE 2

#define VALUE_ADDR
#define VALUE_PINT

#define CODE_EVAL 1
#define CODE_ENTER 2
#define CODE_RETURNCON 3
#define CODE_RETURNINT 4

#define JUMP(lbl) return lbl  // Can be changed for platform-specific optimisations.
#define ENTER(c) JUMP(**c)  // Can be changed for platform-specific optimisations.

typedef signed   char            StgInt8;
typedef unsigned char            StgWord8;
typedef signed   short           StgInt16;
typedef unsigned short           StgWord16;
typedef signed   int             StgInt32;
typedef unsigned int             StgWord32;
typedef signed   long            StgInt64;
typedef unsigned long            StgWord64;

typedef StgInt32                 StgInt; 
typedef StgWord32                StgWord;
typedef StgInt16                 StgHalfInt;
typedef StgWord16                StgHalfWord;
typedef void*                    StgAddr;

typedef void  *(*(*StgFunPtr)(void))(void);
typedef StgFunPtr StgFun(void);

typedef (CodeLabel)(*CodeLabel)();

typedef struct CInfo
{
    CodeLabel standardEntryCode;
    CodeLabel evacuationCode;
    CodeLabel scavengeCode;
    int pointerCount;
    int valueCount;
} CInfo;

typedef struct Value
{
    int type;
    StgAddr data;
} Value;

typedef struct HeapValue
{
    Closure *closure;
} HeapValue;

typedef struct PrimIntValue
{
    Int value;
} PrimIntValue;

typedef struct UpdateFrame
{
    Stack_Value *argS;  // Old arg stack.
    Stack_Cont *retS;  // Old ret stack.
    Closure *closure;
} UpdateFrame;

// Globals.
Closure *node;
Stack_Value *argS;
Stack_Cont *retS;
Stack_UpdateFrame *updS;

#endif
