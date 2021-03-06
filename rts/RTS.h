#ifndef RTS_H
#define RTS_H

#include "setjmp.h"

#define UF_UPDATE 1
#define UF_NOUPDATE 2

#define VALUE_ADDR 1
#define VALUE_PINT 2

#define CODE_EVAL 1
#define CODE_ENTER 2
#define CODE_RETURNCON 3
#define CODE_RETURNINT 4

// Jump to a specified code label (function pointer).
#define JUMP(lbl) return (StgFunPtr)lbl  // Can be changed for platform-specific optimisations.

// Enter the given closure by JUMPing to its standard entry code.
#define ENTER(c) JUMP(*((StgAddr *) *c))  // Can be changed for platform-specific optimisations.

// Throw an error if this is executed.
#define UNREACHABLE(name) printf("Error: Supposedly unreachable code (%s) has been executed. Congratulations!\nFunction: %s\nFile: %s\nLine: %d\n", name, __FUNCTION__ , __FILE__ , __LINE__ )

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

// Statically-defined table shared for all closure instances.
typedef struct CInfo
{
    StgFunPtr standardEntryCode;
    StgFunPtr evacuationCode;
    StgFunPtr scavengeCode;
    int pointerCount;
    int valueCount;
} CInfo;

// A "value" can either be a primitive value (like a machine int) or a pointer to a closure allocated on the heap.
typedef struct Value
{
    int type;
    union {
        StgAddr closure;
        StgInt primInt;
    } data;
} Value;

// Allocate and initialize a new machine int value.
Value *mkValueInt(StgInt data);

// Allocate and initialize a new closure value.
Value *mkValueAddr(StgAddr data);

// Dump a value's type and associated data to the console (for debugging).
void printValue(Value *val);

// When an updatable closure is entered, it pushes an update frame onto upd, and clears the stacks.
typedef struct UpdateFrame
{
    StgAddr *sA;  // Old arg stack.
    StgAddr *sB;  // Old control stack.
    StgAddr closure;  // The closure to be updated.
} UpdateFrame;

inline StgAddr popA();
inline StgAddr peekA();
inline void pushA(StgAddr);

inline StgAddr popB();
inline StgAddr peekB();
inline void pushB(StgAddr);

// Allocate space for the stacks and set the values of the stack pointers to their initial positions.
void initStacks(int);

// Free stack memory.
void freeStacks();

inline StgAddr allocClosure(StgWord *);

// Allocate space for a heap and initialize the heap pointer.
void initHeap(int);

// Free heap memory.
void freeHeap();

// Back hole.
StgFunPtr _black_hole_entry();
StgWord _blackHole_info[1];

// Indirection.
StgFunPtr _indirection_entry();
StgWord _indirection_info[1];

// Primitive addition.
StgFunPtr _primIntAdd_entry();

// Primitive multiplication.
StgFunPtr _primIntMul_entry();

// Wrapped integer info table.
StgWord _wrappedInt_info[1];

// Constructor info tables.
StgWord _constructor0_info[1];
StgWord _constructor1_info[1];
StgWord _constructor2_info[1];
StgWord _constructor3_info[1];

// Dump int closure.
StgWord dumpInt_closure[1];

// This is all that should be called in the C main.
int runRTS(StgWord *main_closure, int argc, char const *argv[]);

// Globals.
StgWord *node;  // Current closure. `node[0]` is the info table (StgWord *).
StgAddr *spA;  // Arg stack. Contains values. Grows upwards.
StgAddr *spB;  // COntrol stack. Contains return vectors (continuations) and update frames. Grows downwards.
StgAddr *spALim;  // Marks bottom of stack space.
StgAddr *spBLim;  // Marks top of stack space.
StgAddr *spABase;  // Marks bottom of ACTIVE stack space.
StgAddr *spBBase;  // Marks top of ACTIVE stack space.
StgAddr *hp;  // Heap pointer. Stores closures. Points to the next value to be allocated.
StgAddr *hpLim;  // Marks bottom of heap.
StgAddr *hpBack;  // Second heap pointer (for semi-space GC).
StgAddr *hpBackLim;  // Bottom of second heap (for semi-space GC).
StgInt rTag;  // Constructor tag register.
StgInt retInt;  // Primitive integer return register.
jmp_buf jmpEnv;  // longjmp ref.

#endif
