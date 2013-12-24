#include "RTS.h"

// Declare global lookup table.
Node *globalTable[3];

int main(int argc, char *argv[]) {
    Instruction global_I_is[4] = { insPush(0), insSlide(2), insUnwind(), insEnd() };
    Instruction global_K_is[4] = { insPush(0), insSlide(3), insUnwind(), insEnd() };
    Instruction global_S_is[10] = { insPush(2), insPush(2), insMkAp(), insPush(3), insPush(2), insMkAp(), insMkAp(), insSlide(4), insUnwind(), insEnd() };
    Instruction global_main_is[10] = { insPushInt(3), insPushGlobal(0), insPushGlobal(1), insPushGlobal(2), insMkAp(), insMkAp(), insMkAp(), insSlide(1), insUnwind(), insEnd() };

    // Allocate global nodes on the heap.
    Node *global_I_node = mkNodeGlobal(1, global_I_is);
    Node *global_K_node = mkNodeGlobal(2, global_K_is);
    Node *global_S_node = mkNodeGlobal(2, global_S_is);

    // Populate global lookup table.
    globalTable[0] = global_I_node;
    globalTable[1] = global_K_node;
    globalTable[2] = global_S_node;

    run(global_main_is, globalTable);
}
