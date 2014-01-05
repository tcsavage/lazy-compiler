#include "RTS.h"

// Global lookup table.
Node *gt[%d];

int main(int argc, char *argv[]) {
 initMemoryManager();
%s
 run(global_main_is, gt);
}
