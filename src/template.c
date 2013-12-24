#include "RTS.h"

Node *globalTable[%d];

int main(int argc, char *argv[]) {
%s
 run(global_main_is, globalTable);
}
