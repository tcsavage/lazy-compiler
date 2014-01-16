#import "RTS.h"

CodeLabel cont;

void run() {
    while (TRUE) {
        cont = (*cont)();
    }
}
