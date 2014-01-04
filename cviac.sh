#! /bin/sh

./cabal-dev/bin/simplelang -bviac $1

gcc -I rts $1.c rts/RTS.c rts/Dump.c rts/Env.c rts/Instruction.c rts/Node.c rts/Stack.c rts/Memory.c
