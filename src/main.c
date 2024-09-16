#include <stdio.h>
// #include <test_class/test.h>
#include "common.h"
#include "chunk.h"
#include "debug.h"

int main(int argc, const char **argv)
{
    Chunk chunk;
    initChunk(&chunk);

    writeConstant(&chunk, 1.2, 124);
    writeConstant(&chunk, 1.3, 124);
    writeConstant(&chunk, 1.4, 124);
    writeConstant(&chunk, 1.5, 124);
    writeConstant(&chunk, 1.6, 124);
    writeChunk(&chunk, OP_RETURN, 126);
    writeChunk(&chunk, OP_RETURN, 126);
    writeChunk(&chunk, OP_RETURN, 126);
    writeConstant(&chunk, 1.7, 127);
    writeConstant(&chunk, 1.4, 127);
    writeConstant(&chunk, 321.7, 127);
    writeConstant(&chunk, 12312.7, 127);
    writeChunk(&chunk, OP_RETURN, 133);

    disassembleChunk(&chunk, "test chunk");

    freeChunk(&chunk);
    return 0;
}
