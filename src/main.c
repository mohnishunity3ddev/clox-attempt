#include <stdio.h>
// #include <test_class/test.h>
#include "common.h"
#include "chunk.h"
#include "debug.h"
#include "vm.h"

// -((1.2 + 3.4) / 5.6) = -(4.6 / 5.6)
void arith01_test(Chunk *chunk)
{
    writeConstant(chunk, 1.2, 123);
    writeConstant(chunk, 3.4, 123);
    writeChunk(chunk, OP_ADD, 123);

    writeConstant(chunk, 5.6, 123);
    writeChunk(chunk, OP_DIVIDE, 123);
    writeChunk(chunk, OP_NEGATE, 123);

    writeChunk(chunk, OP_RETURN, 123);
}

// (1 * 2) + 3 = 5
void arith02_test(Chunk *chunk)
{
    writeConstant(chunk, 1, 123);
    writeConstant(chunk, 2, 123);
    writeChunk(chunk, OP_MULTIPLY, 123);

    writeConstant(chunk, 3, 123);
    writeChunk(chunk, OP_ADD, 123);
    writeChunk(chunk, OP_RETURN, 123);
}

// 1 + 2 * 3 = 1 + (2 * 3) = 1 + 6 = 7
void arith03_test(Chunk *chunk)
{
    writeConstant(chunk, 2, 123);
    writeConstant(chunk, 3, 123);
    writeChunk(chunk, OP_MULTIPLY, 123);

    writeConstant(chunk, 1, 123);
    writeChunk(chunk, OP_ADD, 123);
    writeChunk(chunk, OP_RETURN, 123);
}

// 3 - 2 - 1 = 0
void arith04_test(Chunk *chunk)
{
    writeConstant(chunk, 3, 123);
    writeConstant(chunk, 2, 123);
    writeChunk(chunk, OP_SUBTRACT, 123);

    writeConstant(chunk, 1, 123);
    writeChunk(chunk, OP_SUBTRACT, 123);
    writeChunk(chunk, OP_RETURN, 123);
}

// 1. + 2. * 3. - 4. / -5. = (1. + (2. * 3.)) - (4. / -5.);
void arith05_test(Chunk *chunk)
{
    writeConstant(chunk, 1, 123);
    writeConstant(chunk, 2, 123);
    writeConstant(chunk, 3, 123);
    writeChunk(chunk, OP_MULTIPLY, 123);
    writeChunk(chunk, OP_ADD, 123);

    writeConstant(chunk, 4, 123);
    writeConstant(chunk, 5, 123);
    writeChunk(chunk, OP_NEGATE, 123);
    writeChunk(chunk, OP_DIVIDE, 123);

    writeChunk(chunk, OP_SUBTRACT, 123);
    writeChunk(chunk, OP_RETURN, 123);
}


int main(int argc, const char **argv)
{
    initVM();

    Chunk chunk;
    initChunk(&chunk);

    // arith01_test(&chunk);
    // arith02_test(&chunk);
    // arith03_test(&chunk);
    // arith04_test(&chunk);
    arith05_test(&chunk);

    disassembleChunk(&chunk, "test chunk");
    interpret(&chunk);
    freeVM();
    freeChunk(&chunk);
    return 0;
}
