#include <stdlib.h>
#include "chunk.h"
#include "memory.h"

void
initLinesArray(LineArray *lines)
{
    lines->count = 0;
    lines->capacity = 0;
    lines->values = NULL;
}

void
writeLineArray(LineArray *lineArray, int value)
{
    if (value != -1)
    {
        if (lineArray->count > 0 &&
            lineArray->values[lineArray->count - 1].line == value)
        {
            ++lineArray->values[lineArray->count-1].repetition;
        }
        else
        {
            if (lineArray->capacity < lineArray->count + 1)
            {
                int oldCapacity = lineArray->capacity;
                lineArray->capacity = GROW_CAPACITY(oldCapacity);
                lineArray->values = GROW_ARRAY(LineInfo, lineArray->values, oldCapacity, lineArray->capacity);
            }

            lineArray->values[lineArray->count].repetition = 1;
            lineArray->values[lineArray->count].line = value;
            lineArray->count++;
        }

    }
}

void
freeLinesArray(LineArray *lines)
{
    FREE_ARRAY(int, lines->values, lines->capacity);
    initLinesArray(lines);
}

void
initChunk(Chunk *chunk)
{
    chunk->count = 0;
    chunk->capacity = 0;
    chunk->code = NULL;
    initLinesArray(&chunk->lines);
    initValueArray(&chunk->constants);
}

void
freeChunk(Chunk *chunk)
{
    freeLinesArray(&chunk->lines);
    freeValueArray(&chunk->constants);
    FREE_ARRAY(u8, chunk->code, chunk->capacity);
    initChunk(chunk);
}

void
writeChunk(Chunk *chunk, u8 byte, int line)
{
    if (chunk->capacity < chunk->count + 1)
    {
        int oldCapacity = chunk->capacity;
        chunk->capacity = GROW_CAPACITY(oldCapacity);
        chunk->code     = GROW_ARRAY(u8, chunk->code, oldCapacity, chunk->capacity);
    }

    chunk->code[chunk->count] = byte;
    writeLineArray(&chunk->lines, line);
    chunk->count++;
}

void
writeConstant(Chunk *chunk, Value value, int line)
{
    writeValueArray(&chunk->constants, value);

    int index = chunk->constants.count - 1;
    if (index < 256)
    {
        writeChunk(chunk, OP_CONSTANT, line);
        writeChunk(chunk, index, -1);
    }
    else
    {
        // 3 bytes to store index of this constant
        writeChunk(chunk, OP_CONSTANT_LONG, line);
        writeChunk(chunk, (index >> 16) & 0xff, -1); // High 8 bits.
        writeChunk(chunk, (index >> 8) & 0xff, -1); // Middle 8 bits.
        writeChunk(chunk, index & 0xff, -1); // Low 8 bits.
    }

}
