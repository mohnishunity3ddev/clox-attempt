#ifndef clox_chunk_h
#define clox_chunk_h

#include "common.h"

typedef enum
{
    OP_RETURN,
} OpCode;

typedef struct
{
    int Count;
    int Capacity;
    uint8_t *Code;
} Chunk;

void initChunk(Chunk *chunk);


#endif