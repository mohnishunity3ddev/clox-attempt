#ifndef clox_chunk_h
#define clox_chunk_h

#include "common.h"
#include "value.h"

typedef enum
{
    /// @brief The 3 bytes succeeding this byte code is the index in the constant pool where the constant is
    /// stored.
    OP_CONSTANT_LONG,
    /// @brief The next byte after this opcode stores the index into which the constant was stored in the constant
    /// pool.
    OP_CONSTANT,
    /// @brief This op code means return from a function.
    OP_RETURN,
} OpCode;

typedef struct
{
    int line;
    int repetition;
} LineInfo;

typedef struct
{
    int count;
    int capacity;
    LineInfo *values;
} LineArray;

/// @brief The structure which groups chunk information
typedef struct
{
    /// @brief The number of bytes in the code
    int count;
    /// @brief The capacity of the code array
    int capacity;
    /// @brief The array which holds all the byte codes
    u8 *code;
    /// @brief The number of lines in this array.
    LineArray lines;
    /// @brief constants array which holds all the constants
    ValueArray constants;
} Chunk;

void initChunk(Chunk *chunk);
void freeChunk(Chunk *chunk);
void writeChunk(Chunk *chunk, u8 byte, int line);

/// @brief Adds a constant to the constants pool
/// @param chunk The chunk which has the constants pool
/// @param value The value to add.
/// @param line The line number where we saw the constant in the code
void writeConstant(Chunk *chunk, Value value, int line);

#endif