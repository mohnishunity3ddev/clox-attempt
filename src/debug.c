#include <stdio.h>
#include "debug.h"
#include "value.h"

typedef struct
{
    LineInfo info;
    int index;
} RunningLineInfo;

static void
printLine(Chunk *chunk, int offset, RunningLineInfo *currentLine)
{
    if (offset > 0 && currentLine->info.repetition > 0)
    {
        printf("   | ");
    }
    else
    {
        currentLine->info = chunk->lines.values[currentLine->index];
        printf("%4d ", currentLine->info.line);
    }
    --currentLine->info.repetition;
    if (currentLine->info.repetition <= 0)
        ++currentLine->index;
}

static int
simpleInstruction(const char *name, int offset)
{
    printf("%s\n", name);
    return offset + 1;
}

static int
constantInstruction(const char *name, Chunk *chunk, int offset)
{
    u8 index = chunk->code[offset + 1];
    printf("%-16s %4d '", name, index);
    printValue(chunk->constants.values[index]);
    printf("'\n");

    // One for the instruction identifier is OP_CONSTANT and 1 byte for the actual 1 byte constant value.
    return offset + 2;
}

static int
constantLongInstruction(const char *name, Chunk *chunk, int offset)
{
    u32 index = (chunk->code[offset + 1] << 16);
    index    |= (chunk->code[offset + 2] << 8);
    index    |= (chunk->code[offset + 3]);

    printf("%-16s %4d '", name, index);
    printValue(chunk->constants.values[index]);
    printf("'\n");

    // One for the instruction identifier is OP_CONSTANT and 1 byte for the actual 1 byte constant value.
    return offset + 4;
}

void
disassembleChunk(Chunk *chunk, const char *name)
{
    printf("== %s ==\n", name);

    RunningLineInfo runningLineInfo = {};
    for (int offset = 0; offset < chunk->count; )
    {
        printf("%04d ", offset);
        printLine(chunk, offset, &runningLineInfo);

        offset = disassembleInstruction(chunk, offset);
    }
}

int
disassembleInstruction(Chunk *chunk, int offset)
{
    int result = 0;
    u8 instruction = chunk->code[offset];

    switch(instruction)
    {
        case OP_PRINT:          { result = simpleInstruction("OP_PRINT", offset); }                      break;
        case OP_RETURN:         { result = simpleInstruction("OP_RETURN", offset); }                     break;

        case OP_CONSTANT:       { result = constantInstruction("OP_CONSTANT", chunk, offset); }          break;
        case OP_CONSTANT_LONG:  { result = constantLongInstruction("OP_CONSTANT_LONG", chunk, offset); } break;

        case OP_NIL:            { result = simpleInstruction("OP_NIL", offset); }                        break;
        case OP_TRUE:           { result = simpleInstruction("OP_TRUE", offset); }                       break;
        case OP_FALSE:          { result = simpleInstruction("OP_FALSE", offset); }                      break;
        case OP_POP:            { result = simpleInstruction("OP_POP", offset); }                        break;
        case OP_DEFINE_GLOBAL:  { result = constantInstruction("OP_DEFINE_GLOBAL", chunk, offset); }     break;
        case OP_GET_GLOBAL:     { result = constantInstruction("OP_GET_GLOBAL", chunk, offset); }        break;
        case OP_SET_GLOBAL:     { result = constantInstruction("OP_SET_GLOBAL", chunk, offset); }        break;

        case OP_NEGATE:         { result = simpleInstruction("OP_NEGATE", offset); }                     break;
        case OP_NOT:            { result = simpleInstruction("OP_NOT", offset); }                        break;

        case OP_EQUAL:          { result = simpleInstruction("OP_EQUAL", offset);}                       break;
        case OP_NOT_EQUAL:      { result = simpleInstruction("OP_NOT_EQUAL", offset);}                   break;
        case OP_GREATER:        { result = simpleInstruction("OP_GREATER", offset);}                     break;
        case OP_GREATER_EQUAL:  { result = simpleInstruction("OP_GREATER_EQUAL", offset);}               break;
        case OP_LESS:           { result = simpleInstruction("OP_LESS", offset);}                        break;
        case OP_LESS_EQUAL:     { result = simpleInstruction("OP_LESS_EQUAL", offset);}                  break;

        case OP_ADD:            { result = simpleInstruction("OP_ADD", offset); }                        break;
        case OP_SUBTRACT:       { result = simpleInstruction("OP_SUBTRACT", offset); }                   break;
        case OP_MULTIPLY:       { result = simpleInstruction("OP_MULTIPLY", offset); }                   break;
        case OP_DIVIDE:         { result = simpleInstruction("OP_DIVIDE", offset); }                     break;

        default:
        {
            printf("Unknown opcode %d.\n", instruction);
            result = offset + 1;
        } break;
    }

    return result;
}

