#include <stdio.h>
#include "debug.h"
#include "value.h"
#include "object.h"

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


    return offset + 4;
}

static int
byteInstruction(const char *name, Chunk *chunk, int offset)
{
    u8 slot = chunk->code[offset + 1];
    printf("%-16s %4d\n", name, slot);
    return offset + 2;
}

static int
jumpInstruction(const char *name, int sign, Chunk *chunk, int offset)
{

    u16 jump = (u16)(chunk->code[offset + 1] << 8);
    jump |= chunk->code[offset + 2];

    printf("%-16s %4d -> %d\n", name, offset, offset + 3 + sign * jump);
    return offset + 3;
}

static int
invokeInstruction(const char *name, Chunk *chunk, int offset)
{
    u8 constant = chunk->code[offset + 1];
    u8 argCount = chunk->code[offset + 2];
    printf("%-16s (%d args) %4d '", name, argCount, constant);
    printValue(chunk->constants.values[constant]);
    printf("'\n");
    return offset + 3;
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
        case OP_CLOSE_UPVALUE:  { result = simpleInstruction("OP_CLOSE_UPVALUE", offset); }              break;
        case OP_RETURN:         { result = simpleInstruction("OP_RETURN", offset); }                     break;

        case OP_JUMP:           { result = jumpInstruction("OP_JUMP", 1, chunk, offset); }               break;
        case OP_JUMP_IF_FALSE:  { result = jumpInstruction("OP_JUMP_IF_FALSE", 1, chunk, offset); }      break;
        case OP_LOOP:           { result = jumpInstruction("OP_LOOP", -1, chunk, offset); }              break;
        case OP_CALL:           { result = byteInstruction("OP_CALL", chunk, offset); }                  break;
        case OP_INVOKE:         { result = invokeInstruction("OP_INVOKE", chunk, offset); }              break;
        case OP_CLOSURE:
        {
            offset++;
            u8 constant = chunk->code[offset++];
            printf("%-16s %4d ", "OP_CLOSURE", constant);
            printValue(chunk->constants.values[constant]);
            printf("\n");
            ObjFunction *function = AS_FUNCTION(chunk->constants.values[constant]);
            for (int j = 0; j < function->upvalueCount; ++j)
            {
                int isLocal = chunk->code[offset++];
                int index = chunk->code[offset++];
                printf("%04d    |                     %s %d \n",
                       offset - 2, isLocal ? "local" : "upvalue", index);
            }
            result = offset;
        } break;

        case OP_CLASS:          { result = constantInstruction("OP_CLASS", chunk, offset); }             break;
        case OP_METHOD:         { result = constantInstruction("OP_METHOD", chunk, offset); }            break;

        case OP_CONSTANT:       { result = constantInstruction("OP_CONSTANT", chunk, offset); }          break;
        case OP_CONSTANT_LONG:  { result = constantLongInstruction("OP_CONSTANT_LONG", chunk, offset); } break;

        case OP_NIL:            { result = simpleInstruction("OP_NIL", offset); }                        break;
        case OP_TRUE:           { result = simpleInstruction("OP_TRUE", offset); }                       break;
        case OP_FALSE:          { result = simpleInstruction("OP_FALSE", offset); }                      break;

        case OP_POP:            { result = simpleInstruction("OP_POP", offset); }                        break;
        case OP_POPN:           { result = byteInstruction("OP_POPN", chunk, offset); }                  break;

        case OP_GET_LOCAL:      { result = byteInstruction("OP_GET_LOCAL", chunk, offset);}              break;
        case OP_SET_LOCAL:      { result = byteInstruction("OP_SET_LOCAL", chunk, offset);}              break;

        case OP_DEFINE_GLOBAL:  { result = constantInstruction("OP_DEFINE_GLOBAL", chunk, offset); }     break;
        case OP_GET_GLOBAL:     { result = constantInstruction("OP_GET_GLOBAL", chunk, offset); }        break;
        case OP_SET_GLOBAL:     { result = constantInstruction("OP_SET_GLOBAL", chunk, offset); }        break;
        case OP_GET_UPVALUE:    { result = byteInstruction("OP_GET_UPVALUE", chunk, offset); }           break;
        case OP_SET_UPVALUE:    { result = byteInstruction("OP_SET_UPVALUE", chunk, offset); }           break;

        case OP_GET_PROPERTY:   { result = constantInstruction("OP_GET_PROPERTY", chunk, offset); }      break;
        case OP_SET_PROPERTY:   { result = constantInstruction("OP_SET_PROPERTY", chunk, offset); }      break;

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

