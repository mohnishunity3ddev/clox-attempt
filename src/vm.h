#ifndef clox_vm_h
#define clox_vm_h

#include "chunk.h"
#include "value.h"

#define STACK_MAX 256

typedef struct
{
    int count;
    int capacity;
    Value *top;
    Value *values;
} Stack;

typedef struct
{
    /// @brief The chunk which the VM is currently processing.
    Chunk *chunk;

    /// @brief Instruction Pointer - cursor position of the vm telling where it is inside the chunk.
    ///        points to the instruction about to be executed.
    u8 *ip;

    Stack stack;
} VM;

typedef enum
{
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR
} InterpretResult;

void initVM();
void freeVM();
InterpretResult interpret(Chunk *chunk);
InterpretResult interpret(const char *source);

void push(Value value);
Value pop();
Value peek();
void setTop(Value value);

#endif