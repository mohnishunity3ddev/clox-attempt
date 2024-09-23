#ifndef clox_vm_h
#define clox_vm_h

#include "chunk.h"
#include "value.h"
#include "table.h"

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
    /// @brief a hashtable of unique strings (interned strings).
    Table strings;
    Obj *objects;
} VM;

typedef enum
{
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR
} InterpretResult;

extern VM vm;

void initVM();
void freeVM();
InterpretResult interpret(const char *source);

void push(Value value);
Value pop();
Value peek(int indexFromLast);
void setTop(Value value);

#endif