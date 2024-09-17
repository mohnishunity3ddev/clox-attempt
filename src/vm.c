#include "common.h"
#include "vm.h"
#include <stdio.h>
#include "compiler.h"
#include "debug.h"
#include "memory.h"

VM vm;

static void
resetStack()
{
    vm.stack.top = vm.stack.values;
    vm.stack.count = 0;
}

static InterpretResult
run()
{
#define READ_BYTE() (*vm.ip++)
#define READ_CONSTANT() (vm.chunk->constants.values[READ_BYTE()])
#define BINARY_OP(op)                                                                                             \
    do                                                                                                            \
    {                                                                                                             \
        double b = pop();                                                                                         \
        setTop(peek() op b);                                                                                      \
    } while (false)

    for (;;)
    {
#ifdef DEBUG_TRACE_EXECUTION
        printf("        ");
        for (int i = 0; i < vm.stack.count; i++)
        {
            printf("[ ");
            printValue(vm.stack.values[i]);
            printf(" ]");
        }
        printf("\n");
        int offset = (int)(vm.ip - vm.chunk->code);
        disassembleInstruction(vm.chunk, offset);
#endif

        u8 instruction;
        switch (instruction = READ_BYTE())
        {
            case OP_CONSTANT:
            {
                Value constant = READ_CONSTANT();
                push(constant);
            } break;

            case OP_ADD:        { BINARY_OP(+); } break;
            case OP_SUBTRACT:   { BINARY_OP(-); } break;
            case OP_MULTIPLY:   { BINARY_OP(*); } break;
            case OP_DIVIDE:     { BINARY_OP(/); } break;

            case OP_NEGATE:     { setTop(-peek()); } break;

            case OP_RETURN:
            {
                printValue(pop());
                printf("\n");
                return INTERPRET_OK;
            }
        }
    }
#undef READ_BYTE
#undef READ_CONSTANT
#undef BINARY_OP
}

void initVM() { resetStack(); }

InterpretResult
interpret(Chunk *chunk)
{
    vm.chunk = chunk;
    vm.ip = vm.chunk->code;
    return run();
}

InterpretResult
interpret(const char *source)
{
    compile(source);
    return INTERPRET_OK;
}

void freeVM()
{
    FREE_ARRAY(Value, vm.stack.values, vm.stack.capacity);
    vm.stack.values = NULL;
    vm.stack.capacity = 0;
    vm.stack.count = 0;
}

void
push(Value value)
{
    if (vm.stack.capacity < vm.stack.count + 1)
    {
        int oldCapacity = vm.stack.capacity;
        vm.stack.capacity = GROW_CAPACITY(oldCapacity);

        vm.stack.values = GROW_ARRAY(Value, vm.stack.values, oldCapacity, vm.stack.capacity);
        vm.stack.top = vm.stack.values + vm.stack.count;
    }

    *vm.stack.top = value;
    vm.stack.top++;
    vm.stack.count++;
}

Value
pop()
{
    vm.stack.top--;
    vm.stack.count--;
    return *vm.stack.top;
}

Value
peek()
{
    _assert(vm.stack.count > 0);
    return *(vm.stack.top - 1);
}

void
setTop(Value value)
{
    if (vm.stack.count > 0)
    {
        *(vm.stack.top - 1) = value;
    }
}