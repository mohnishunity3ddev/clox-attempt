#include <stdarg.h>
#include <stdio.h>

#include "common.h"
#include "vm.h"
#include "compiler.h"
#include "debug.h"
#include "memory.h"

VM vm;

static bool
isFalsey(Value value)
{
    return IS_NIL(value) || (IS_BOOL(value) && !AS_BOOL(value));
}

static void
resetStack()
{
    vm.stack.top = vm.stack.values;
    vm.stack.count = 0;
}

static void
runtimeError(const char *format, ...)
{
    va_list args;
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);
    fputs("\n", stderr);

    size_t offset = vm.ip - vm.chunk->code - 1;
    int line = getLine(vm.chunk, offset);
    fprintf(stderr, "[line %d] in script\n", line);
    resetStack();
}

static InterpretResult
run()
{
#define READ_BYTE() (*vm.ip++)
#define READ_CONSTANT() (vm.chunk->constants.values[READ_BYTE()])
#define BINARY_OP(valueType, op)                                                                                  \
    do                                                                                                            \
    {                                                                                                             \
        if (!IS_NUMBER(peek(0)) || !IS_NUMBER(peek(1)))                                                           \
        {                                                                                                         \
            runtimeError("Operands must be numbers.");                                                            \
            return INTERPRET_RUNTIME_ERROR;                                                                       \
        }                                                                                                         \
        double b = AS_NUMBER(pop());                                                                              \
        Value opResult = valueType(AS_NUMBER(peek(0)) op b);                                                      \
        setTop(opResult);                                                                                         \
    } while (false)
#define BINARY_OP_FUNC(func)                                                                                      \
    do                                                                                                            \
    {                                                                                                             \
        Value b = pop();                                                                                          \
        Value a = peek(0);                                                                                        \
        Value res = BOOL_VAL(func(a, b));                                                                         \
        setTop(res);                                                                                              \
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

            case OP_NIL:        { push(NIL_VAL);            } break;
            case OP_TRUE:       { push(BOOL_VAL(true));     } break;
            case OP_FALSE:      { push(BOOL_VAL(false));    } break;

            case OP_EQUAL:          { BINARY_OP_FUNC(valuesEqual); }    break;
            case OP_NOT_EQUAL:      { BINARY_OP_FUNC(!valuesEqual); }   break;
            case OP_GREATER:        { BINARY_OP_FUNC(valuesGreater); }  break;
            case OP_GREATER_EQUAL:  { BINARY_OP_FUNC(!valuesLess); }    break;
            case OP_LESS:           { BINARY_OP_FUNC(valuesLess); }     break;
            case OP_LESS_EQUAL:     { BINARY_OP_FUNC(!valuesGreater); } break;

            case OP_ADD:        { BINARY_OP(NUMBER_VAL, +); } break;
            case OP_SUBTRACT:   { BINARY_OP(NUMBER_VAL, -); } break;
            case OP_MULTIPLY:   { BINARY_OP(NUMBER_VAL, *); } break;
            case OP_DIVIDE:     { BINARY_OP(NUMBER_VAL, /); } break;

            case OP_NOT:
            {
                Value p = peek(0);
                Value t = BOOL_VAL(isFalsey(p));
                setTop(t);
            } break;

            case OP_NEGATE:
            {
                // if the value at the top of the stack is not an actual number, then we generate a runtime error.
                if (!IS_NUMBER(peek(0))) {
                    runtimeError("Operand must be a number!");
                    return INTERPRET_RUNTIME_ERROR;
                }

                double number = AS_NUMBER(peek(0));
                setTop(NUMBER_VAL(-number));
            } break;

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
#undef BINARY_OP_FUNC
}

void initVM() { resetStack(); }

InterpretResult
interpret(const char *source)
{
    Chunk chunk;
    initChunk(&chunk);

    if (!compile(source, &chunk)) {
        freeChunk(&chunk);
        return INTERPRET_COMPILE_ERROR;
    }

    vm.chunk = &chunk;
    vm.ip = vm.chunk->code;
    InterpretResult result = run();

    freeChunk(&chunk);
    return result;
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
peek(int indexFromLast)
{
    _assert(indexFromLast >= 0 &&
            vm.stack.count > 0 &&
            vm.stack.count >= (indexFromLast + 1));
    return *((vm.stack.top - 1) - indexFromLast);
}

void
setTop(Value value)
{
    if (vm.stack.count > 0)
    {
        *(vm.stack.top - 1) = value;
    }
}