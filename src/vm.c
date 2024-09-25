#include <stdarg.h>
#include <stdio.h>
#include <string.h>

#include "common.h"
#include "compiler.h"
#include "debug.h"
#include "object.h"
#include "memory.h"
#include "vm.h"

VM vm;

static bool
isFalsey(Value value)
{
    return IS_NIL(value) || (IS_BOOL(value) && !AS_BOOL(value));
}

static void
concatenate()
{
    ObjString *b = AS_STRING(pop());
    ObjString *a = AS_STRING(peek(0));

    int lenA = a->length;
    int lenB = b->length;
    int totalLength = lenA + lenB;
    char *str = ALLOCATE(char, totalLength+1);
    memcpy(str, a->chars, lenA);
    memcpy(str + lenA, b->chars, lenB);
    str[totalLength] = '\0';
    ObjString *result = takeString(str, totalLength);

    // NOTE: This was when we made a single allocation for both the string object and its string array.
    // ObjString *result = makeStringConcat(a, b);
    setTop(OBJ_VAL((Obj *)result));
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
#define READ_SHORT() \
    (vm.ip += 2, (u16)((vm.ip[-2] << 8) | vm.ip[-1]))
#define READ_CONSTANT() (vm.chunk->constants.values[READ_BYTE()])
#define READ_STRING() AS_STRING(READ_CONSTANT())
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

            // Pops the top value off the stack and forgets about it. Usually after compiling and parsing an
            // expression statement.
            case OP_POP:        { pop();                    } break;
            // pops 'N' items at once from the stack.
            case OP_POPN:
            {
                u8 popCount = READ_BYTE();
                popN(popCount);
            } break;

            // Global variables are stored in the VM's hashtable. The operand for 'GET' and 'SET' instructions
            // is an index into the constant array, which holds the global variable name. Globals are accessed
            // from the hashtable, not the stack.
            //
            // Local variables' values are stored directly on the stack. The compiler tells the VM which stack
            // slot to use for retrieving or storing the value. The VM and compiler's stacks stay in sync.
            //
            // Even though 'SET_LOCAL' overwrites stack values, global variable values can always be retrieved
            // using 'GET_GLOBAL' since globals are stored in the hashtable and their names in the constants
            // array. The stack can be rebuilt as needed.
            case OP_GET_LOCAL:
            {
                u8 slot = READ_BYTE();
                Value val = vm.stack.values[slot];
                if (IS_NIL(val)) {
                    runtimeError("Trying to access an uninitialized variable");
                    return INTERPRET_RUNTIME_ERROR;
                }

                push(val);
            } break;
            case OP_SET_LOCAL:
            {
                u8 slot = READ_BYTE();
                vm.stack.values[slot] = peek(0);
            } break;

            case OP_DEFINE_GLOBAL:
            {
                // The way the compiler works is that before the OP_DEFINE_GLOBAL instruction is emitted, it emits
                // the constant value that this var represents. so that it already on the stack from the previous
                // iteration of this loop.
                // After this OP_DEFINE_GLOBAL instruction is the index into the constants array where the "name"
                // string of the variable is stored.
                ObjString *globalVarName = READ_STRING();
                // We set the value (that is already on the stack from a previous iteration of this loop) that this
                // variable is representing into a globals hashtable where this value will be stored as a value for
                // the key i.e. the variable name.
                bool isNewKey = tableSet(&vm.globals, globalVarName, peek(0));
                _assert(isNewKey == true);
                // remove the value of this global variable off the top of the stack. since it is guaranteed that
                // there will be no expression that needs to use this value after a definition.
                pop();
            } break;
            case OP_GET_GLOBAL:
            {
                // at this point, if the vm encounters this expression, then it means the program wants to retrieve
                // the value set of a global variable.
                // The next bytecode is the index into the constants array where the name of this global variable
                // is stored. We retrieve that, and then consult the globals hashtable using the global varName as
                // key and get the actual value this global variable represents. We did that in the
                // OP_DEFINE_GLOBALS routine below.
                ObjString *globalVar = READ_STRING();
                Value value;
                if (!tableGet(&vm.globals, globalVar, &value))
                {
                    runtimeError("Undefined variable '%s'.", globalVar->chars);
                    return INTERPRET_RUNTIME_ERROR;
                }

                // push the value of the global variable onto the stack so it can be used by some expression where
                // this getter was called.
                push(value);
            } break;
            case OP_SET_GLOBAL:
            {
                // The global variable name.
                ObjString *name = READ_STRING();

                // Here, we want to set the value of a global variable.
                // the hashtable set function returns true if the key for which the value was to be set, wasn't
                // there and needed to be added. So if we get true, that means the global var name was not already
                // there in the hashtable which is infact a runtime error. Also, we dont want to a new key into the
                // table since it should already be there, so we delete it.
                //
                // IMPORTANT: NOTE: Also note that when the compiler emitted this instruction, the value that this
                // variable should be set to was already emitted before. So the value is already there onto this
                // vm's stack due to the previous iteration of this loop. Note that we don't pop that value.
                // Reason is that assignment is still an expression, so it needs to leave the value onto the stack
                // in case the assignment is nested inside some larger expression.
                if (tableSet(&vm.globals, name, peek(0)))
                {
                    tableDelete(&vm.globals, name);
                    runtimeError("Undefined variable '%s'.", name->chars);
                    return INTERPRET_RUNTIME_ERROR;
                }
            } break;

            case OP_EQUAL:          { BINARY_OP_FUNC(valuesEqual); }    break;
            case OP_NOT_EQUAL:      { BINARY_OP_FUNC(!valuesEqual); }   break;
            case OP_GREATER:        { BINARY_OP_FUNC(valuesGreater); }  break;
            case OP_GREATER_EQUAL:  { BINARY_OP_FUNC(!valuesLess); }    break;
            case OP_LESS:           { BINARY_OP_FUNC(valuesLess); }     break;
            case OP_LESS_EQUAL:     { BINARY_OP_FUNC(!valuesGreater); } break;

            case OP_ADD:
            {
                Value bVal = peek(0);
                Value aVal = peek(1);
                if (IS_NUMBER(bVal) && IS_NUMBER(aVal)) {
                    double b = AS_NUMBER(pop());
                    double a = AS_NUMBER(peek(0));
                    Value res = NUMBER_VAL(b + a);
                    setTop(res);
                } else if (IS_STRING(bVal) && IS_STRING(aVal)) {
                    concatenate();
                } else if (!IS_STRING(aVal) && IS_STRING(bVal)) {
                    ObjString *b = AS_STRING(pop());
                    Obj *strObject = NULL;
                    if (IS_NUMBER(aVal)) {
                        double a = AS_NUMBER(peek(0));
                        strObject = (Obj *)copyStringFormat("%g%s", a, b->chars);
                    } else if (IS_BOOL(aVal)) {
                        bool a = AS_BOOL(aVal);
                        const char *aStr = a ? "True" : "False";
                        strObject = (Obj *)copyStringFormat("%s%s", aStr, b->chars);
                    } else {
                        _assert(!"Should not be here!");
                    }
                    setTop(OBJ_VAL(strObject));
                } else if (IS_STRING(aVal) && !IS_STRING(bVal)) {
                    if (IS_NUMBER(bVal)) {
                        double b = AS_NUMBER(pop());
                        ObjString *a = AS_STRING(peek(0));
                        Obj *strObject = (Obj *)copyStringFormat("%s%g", a->chars, b);
                        setTop(OBJ_VAL(strObject));
                    } else if (IS_BOOL(bVal)) {
                        bool b = AS_BOOL(pop());
                        ObjString *a = AS_STRING(peek(0));
                        Obj *strObject = (Obj *)copyStringFormat("%s%s", a->chars, b ? "True" : "False");
                        setTop(OBJ_VAL(strObject));
                    } else {
                        _assert(!"Should not be here!");
                    }
                } else {
                    runtimeError("Operands must be two numbers or two strings.");
                    return INTERPRET_RUNTIME_ERROR;
                }
            } break;

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

            // We pop the top value of the stack and print it onto the screen when we encounter a print opcode.
            // When the interpreter reaches this instruction, the preceding expression has already been parsed,
            // compiled and the result is already on top of the stack.
            case OP_PRINT:
            {
                printValue(pop());
                printf("\n");
            } break;

            // skip over the 'else' clause of the 'if' statement if it's condition evaluates to true.
            case OP_JUMP:
            {
                u16 offset = READ_SHORT();
                vm.ip += offset;
            } break;

            // skip over the 'then' clause of the 'if' statement if it's condition evaluates to false.
            case OP_JUMP_IF_FALSE:
            {
                // Get the number of bytes to skip if the condition evaluates to false.
                u16 jumpOffset = READ_SHORT();
                // result of the 'if' statement condition at this point is on top of the stack.
                Value ifConditionResult = peek(0);
                // if the 'if' condition is false, skip the 'then' clause of the 'if'. Otherwise, we leave the ip
                // alone and parse the 'then' code following the jump instruction uninterrupted.
                if (isFalsey(ifConditionResult)) {
                    vm.ip += jumpOffset;
                }
                // NOTE: we keep the if condition result there on the stack.
            } break;

            case OP_LOOP:
            {
                u16 offset = READ_SHORT();
                vm.ip -= offset;
            } break;

            case OP_RETURN:
            {
                // Exit the interpreter.
                return INTERPRET_OK;
            }
        }
    }
#undef READ_BYTE
#undef READ_SHORT
#undef READ_CONSTANT
#undef READ_STRING
#undef BINARY_OP
#undef BINARY_OP_FUNC
}

void
initVM()
{
    resetStack();
    vm.objects = NULL;
    initTable(&vm.strings);
    initTable(&vm.globals);
}

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

void
freeVM()
{
    freeTable(&vm.globals);
    freeTable(&vm.strings);

    FREE_ARRAY(Value, vm.stack.values, vm.stack.capacity);
    vm.stack.values = NULL;
    vm.stack.capacity = 0;
    vm.stack.count = 0;

    freeObjects();
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

void
popN(int n)
{
    _assert(vm.stack.count >= n);
    vm.stack.top -= n;
    vm.stack.count -= n;
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