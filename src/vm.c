#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include "common.h"
#include "compiler.h"
#include "debug.h"
#include "object.h"
#include "memory.h"
#include "vm.h"

VM vm;

static Value
clockNative(int argCount, Value *args)
{
    Value result = NUMBER_VAL((double)clock() / CLOCKS_PER_SEC);
    return result;
}

static bool callValue(Value callee, int argCount);
static bool call(ObjClosure *closure, int argCount);
static ObjUpvalue *captureUpvalue(Value *local);
CallFrame *mainFrame;

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
    setTop(OBJ_VAL((Obj *)result));
}

static void
resetStack()
{
    vm.stack.top = vm.stack.values;
    vm.stack.count = 0;
    vm.frameCount = 0;
}

static void
runtimeError(const char *format, ...)
{
    va_list args;
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);
    fputs("\n", stderr);
    for (int i = vm.frameCount - 1; i >= 0; i--) {
        CallFrame *frame = &vm.frames[i];
        ObjFunction *function = frame->closure->function;
        size_t instruction = frame->ip - function->chunk.code - 1;
        fprintf(stderr, "[line: %d] in ", function->chunk.lines.values[instruction].line);
        if (function->name == NULL) {
            fprintf(stderr, "main\n");
        } else {
            fprintf(stderr, "%s()\n", function->name->chars);
        }
    }
}

static void
defineNative(const char *name, NativeFunc function)
{
    push(OBJ_VAL((Obj *)copyString(name, (int)strlen(name))));
    push(OBJ_VAL((Obj *)newNative(function)));
    tableSet(&vm.globals, AS_STRING(vm.stack.values[0]), vm.stack.values[1]);
    pop();
    pop();
}

static InterpretResult
run()
{
    CallFrame *frame = &vm.frames[vm.frameCount - 1];

#define READ_BYTE() (*frame->ip++)
#define READ_SHORT() \
    (frame->ip += 2, (u16)((frame->ip[-2] << 8) | frame->ip[-1]))
#define READ_CONSTANT() (frame->closure->function->chunk.constants.values[READ_BYTE()])
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
        int offset = (int)(frame->ip - frame->closure->function->chunk.code);
        disassembleInstruction(&frame->closure->function->chunk, offset);
#endif
        u8 instruction;
        switch (instruction = READ_BYTE())
        {
            case OP_CONSTANT:
            {
                Value constant = READ_CONSTANT();
                push(constant);
            } break;
            // **********************************************************************************
            case OP_NIL:        { push(NIL_VAL);            } break;
            case OP_TRUE:       { push(BOOL_VAL(true));     } break;
            case OP_FALSE:      { push(BOOL_VAL(false));    } break;
            case OP_POP:        { pop();                    } break;
            case OP_POPN:
            {
                u8 popCount = READ_BYTE();
                popN(popCount);
            } break;
            // **********************************************************************************
            case OP_GET_UPVALUE:
            {
                u8 slot = READ_BYTE();
                push(*frame->closure->upvalues[slot]->location);
            } break;
            // **********************************************************************************
            case OP_SET_UPVALUE:
            {
                u8 slot = READ_BYTE();
                *frame->closure->upvalues[slot]->location = peek(0);
            } break;
            // **********************************************************************************
            case OP_GET_LOCAL:
            {
                u8 slot = READ_BYTE();
                Value val = frame->slots[slot];
                if (IS_NIL(val)) {
                    runtimeError("Trying to access an uninitialized variable");
                    return INTERPRET_RUNTIME_ERROR;
                }
                push(val);
            } break;
            // **********************************************************************************
            case OP_SET_LOCAL:
            {
                u8 slot = READ_BYTE();
                frame->slots[slot] = peek(0);
            } break;
            // **********************************************************************************
            case OP_DEFINE_GLOBAL:
            {
                ObjString *globalVarName = READ_STRING();
                bool isNewKey = tableSet(&vm.globals, globalVarName, peek(0));
                _assert(isNewKey == true);
                pop();
            } break;
            // **********************************************************************************
            case OP_GET_GLOBAL:
            {
                ObjString *globalVar =
                    ((ObjString *)(((mainFrame->closure->function->chunk.constants.values[(*frame->ip++)])).as.obj));
                Value value;
                if (!tableGet(&vm.globals, globalVar, &value))
                {
                    runtimeError("Undefined variable '%s'.", globalVar->chars);
                    return INTERPRET_RUNTIME_ERROR;
                }
                push(value);
            } break;
            // **********************************************************************************
            case OP_SET_GLOBAL:
            {
                ObjString *name = READ_STRING();
                // the hashtable set function returns true if the key for which the value was to be set, wasn't
                if (tableSet(&vm.globals, name, peek(0)))
                {
                    tableDelete(&vm.globals, name);
                    runtimeError("Undefined variable '%s'.", name->chars);
                    return INTERPRET_RUNTIME_ERROR;
                }
            } break;
            // **********************************************************************************
            case OP_EQUAL:          { BINARY_OP_FUNC(valuesEqual); }    break;
            case OP_NOT_EQUAL:      { BINARY_OP_FUNC(!valuesEqual); }   break;
            case OP_GREATER:        { BINARY_OP_FUNC(valuesGreater); }  break;
            case OP_GREATER_EQUAL:  { BINARY_OP_FUNC(!valuesLess); }    break;
            case OP_LESS:           { BINARY_OP_FUNC(valuesLess); }     break;
            case OP_LESS_EQUAL:     { BINARY_OP_FUNC(!valuesGreater); } break;
            // **********************************************************************************
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
            // **********************************************************************************
            case OP_SUBTRACT:   { BINARY_OP(NUMBER_VAL, -); } break;
            case OP_MULTIPLY:   { BINARY_OP(NUMBER_VAL, *); } break;
            case OP_DIVIDE:     { BINARY_OP(NUMBER_VAL, /); } break;
            case OP_MOD:
            {
                do
                {
                    if (!((peek(0)).type == VAL_NUMBER) || !((peek(1)).type == VAL_NUMBER))
                    {
                        runtimeError("Operands must be numbers.");
                        return INTERPRET_RUNTIME_ERROR;
                    }
                    u32 b = (u32)((pop()).as.number);
                    Value opResult = ((Value){VAL_NUMBER, {.number = (u32)((peek(0)).as.number) % b}});
                    setTop(opResult);
                } while (0);
            } break;
            // **********************************************************************************
            case OP_NOT:
            {
                Value p = peek(0);
                Value t = BOOL_VAL(isFalsey(p));
                setTop(t);
            } break;
            // **********************************************************************************
            case OP_NEGATE:
            {
                if (!IS_NUMBER(peek(0))) {
                    runtimeError("Operand must be a number!");
                    return INTERPRET_RUNTIME_ERROR;
                }
                double number = AS_NUMBER(peek(0));
                setTop(NUMBER_VAL(-number));
            } break;
            // **********************************************************************************
            case OP_PRINT:
            {
                Value stackTop = pop();
                printValue(stackTop);
                printf("\n");
            } break;
            // **********************************************************************************=
            case OP_JUMP:
            {
                u16 offset = READ_SHORT();
                frame->ip += offset;
            } break;
            // **********************************************************************************
            case OP_JUMP_IF_FALSE:
            {
                u16 jumpOffset = READ_SHORT();
                Value ifConditionResult = peek(0);
                if (isFalsey(ifConditionResult)) {
                    frame->ip += jumpOffset;
                }
            } break;
            // **********************************************************************************
            case OP_LOOP:
            {
                u16 offset = READ_SHORT();
                frame->ip -= offset;
            } break;
            // **********************************************************************************
            case OP_CALL:
            {
                int argCount = READ_BYTE();
                if (!callValue(peek(argCount), argCount)) {
                    return INTERPRET_RUNTIME_ERROR;
                }
                frame = &vm.frames[vm.frameCount - 1];
            } break;
            // **********************************************************************************
            case OP_CLOSURE:
            {
                ObjFunction *function = AS_FUNCTION(READ_CONSTANT());
                ObjClosure *closure = newClosure(function);
                push(OBJ_VAL((Obj *)closure));
                for (int i = 0; i < closure->upvalueCount; ++i) {
                    u8 isLocal = READ_BYTE();
                    u8 index = READ_BYTE();
                    if (isLocal) {
                        closure->upvalues[i] = captureUpvalue(frame->slots + index);
                    } else {
                        closure->upvalues[i] = frame->closure->upvalues[i];
                    }
                }
            } break;
            // **********************************************************************************
            case OP_RETURN:
            {
                Value result = pop();
                vm.frameCount--;
                if (vm.frameCount == 0) {
                    pop();
                    return INTERPRET_OK;
                }

                vm.stack.top = frame->slots;
                int newCount = (int)(vm.stack.top - vm.stack.values);
                vm.stack.count = newCount;
                push(result);
                frame = &vm.frames[vm.frameCount - 1];
            } break;
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
    defineNative("clock", clockNative);
}

InterpretResult
interpret(const char *source)
{
    // passing the source code to the compiler and getting back the top level function code (main function's code).
    ObjFunction *function = compile(source);
    if (function == NULL)
        return INTERPRET_COMPILE_ERROR;
    push(OBJ_VAL((Obj *)function));
    ObjClosure *closure = newClosure(function);
    pop();
    push(OBJ_VAL(closure));
    call(closure, 0);
    mainFrame = &vm.frames[0];
    InterpretResult result = run();
    return result;
}

void
freeVM()
{
    freeTable(&vm.globals);
    freeTable(&vm.strings);
#if USE_DYNAMIC_STACK
    FREE_ARRAY(Value, vm.stack.values, vm.stack.capacity);
    vm.stack.values = NULL;
    vm.stack.capacity = 0;
#endif
    vm.stack.count = 0;
    freeObjects();
}

void
push(Value value)
{
#if USE_DYNAMIC_STACK
    if (vm.stack.capacity < vm.stack.count + 1)
    {
        int oldCapacity = vm.stack.capacity;
        vm.stack.capacity = GROW_CAPACITY(oldCapacity);
        vm.stack.values = GROW_ARRAY(Value, vm.stack.values, oldCapacity, vm.stack.capacity);
        vm.stack.top = vm.stack.values + vm.stack.count;
    }
#else
    if (vm.stack.count > STACK_MAX - 1) {
        _assert(!"Stack Overflow!");
        exit(99);
    }
#endif
    *vm.stack.top = value;
    vm.stack.top++;
    vm.stack.count++;
}

Value
pop()
{
    _assert(vm.stack.count >= 1);
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

static bool
call(ObjClosure *closure, int argCount)
{
    if (argCount != closure->function->arity)
    {
        runtimeError("Expected %d arguments but got %d.", closure->function->arity, argCount);
        return false;
    }
    if (vm.frameCount == FRAMES_MAX) {
        runtimeError("Stack overflow!");
        return false;
    }
    CallFrame *frame = &vm.frames[vm.frameCount++];
    frame->closure = closure;
    frame->ip = closure->function->chunk.code;
    frame->slots = vm.stack.top - argCount - 1;
    return true;
}

static bool
callValue(Value callee, int argCount)
{
    if (IS_OBJ(callee)) {
        switch(OBJ_TYPE(callee)) {
            case OBJ_CLOSURE:
            {
                return call(AS_CLOSURE(callee), argCount);
            } break;

            case OBJ_NATIVE:
            {
                NativeFunc native = AS_NATIVE(callee);
                Value result = native(argCount, vm.stack.top - argCount);
                vm.stack.top -= argCount + 1;
                push(result);
                return true;
            } break;
            
            default: {
                _assert(!"Should not be here!");
            } break;
        }
    }
    runtimeError("Can only call functions and classes");
    return false;
}

static ObjUpvalue *
captureUpvalue(Value *local)
{
    ObjUpvalue *createdUpvalue = newUpvalue(local);
    return createdUpvalue;
}

void
setTop(Value value)
{
    if (vm.stack.count > 0)
    {
        *(vm.stack.top - 1) = value;
    }
}