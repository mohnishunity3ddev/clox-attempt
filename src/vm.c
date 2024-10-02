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

CallFrame *mainFrame;

static bool
isFalsey(Value value)
{
    return IS_NIL(value) || (IS_BOOL(value) && !AS_BOOL(value));
}

static void
concatenate()
{
    // peeking here since I want to keep it safe from the GC cleaning it up.
    ObjString *b = AS_STRING(peek(0));
    ObjString *a = AS_STRING(peek(1));
    int lenA = a->length;
    int lenB = b->length;
    int totalLength = lenA + lenB;
    char *str = ALLOCATE(char, totalLength+1);
    memcpy(str, a->chars, lenA);
    memcpy(str + lenA, b->chars, lenB);
    str[totalLength] = '\0';
    ObjString *result = takeString(str, totalLength);
    pop();
    pop();
    push(OBJ_VAL((Obj *)result));
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

/// @brief Call the closure. Push a new callframe to the frame stack for this function call.
/// @param closure closure object whose function we have to call.
/// @param argCount the number of arguments passed into the closure
/// @return true if vm was able to call, false, otherwise.
static bool
call(ObjClosure *closure, int argCount)
{
    // number of arguments passed should equal the the function's parameter list while defining it.
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
            // call a bound method of a class instance.
            case OBJ_BOUND_METHOD:
            {
                // OP_CALL on a class field(method) - instance.method(args) '.' calls compiler.dot(), which emits
                // GET_PROPERTY, which calls bindMethod() which stores BoundMethod object on the stack that we get here.
                ObjBoundMethod *bound = AS_BOUND_METHOD(callee);
                // default 0 slot for a function call(first byte of a new CallFrame stack code chunk) stores the
                // instance this method was accessed from. otherwise its empty by default.
                vm.stack.top[-argCount - 1] = bound->instanceOwner;
                return call(bound->method, argCount);
            } break;

            // constructor call.
            case OBJ_CLASS:
            {
                // this is similar to a function call where the 0 slot is the new instance object.
                ObjClass *klass = AS_CLASS(callee);
                vm.stack.top[-argCount - 1] = OBJ_VAL(newInstance(klass));
                // if initString is init. then init's closure gets called. pushes a new callFrame.
                Value initializer;
                // look for an init(default constructor method name) method inside the class's method table.
                if (tableGet(&klass->methods, vm.initString, &initializer)) {
                    return call(AS_CLOSURE(initializer), argCount);
                } else if (argCount != 0) {
                    runtimeError("No explicit initializer defined. Was expecting 0 arguments but got %d.",
                                 argCount);
                    return false;
                }
                return true;
            } break;

            // call a normal function.
            case OBJ_CLOSURE:
            {
                return call(AS_CLOSURE(callee), argCount);
            } break;

            // call a native function like call, sqrtf available in the c runtime.
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

/// @brief helper function for method invocation.
/// @param klass The class the instance belongs to where the method is being called from.
/// @param name Name of the method being called.
/// @param argCount number of arguments.
/// @return true if call successful, false, otherwise.
static bool
invokeFromClass(ObjClass *klass, ObjString *methodName, int argCount)
{
    Value method;
    if (!tableGet(&klass->methods, methodName, &method)) {
        runtimeError("Undefined property '%s'.", methodName->chars);
        return false;
    }

    return call(AS_CLOSURE(method), argCount);
}

/// @brief invoke a method on an instance
/// @param name name of the method
/// @param argCount number of arguments passed in
/// @return true if call successful, false otherwise.
static bool
invoke(ObjString *name, int argCount)
{
    Value receiver = peek(argCount);
    if (!IS_INSTANCE(receiver)) {
        runtimeError("Only instances have methods");
        return false;
    }

    ObjInstance *instance = AS_INSTANCE(receiver);

    Value value;
    // we could be invoking a closure function stored inside a class field. This was how the OP_GET_PROPERTY field
    // was working before.
    if (tableGet(&instance->fields, name, &value)) {
        vm.stack.top[-argCount - 1] = value;
        return callValue(value, argCount);
    }

    bool result = invokeFromClass(instance->klass, name, argCount);

    return result;
}

/// @brief check if method called is defined in the given class(belonging to the instance it was called from), if
///        so, place bound method on top of the stack. binds the method's 'closure' object to the stacktop instance
///        object.
/// @param klass class of the instance where this method was accessed from.
/// @param name Name of the method called.
/// @return true if the method name was found inside class's methods hashtable and was added onto the stack.
static bool
bindMethod(ObjClass *klass, ObjString *name)
{
    Value method;
    if (!tableGet(&klass->methods, name, &method)) {
        // methods are looked up from the instance as a last resort. If the property is not even a method, we
        // return a runtime error.
        runtimeError("Undefined property '%s'.", name->chars);
        return false;
    }

    // create a newBoundMethod object. top of the stack here is the instance object where this method was accessed
    // from. push the bound method to the stack top.
    Value instance = peek(0);
    ObjBoundMethod *bound = newBoundMethod(instance, AS_CLOSURE(method));
    pop();                  // pop the instance.
    push(OBJ_VAL(bound));   // push the bound object.
    return true;
}

/// @brief generate upvalue for the given value on the stack so closures accessing these stack values are able to
///        access them even when the stack values have been removed when the function that define them has returned after
///        execution.
/// @param local pointer to a value that is currently there on the call stack.
/// @return return the pointer to the upvalue.
static ObjUpvalue *
captureUpvalue(Value *local)
{
    ObjUpvalue *prevUpvalue = NULL;
    ObjUpvalue *upvalue = vm.openUpvalues;
    // vm.openUpvalues are sorted on decreasing stack size. the head (vm.openUpvalues) is near the top of the
    // stack.
    while (upvalue != NULL &&
           upvalue->location > local)
    {
        prevUpvalue = upvalue;
        upvalue = upvalue->next;
    }

    // we found an upvalue which is referencing the same local that we are wanting an upvalue for.
    if (upvalue != NULL && upvalue->location == local) {
        return upvalue;
    }

    // create a new open value(referencing variables already on the stack) and add it to the openUpvalues list.
    ObjUpvalue *createdUpvalue = newUpvalue(local);
    createdUpvalue->next = upvalue;
    if (prevUpvalue == NULL) {
        vm.openUpvalues = createdUpvalue;
    } else {
        prevUpvalue->next = createdUpvalue;
    }

    return createdUpvalue;
}

/// @brief hoist the value on the stack slot to the heap(close the upvalue) and additionally close all upvalue that
///        are referencing to stack slots above the provided one.
/// @param last pointer to the stack slot that the upvalue refers to
static void
closeUpvalues(Value *last)
{
    // close all upvalues that are above the value sent it on the stack OR exactly referring to the value sent in
    // here. walk the openvalue list from top to bottom (stack).
    while (vm.openUpvalues != NULL &&
           vm.openUpvalues->location >= last)
    {
        ObjUpvalue *upvalue = vm.openUpvalues;
        upvalue->closed   = *upvalue->location; // copy the stack value into the heap memory where the upvalue is stored.
        upvalue->location = &upvalue->closed;   // update the location of the value it is referring to to point to its 'closed' variable.
        vm.openUpvalues   = upvalue->next;      // move the head of the openValues list to the next one since this one is closed now.
    }
}

static void
defineMethod(ObjString *name)
{
    // objClosure is on top of the stack after compiling method body.
    Value method = peek(0);
    // class object is underneath objClosure(which will bind to this class) on stack before compiling methods of a
    // class declaration in the compiler.
    ObjClass *klass = AS_CLASS(peek(1));
    // add method name to class's method hashtable.
    tableSet(&klass->methods, name, method);
    // pop the method value off the stack top.
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
#define READ_GLOBAL_STRING()                                                                                      \
    (AS_STRING(mainFrame->closure->function->chunk.constants.values[READ_BYTE()]));

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
            // -----------------------------------------------------------------------------------
            case OP_CONSTANT:
            {
                Value constant = READ_CONSTANT();
                push(constant);
            } break;

            // -----------------------------------------------------------------------------------
            case OP_NIL:        { push(NIL_VAL);            } break;
            case OP_TRUE:       { push(BOOL_VAL(true));     } break;
            case OP_FALSE:      { push(BOOL_VAL(false));    } break;
            case OP_POP:        { pop();                    } break;
            case OP_POPN:
            {
                u8 popCount = READ_BYTE();
                popN(popCount);
            } break;

            // -----------------------------------------------------------------------------------
            case OP_GET_UPVALUE:
            {
                u8 slot = READ_BYTE();
                push(*frame->closure->upvalues[slot]->location);
            } break;

            // -----------------------------------------------------------------------------------
            case OP_SET_UPVALUE:
            {
                u8 slot = READ_BYTE();
                *frame->closure->upvalues[slot]->location = peek(0);
            } break;

            // -----------------------------------------------------------------------------------
            case OP_GET_LOCAL:
            {
                u8 localIndex = READ_BYTE();
                Value val = frame->slots[localIndex];
                if (IS_NIL(val)) {
                    runtimeError("Trying to access an uninitialized variable");
                    return INTERPRET_RUNTIME_ERROR;
                }
                push(val);
            } break;

            // -----------------------------------------------------------------------------------
            case OP_SET_LOCAL:
            {
                u8 slot = READ_BYTE();
                frame->slots[slot] = peek(0);
            } break;

            // -----------------------------------------------------------------------------------
            case OP_DEFINE_GLOBAL:
            {
                ObjString *globalVarName = READ_STRING();
                // value meant for this global is already there on the stack. lookup compiler.varDeclaration()
                bool isNewKey = tableSet(&vm.globals, globalVarName, peek(0));
                _assert(isNewKey == true);
                pop();
            } break;

            // -----------------------------------------------------------------------------------
            case OP_GET_GLOBAL:
            {
                ObjString *globalVar = READ_GLOBAL_STRING();
                Value value;
                if (!tableGet(&vm.globals, globalVar, &value))
                {
                    runtimeError("Undefined variable '%s'.", globalVar->chars);
                    return INTERPRET_RUNTIME_ERROR;
                }
                push(value);
            } break;

            // -----------------------------------------------------------------------------------
            case OP_SET_GLOBAL:
            {
                ObjString *globalVar = READ_GLOBAL_STRING();
                // the hashtable set function returns true if key-value pair wasn't in it, which is error since
                // set_global is called for variables that were seen before and hence should be inside the table.
                if (tableSet(&vm.globals, globalVar, peek(0)))
                {
                    tableDelete(&vm.globals, globalVar);
                    runtimeError("Undefined variable '%s'.", globalVar->chars);
                    return INTERPRET_RUNTIME_ERROR;
                }
            } break;

            // -----------------------------------------------------------------------------------
            case OP_GET_PROPERTY:
            {
                if (!IS_INSTANCE(peek(0))) {
                    runtimeError("Only instances can have fields");
                    return INTERPRET_RUNTIME_ERROR;
                }
                // get an instance property. when parsing b.c = 3, b is a variable(), compiler emits
                // GET_GLOBAL/LOCAL on it while parsing which the vm gets and pushes the instance on to the stack
                // prior to getting here.
                ObjInstance *instance = AS_INSTANCE(peek(0));
                // name of the property i.e. c in b.c
                ObjString *name = READ_STRING();
                // get the value for the property in the fields hashtable keyed by it's name.
                Value value;
                if (tableGet(&instance->fields, name, &value)) {
                    pop(); // pop instance
                    push(value);
                } else {
                    // is this a method property?
                    if (!bindMethod(instance->klass, name)) {
                        runtimeError("instance of type '%s' does not have property '%s'", instance->klass->name->chars,
                                    name->chars);
                        return INTERPRET_RUNTIME_ERROR;
                    }
                }
            } break;

            // -----------------------------------------------------------------------------------
            case OP_SET_PROPERTY:
            {
                if (!IS_INSTANCE(peek(1))) {
                    runtimeError("Can only set values to fields");
                    return INTERPRET_RUNTIME_ERROR;
                }
                ObjInstance *instance = AS_INSTANCE(peek(1));      // the instance whose field is being set.
                ObjString *fieldName = READ_STRING();              // the field/property of the instance we want to set the value to.
                tableSet(&instance->fields, fieldName, peek(0));   // value meant for the object field.
                Value value = pop();                               // pop off the value meant for the field.
                pop();                                             // pop the instance object.
                push(value);                                       // push the value.
            } break;

            // -----------------------------------------------------------------------------------
            case OP_EQUAL:          { BINARY_OP_FUNC(valuesEqual);    } break;
            case OP_NOT_EQUAL:      { BINARY_OP_FUNC(!valuesEqual);   } break;
            case OP_GREATER:        { BINARY_OP_FUNC(valuesGreater);  } break;
            case OP_GREATER_EQUAL:  { BINARY_OP_FUNC(!valuesLess);    } break;
            case OP_LESS:           { BINARY_OP_FUNC(valuesLess);     } break;
            case OP_LESS_EQUAL:     { BINARY_OP_FUNC(!valuesGreater); } break;

            // -----------------------------------------------------------------------------------
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
                        runtimeError("Unsupported operands for +");
                        exit(1);
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
                        runtimeError("Unsupported operands for +");
                        exit(1);
                    }
                } else {
                    runtimeError("Operands must be two numbers or two strings.");
                    return INTERPRET_RUNTIME_ERROR;
                }
            } break;

            // -----------------------------------------------------------------------------------
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

            // -----------------------------------------------------------------------------------
            case OP_NOT:
            {
                Value p = peek(0);
                Value t = BOOL_VAL(isFalsey(p));
                setTop(t);
            } break;

            // -----------------------------------------------------------------------------------
            case OP_NEGATE:
            {
                if (!IS_NUMBER(peek(0))) {
                    runtimeError("Operand must be a number!");
                    return INTERPRET_RUNTIME_ERROR;
                }
                double number = AS_NUMBER(peek(0));
                setTop(NUMBER_VAL(-number));
            } break;

            // -----------------------------------------------------------------------------------
            case OP_PRINT:
            {
                Value stackTop = pop();
                printValue(stackTop);
                printf("\n");
            } break;

            // -----------------------------------------------------------------------------------=
            case OP_JUMP:
            {
                u16 offset = READ_SHORT();
                frame->ip += offset;
            } break;

            // -----------------------------------------------------------------------------------
            case OP_JUMP_IF_FALSE:
            {
                u16 jumpOffset = READ_SHORT();
                Value ifConditionResult = peek(0);
                if (isFalsey(ifConditionResult)) {
                    frame->ip += jumpOffset;
                }
            } break;

            // -----------------------------------------------------------------------------------
            case OP_LOOP:
            {
                u16 offset = READ_SHORT();
                frame->ip -= offset;
            } break;

            // -----------------------------------------------------------------------------------
            case OP_CALL:
            {
                int argCount = READ_BYTE();
                if (!callValue(peek(argCount), argCount)) {
                    return INTERPRET_RUNTIME_ERROR;
                }
                frame = &vm.frames[vm.frameCount - 1];
            } break;

            // -----------------------------------------------------------------------------------
            case OP_CLASS:
            {
                ObjString *klassName = READ_STRING();
                Value klassValue = OBJ_VAL(newClass(klassName));
                push(klassValue);
            } break;

            // -----------------------------------------------------------------------------------
            case OP_GET_SUPER:
            {
                ObjString *superclassMethodName = READ_STRING();
                ObjClass *superclass = AS_CLASS(pop());
                if (!bindMethod(superclass, superclassMethodName)) {
                    return INTERPRET_RUNTIME_ERROR;
                }
            } break;

            // -----------------------------------------------------------------------------------
            case OP_INHERIT:
            {
                Value superclass = peek(1);
                // superclass has to be a class.
                if (!IS_CLASS(superclass)) {
                    runtimeError("Superclass must be a class");
                    return INTERPRET_RUNTIME_ERROR;
                }
                ObjClass *subclass = AS_CLASS(peek(0));
                // copy all superclass's methods into the subclass's methods table. At this point, we haven't
                // looked at the subclass's methods in the compiler. so methods with the same name don't get
                // overwritten by this addition.
                tableAddAll(&AS_CLASS(superclass)->methods, &subclass->methods);
                pop(); // pop subclass.
            } break;

            // -----------------------------------------------------------------------------------
            case OP_METHOD:
            {
                // read the method name from the constant table.
                defineMethod(READ_STRING());
            } break;

            // -----------------------------------------------------------------------------------
            case OP_INVOKE:
            {
                ObjString *method = READ_STRING();
                int argCount = READ_BYTE();
                if (!invoke(method, argCount)) {
                    return INTERPRET_RUNTIME_ERROR;
                }
                // push the new method callFrame on the call Stack.
                frame = &vm.frames[vm.frameCount - 1];
            } break;

            // -----------------------------------------------------------------------------------
            case OP_SUPER_INVOKE:
            {
                ObjString *superclassMethodName = READ_STRING();
                int argCount = READ_BYTE();
                ObjClass *superclass = AS_CLASS(pop());
                if (!invokeFromClass(superclass, superclassMethodName, argCount)) {
                    return INTERPRET_RUNTIME_ERROR;
                }
                frame = &vm.frames[vm.frameCount - 1];
            } break;

            // -----------------------------------------------------------------------------------
            // create the closure object for the function and leave it on top of VM stack.
            case OP_CLOSURE:
            {
                ObjFunction *function = AS_FUNCTION(READ_CONSTANT());
                ObjClosure *closure = newClosure(function);
                push(OBJ_VAL((Obj *)closure));
                // are there any local variables in its parent hierarchy that the current function(closure) is accessing?
                for (int i = 0; i < closure->upvalueCount; ++i) {
                    u8 isLocal = READ_BYTE(); // is it a local variable in the immediate enclosing function?
                    u8 index = READ_BYTE(); // what is the local index of the function's local variables list.
                    if (isLocal) {
                        // slots is the first stack slot where this function starts.
                        closure->upvalues[i] = captureUpvalue(frame->slots + index);
                    } else {
                        // this closure deeply nested and it needs to access an upvalue from one of its ancestor functions.
                        closure->upvalues[i] = frame->closure->upvalues[i];
                    }
                }
            } break;

            // -----------------------------------------------------------------------------------
            case OP_CLOSE_UPVALUE:
            {
                // hoist the variable on stack top to the heap since it will be accessed by a closure.
                closeUpvalues(vm.stack.top - 1);
                // we still want the local variable to be popped since the function is exiting.
                pop();
            } break;

            // -----------------------------------------------------------------------------------
            case OP_RETURN:
            {
                Value result = pop();
                vm.frameCount--;
                // close every remaining openvalue still owned by the returning function.
                closeUpvalues(frame->slots);
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

#undef READ_GLOBAL_STRING
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
    vm.openUpvalues = NULL;

    // At the beginning there are no gray objects since the GC has not even started visiting nodes yet.
    vm.grayCapacity = 0;
    vm.grayCount = 0;
    vm.grayStack = NULL;
    vm.bytesAllocated = 0;
    vm.nextGC = 1024 * 1024;

    initTable(&vm.strings);

    // defining what the constructor method names should be.
    vm.initString = NULL; // make sure if the GC runs after the copyString call. it doesn't try to read initString
                          // ptr before it's initialized.
    vm.initString = copyString("init", 4);

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
    // make sure initString memory is unreachable by the GC and frees the memory held here.
    vm.initString = NULL;
#if USE_DYNAMIC_STACK
    FREE_ARRAY(Value, vm.stack.values, vm.stack.capacity);
    vm.stack.values = NULL;
    vm.stack.capacity = 0;
#endif
    vm.stack.count = 0;
    freeObjects();
    free(vm.grayStack);
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

void
setTop(Value value)
{
    if (vm.stack.count > 0) {
        *(vm.stack.top - 1) = value;
    }
}