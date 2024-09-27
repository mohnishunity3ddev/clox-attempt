#ifndef clox_vm_h
#define clox_vm_h

#include "object.h"
#include "chunk.h"
#include "value.h"
#include "table.h"

#define FRAMES_MAX 8192*32
// FIXME: Can't use a dynamic stack right now! :( Check this out.
#define USE_DYNAMIC_STACK 0
#if !USE_DYANMIC_STACK
    #define STACK_MAX (FRAMES_MAX * UINT8_COUNT)
#endif


/// @brief Keeps track of where the caller should begin after calling a function. And where on the vm's stack does
///        the variables local to the function begin.
///        This is during a function invocation, it has not returned yet to the caller.
///        Each time a function is called, this struct is created.
typedef struct {
    /// @brief caller function. which has its bytecode (containing constants and the bytecode itself). Also has the
    /// name of the function currently being executed.
    ObjFunction *function;

    /// @brief Instead of storing the return address in the callee's frame, caller stores its own ip. When we
    ///        return from a function, The VM will jump to the ip of the caller's CallFrame.
    u8 *ip;

    /// @brief Points to the location in the VM's stack where the function actually begins.
    ///        The 0th slot when a function begins is the function object itself. slot 0 is reserved for the
    ///        function object always. the arguments(if any) and local variables used inside the function are
    ///        stored from slot index 1.
    Value *slots;
} CallFrame;

typedef struct
{
    int count;
    Value *top;
#if USE_DYNAMIC_STACK
    int capacity;
    Value *values;
#else
    Value values[STACK_MAX];
#endif
} Stack;

typedef struct
{
    /// @brief each Callframe represents its own ip and the pointer to the ObjFunction that it's executing.
    CallFrame frames[FRAMES_MAX];

    /// @brief Current height of the CallFrame Stack (number of ongoing function calls which haven't finished
    ///        executing yet.)
    int frameCount;

    Stack stack;
    /// @brief a hashtable of unique strings (interned strings).
    Table strings;
    /// @brief a hashtable with key = names of the global variables and the value they represent.
    Table globals;
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
void popN(int n);
Value peek(int indexFromLast);
void setTop(Value value);

#endif