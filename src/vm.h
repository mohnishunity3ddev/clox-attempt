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
    ObjClosure *closure;

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

    /// @brief head of the list of upvalues sorted based on increasing stack size. Each open upvalue points to the
    ///        next open upvalue that references a local variable farther down the stack. open upvalues are
    ///        referring to variables still on the stack. If they get removed from the stack upvalues need to be
    ///        'closed' meaning the variable they are referencing need to be put up on the heap.
    ObjUpvalue *openUpvalues;

    // NOTE: ------------------------GC Stuff------------------------
    // Gray objects are ones which are found to be reachable by the GC. But the GC hasn't gone through the gray
    // object's own graph of nodes it can reach. When all its references are traversed/visited, we mark it
    // black. when GC has not even visited the root, it's default color is white.

    /// @brief number of gray objects currently in the stack.
    int grayCount;
    /// @brief capacity of the gray varaible stack
    int grayCapacity;
    /// @brief stack holding obj pointers (heap allocated).
    Obj **grayStack;

    /// @brief the number of memory allocated till now.
    size_t bytesAllocated;
    /// @brief the threshold which when crossed invokes the GC's mark-sweep.
    size_t nextGC;
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