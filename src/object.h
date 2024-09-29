#ifndef clox_object_h
#define clox_object_h

#include "common.h"
#include "chunk.h"
#include "value.h"

#define OBJ_TYPE(value)   (AS_OBJ(value)->type)

#define IS_STRING(value)  isObjType(value, OBJ_STRING)
#define IS_FUNCTION(value) isObjType(value, OBJ_FUNCTION)
#define IS_CLOSURE(value) isObjType(value, OBJ_CLOSURE)
#define IS_NATIVE(value)  isObjType(value, OBJ_NATIVE)

#define AS_STRING(value)  ((ObjString *)AS_OBJ(value))
#define AS_CSTRING(value) (((ObjString *)AS_OBJ(value))->chars)
#define AS_FUNCTION(value) ((ObjFunction *)AS_OBJ(value))
#define AS_CLOSURE(value) ((ObjClosure *)AS_OBJ(value))
#define AS_NATIVE(value) (((ObjNative *) AS_OBJ(value))->function)

typedef enum {
    OBJ_UPVALUE,
    OBJ_CLOSURE,
    OBJ_FUNCTION,
    OBJ_NATIVE,
    OBJ_STRING,
} ObjType;

/// @brief Base class definition for all structs. Using this structure for inhertitance.
struct Obj{
    ObjType type;
    /// @brief is this object marked to be reachable by the GC?
    bool isMarked;
    /// @brief intrusive linked list next pointers.
    struct Obj *next;
};

struct ObjString {
    Obj obj;
    int length;
    char *chars;
    u32 hash;
};

/// @brief Describes the runtime representation of a function object.
///        This struct encapsulates all information needed to execute a function, including its code, parameters,
///        and metadata.
typedef struct
{
    /// @brief The base class object header for a function object.
    ///        This is necessary for the function to be treated as an object in the language's runtime (e.g., for
    ///        garbage collection).
    Obj obj;

    /// @brief The number of parameters the function expects to receive.
    ///        This is used to verify argument count when the function is called and to set up the function's stack
    ///        frame.
    int arity;

    /// @brief The number of upvalues that the function uses. Gets set by the compiler at backend.
    int upvalueCount;

    /// @brief The chunk of bytecode that this function represents.
    ///        The chunk contains the compiled instructions and constants for the function, forming the executable
    ///        part of the function.
    Chunk chunk;

    /// @brief The name of the function (if available).
    ///        This is used for debugging, error reporting, or profiling purposes. Anonymous functions may have a
    ///        `NULL` name.
    ObjString *name;
} ObjFunction;
/// @brief Describes the runtime representation of an upvalue object.
///        Upvalues are used to capture variables from the enclosing scope that remain in use even after the parent
///        function has returned. They allow closures to keep access to variables from their defining context.
typedef struct ObjUpvalue ObjUpvalue;
struct ObjUpvalue
{
    /// @brief The base class object header for the upvalue.
    ///        As upvalues are objects, this header enables them to integrate with the language’s object model.
    Obj obj;

    /// @brief the place where the upvalue stores the value on the stack that it is pointing to after the value has
    ///        been popped from the stack since the scope they were defined in has ended.
    Value closed;

    /// @brief A pointer to the closed-over variable's memory location further down the stack that can be
    ///        referenced way past its lifetime(when its function where it is declared returns after executing).
    ///        Instead of storing a copy of the variable, the upvalue holds a reference to the original variable,
    ///        ensuring that updates to the variable are visible to all closures that share the upvalue.
    Value *location;

    /// @brief intrusive linked list of upvalues sorted on basis of increasing stack size. upvalues refer to values
    ///        on the stack which need to be accessed way after its lifetime's meant to be.
    ObjUpvalue *next;
};

/// @brief Represents a closure object in the runtime.
///        A closure is a function combined with the environment in which it was declared.
///        It "closes over" variables from its outer scope (upvalues) and keeps them alive beyond the life of the
///        scope.
typedef struct
{
    /// @brief The base class object header for a closure.
    ///        This ensures that closures are recognized as objects by the runtime and can be managed (e.g., for
    ///        memory management).
    Obj obj;

    /// @brief The function that this closure wraps.
    ///        The function contains the compiled bytecode and other metadata, while the closure keeps the
    ///        necessary context (upvalues) to execute it.
    ObjFunction *function;

    /// @brief An array of pointers to upvalues that the closure has captured.
    ///        Each upvalue corresponds to a variable from an outer function that remains in use by the closure.
    ///        The number of upvalues varies depending on the function, so the array is dynamically sized.
    ObjUpvalue **upvalues;

    /// @brief The number of upvalues captured by this closure. used at runtime in the virtual machine.
    int upvalueCount;
} ObjClosure;

// NOTE: Native functions that are resolved at runtime and directly call some native function that we get and
// execute directly in C in VM. For example - sqrtf, atan2, clock etc.
typedef Value (*NativeFunc)(int argCount, Value *args);
typedef struct {
    Obj obj;
    NativeFunc function;
} ObjNative;

static inline bool
isObjType(Value value, ObjType type)
{
    bool result = IS_OBJ(value) && AS_OBJ(value)->type == type;
    return result;
}

/// @brief creates a new upvalue object.
/// @param slot address of the slot where the closed-over variable lives. slot is the location on the stack where
///             the local variable to an enclosing function lives.
/// @return
ObjUpvalue *newUpvalue(Value *slot);

ObjClosure *newClosure(ObjFunction *function);

/// @brief helper function to create a new function
/// @return a ObjFunction * pointer describing the function.
ObjFunction *newFunction();

/// @brief creates a new native function object.
/// @param function pointer to the actual native function
/// @return
ObjNative *newNative(NativeFunc function);

/// @brief Take ownership of the string passed in.
/// @param chars character array of the string
/// @param length length of the string
/// @return pointer to the string object which contains the string.
ObjString *takeString(char *chars, int length);

/// @brief allocate memory to copy/duplicate the string passed in
/// @param chars character array of the string
/// @param length length of the string
/// @return pointer to the string object which contains the string.
ObjString *copyString(const char *chars, int length);
ObjString *copyStringFormat(const char *format, ...);

#define USE_SINGLE_ALLOCATION 0
#if USE_SINGLE_ALLOCATION
// NOTE: Functions to encapsulate memory for StringObject and the string inside a single allocation.
ObjString *makeString(const char *chars, int length, bool ownsString);
ObjString *makeStringConcat(ObjString *a, ObjString *b);
#endif

void printObject(Value value);

#endif