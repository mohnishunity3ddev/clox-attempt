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
    OBJ_CLOSURE,
    OBJ_FUNCTION,
    OBJ_NATIVE,
    OBJ_STRING,
} ObjType;

/// @brief Base class definition for all structs. Using this structure for inhertitance.
struct Obj{
    ObjType type;
    struct Obj *next;
};

struct ObjString {
    Obj obj;
    int length;
    char *chars;
    u32 hash;
};

/// @brief describes the runtime representation of a function
typedef struct {
    /// @brief base class obj header.
    Obj obj;
    /// @brief Number of parameters the function expects.
    int arity;
    /// @brief number of upvalues (variables inside a nested function which refer to local variables in its
    ///        enclosing/parent function).
    int upvalueCount;
    /// @brief chunk of code this function has. A function gets to have its own chunk.
    Chunk chunk;
    /// @brief describes the name of the function
    ObjString *name;
} ObjFunction;

/// @brief runtime wrappers for functions.
typedef struct {
    Obj obj;
    ObjFunction *function;
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