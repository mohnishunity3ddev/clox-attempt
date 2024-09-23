#ifndef clox_object_h
#define clox_object_h

#include "common.h"
#include "value.h"

#define OBJ_TYPE(value)   (AS_OBJ(value)->type)
#define IS_STRING(value)  isObjType(value, OBJ_STRING)

#define AS_STRING(value)  ((ObjString *)AS_OBJ(value))
#define AS_CSTRING(value) (((ObjString *)AS_OBJ(value))->chars)

typedef enum
{
    OBJ_STRING
} ObjType;

/// @brief Base class definition for all structs. Using this structure for inhertitance.
struct Obj
{
    ObjType type;
    struct Obj *next;
};

struct ObjString
{
    Obj obj;
    int length;
    char *chars;
    u32 hash;
};

static inline bool
isObjType(Value value, ObjType type)
{
    bool result = IS_OBJ(value) && AS_OBJ(value)->type == type;
    return result;
}

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