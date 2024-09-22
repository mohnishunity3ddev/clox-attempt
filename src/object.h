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
};

static inline bool
isObjType(Value value, ObjType type)
{
    bool result = IS_OBJ(value) && AS_OBJ(value)->type == type;
    return result;
}

ObjString *takeString(char *chars, int length);
ObjString *copyString(const char *chars, int length);
void printObject(Value value);

#endif