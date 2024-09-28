#ifndef clox_value_h
#define clox_value_h

#include "common.h"

typedef struct Obj Obj;
typedef struct ObjString ObjString;

typedef enum
{
    VAL_BOOL,
    VAL_NIL,
    VAL_NUMBER,
    /// @brief Obj Value types are ones whose values live on the heap. For example, a large string etc.
    VAL_OBJ,
} ValueType;

typedef struct
{
    ValueType type;
    union {
        bool boolean;
        double number;
        // NOTE: IF the value type is VAL_OBJ then the value lives on the heap. The pointer to that memory is
        // stored here.
        Obj *obj;
    } as;
} Value;

// Type checking. Need to use these to guard calls to AS_ macros below.
#define IS_BOOL(value)   ((value).type == VAL_BOOL)
#define IS_NIL(value)    ((value).type == VAL_NIL)
#define IS_NUMBER(value) ((value).type == VAL_NUMBER)
#define IS_OBJ(value)    ((value).type == VAL_OBJ)

// Getting C type values from the typedef Value we are using for this lang.
#define AS_BOOL(value)   ((value).as.boolean)
#define AS_NUMBER(value) ((value).as.number)
#define AS_OBJ(value)    ((value).as.obj)

// converts a C value to the struct Value that we are using for this lang.
#define BOOL_VAL(value)   ((Value){ VAL_BOOL,   {.boolean = value} })
#define NIL_VAL           ((Value){ VAL_NIL,    {.number  = 0}     })
#define NUMBER_VAL(value) ((Value){ VAL_NUMBER, {.number  = value} })
#define OBJ_VAL(value)    ((Value){ VAL_OBJ,    {.obj     = (Obj *)value} })

typedef struct
{
    int capacity;
    int count;
    Value *values;
} ValueArray;

void initValueArray(ValueArray *array);
void writeValueArray(ValueArray *array, Value value);
void freeValueArray(ValueArray *array);
void printValue(Value value);

/// @brief Get the index of a string constant in the passed in constants array if it is present here.
/// @param array pointer to the constants array
/// @param string the string that we want to check if it is present.
/// @param len length of the string you passed in here.
/// @return -1 if the string is not present, Otherwise - returns the index into the array where it is present.
int stringValueIndex(ValueArray *array, const char *string, int len);

bool valuesEqual(Value a, Value b);
bool valuesGreater(Value a, Value b);
bool valuesLess(Value a, Value b);

#endif // clox_value_h