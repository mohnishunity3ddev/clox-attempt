#ifndef clox_value_h
#define clox_value_h

#include "common.h"

typedef enum
{
    VAL_BOOL,
    VAL_NIL,
    VAL_NUMBER,
} ValueType;

typedef struct
{
    ValueType type;
    union {
        bool boolean;
        double number;
    } as;
} Value;

// Type checking. Need to use these to guard calls to AS_ macros below.
#define IS_BOOL(value)   ((value).type == VAL_BOOL)
#define IS_NIL(value)    ((value).type == VAL_NIL)
#define IS_NUMBER(value) ((value).type == VAL_NUMBER)

// Getting C type values from the typedef Value we are using for this lang.
#define AS_BOOL(value)   ((value).as.boolean)
#define AS_NUMBER(value) ((value).as.number)

// converts a C value to the struct Value that we are using for this lang.
#define BOOL_VAL(value)   ((Value){ VAL_BOOL,   {.boolean = value} })
#define NIL_VAL           ((Value){ VAL_NIL,    {.number = 0} })
#define NUMBER_VAL(value) ((Value){ VAL_NUMBER, {.number = value} })

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
bool valuesEqual(Value a, Value b);
bool valuesGreater(Value a, Value b);
bool valuesLess(Value a, Value b);

#endif // clox_value_h