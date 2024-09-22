#include <stdio.h>
#include <string.h>

#include "memory.h"
#include "value.h"
#include "object.h"

void
initValueArray(ValueArray *array)
{
    array->values = NULL;
    array->capacity = 0;
    array->count = 0;
}

void
writeValueArray(ValueArray *array, Value value)
{
    if (array->capacity < array->count + 1)
    {
        int oldCapacity = array->capacity;
        array->capacity = GROW_CAPACITY(oldCapacity);
        array->values = GROW_ARRAY(Value, array->values, oldCapacity, array->capacity);
    }

    array->values[array->count] = value;
    array->count++;
}

void
freeValueArray(ValueArray *array)
{
    FREE_ARRAY(Value, array->values, array->capacity);
    initValueArray(array);
}

bool
valuesEqual(Value a, Value b)
{
    if (a.type != b.type)
        return false;

    bool result = false;
    switch(a.type)
    {
        case VAL_BOOL:   { result = AS_BOOL(a) == AS_BOOL(b); }     break;
        case VAL_NIL:    { result = true; }                         break;
        case VAL_NUMBER: { result = AS_NUMBER(a) == AS_NUMBER(b); } break;
        case VAL_OBJ:
        {
            ObjString *aString = AS_STRING(a);
            ObjString *bString = AS_STRING(b);
            result = (aString->length == bString->length) &&
                     (memcmp(aString->chars, bString->chars, aString->length) == 0);
        } break;
        default:         { result = false; }                        break;
    }

    return result;
}

bool
valuesGreater(Value a, Value b)
{
    bool result = false;
    switch(a.type)
    {
        case VAL_NIL:    { result = false; }                                        break;

        case VAL_BOOL:
        case VAL_NUMBER: { result = AS_NUMBER(a) > AS_NUMBER(b); }                  break;
        default:         { result = false; }                                        break;
    }

    return result;
}

bool
valuesLess(Value a, Value b)
{
    bool result = false;
    switch(a.type)
    {
        case VAL_NIL:    { result = false; }                                        break;

        case VAL_BOOL:
        case VAL_NUMBER: { result = AS_NUMBER(a) < AS_NUMBER(b); }                  break;
        default:         { result = false; }                                        break;
    }

    return result;
}

void printValue(Value value)
{
    switch(value.type)
    {
        case VAL_BOOL:   { printf(AS_BOOL(value) ? "true" : "false"); } break;
        case VAL_NIL:    { printf("nil"); } break;
        case VAL_NUMBER: { printf("%g", AS_NUMBER(value)); } break;
        case VAL_OBJ:    { printObject(value); } break;
        default: { printf("Unknown value type\n"); } break;
    }
}
