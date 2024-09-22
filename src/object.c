#include <stdio.h>
#include <string.h>
#include <stdarg.h>

#include "memory.h"
#include "object.h"
#include "value.h"
#include "vm.h"

#define ALLOCATE_OBJ(type, objectType) \
    (type *)allocateObject(sizeof(type), objectType)

static Obj *
allocateObject(size_t size, ObjType type)
{
    Obj *object = (Obj *)reallocate(NULL, 0, size);
    object->type = type;
    object->next = vm.objects;
    vm.objects = object;
    return object;
}

static ObjString *
allocateString(char *chars, int length)
{
    ObjString *string = ALLOCATE_OBJ(ObjString, OBJ_STRING);
    string->length = length;
    string->chars = chars;
    return string;
}

static ObjString *
stringObjConcat(const char *a, int lenA, const char *b, int lenB)
{
    int length = lenA + lenB;
    size_t structSize = sizeof(ObjString);
    size_t charSize = (length + 1);

    ObjString *objString = (ObjString *)reallocate(NULL, 0, structSize + charSize);
    objString->obj.type = OBJ_STRING;
    objString->obj.next = vm.objects;
    vm.objects = (Obj *)objString;

    objString->length = length;
    objString->chars = (char *)objString + structSize;
    memcpy(objString->chars, a, lenA);
    memcpy(objString->chars + lenA, b, lenB);
    objString->chars[length] = '\0';

    return objString;
}

ObjString *
makeString(const char *chars, int length, bool ownsString)
{
    size_t structSize = sizeof(ObjString);
    size_t charSize = (length + 1);
    size_t sz = ownsString ? (structSize + charSize) : structSize;

    ObjString *objString = (ObjString *)reallocate(NULL, 0, sz);
    objString->obj.type = OBJ_STRING;
    objString->obj.next = vm.objects;
    vm.objects = (Obj *)objString;

    objString->length = length;

    if (ownsString) {
        objString->chars = (char *)objString + structSize;
        memcpy(objString->chars, chars, length);
        objString->chars[length] = '\0';
    } else {
        objString->chars = (char *)chars;
    }

    objString->ownsString = ownsString;

    return objString;
}

ObjString *
makeStringConcat(ObjString *a, ObjString *b)
{
    ObjString *result = stringObjConcat(a->chars, a->length, b->chars, b->length);
    result->ownsString = true;

    return result;
}

ObjString *
makeStringFormat(const char *format, ...)
{
    va_list args;
    va_start(args, format);
    char buffer[8192];
    int charsWritten = vsnprintf(buffer, 8192, format, args);
    va_end(args);

    ObjString *result = makeString(buffer, charsWritten, true);
    return result;
}

ObjString *
takeString(char *chars, int length)
{
    return allocateString(chars, length);
}

ObjString *
copyString(const char *chars, int length)
{
    char *heapChars = ALLOCATE(char, length + 1);
    memcpy(heapChars, chars, length);
    heapChars[length] = '\0';
    return allocateString(heapChars, length);
}

void
printObject(Value value)
{
    switch(OBJ_TYPE(value))
    {
        case OBJ_STRING: { printf("%s", AS_CSTRING(value)); } break;

        default: { _assert(!"Invalid Path"); } break;
    }
}
