#include <stdio.h>
#include <string.h>
#include <stdarg.h>

#include "memory.h"
#include "object.h"
#include "value.h"
#include "vm.h"
#include "table.h"

#define ALLOCATE_OBJ(type, objectType) \
    (type *)allocateObject(sizeof(type), objectType)

static u32
hashString(const char *key, int length)
{
    u32 hash = 2166136261u;
    for (int i = 0; i < length; i++)
    {
        hash ^= (u8)key[i];
        hash *= 16777619;
    }
    return hash;
}

static Obj *
allocateObject(size_t size, ObjType type)
{
    Obj *object = (Obj *)reallocate(NULL, 0, size);
    object->type = type;
    object->isMarked = false;

    object->next = vm.objects;
    vm.objects = object;

#ifdef DEBUG_LOG_GC
    printf("%p allocate %zu for %d\n", (void *)object, size, type);
#endif

    return object;
}

static ObjString *
allocateStringObject(char *chars, int length, u32 hash)
{
    ObjString *string = ALLOCATE_OBJ(ObjString, OBJ_STRING);
    string->length = length;
    string->chars = chars;
    string->hash = hash;

    // NOTE: the vm.strings is a hashset or table of unique strings. That's all we care about. so we fill them with
    // null values.
    // NOTE: pushing the string object to the stack so that it survives the GC sweep if invoked by adding an entry
    // to the hashtable (in case it needs to be resized).
    push(OBJ_VAL(string));
    tableSet(&vm.strings, string, NIL_VAL);
    pop();
    return string;
}

ObjUpvalue *
newUpvalue(Value *slot)
{
    ObjUpvalue *upvalue = ALLOCATE_OBJ(ObjUpvalue, OBJ_UPVALUE);
    upvalue->location = slot;
    upvalue->next = NULL;
    return upvalue;
}

ObjClosure *
newClosure(ObjFunction *function)
{
    ObjUpvalue **upvalues = ALLOCATE(ObjUpvalue*, function->upvalueCount);
    for (int i = 0; i < function->upvalueCount; ++i)
    {
        upvalues[i] = NULL;
    }

    ObjClosure *closure     = ALLOCATE_OBJ(ObjClosure, OBJ_CLOSURE);
    closure->function       = function;
    closure->upvalues       = upvalues;
    closure->upvalueCount   = function->upvalueCount;
    return closure;
}

ObjFunction *
newFunction()
{
    // Create a zero-initialized 'function'.
    ObjFunction *function = ALLOCATE_OBJ(ObjFunction, OBJ_FUNCTION);
    function->arity = 0;
    function->upvalueCount = 0;
    function->name = NULL;
    initChunk(&function->chunk);
    return function;
}

ObjNative *
newNative(NativeFunc function)
{
    ObjNative *native = ALLOCATE_OBJ(ObjNative, OBJ_NATIVE);
    native->function = function;
    return native;
}

ObjString *
takeString(char *chars, int length)
{
    u32 hash = hashString(chars, length);
    ObjString *interned = tableFindString(&vm.strings, chars, length, hash);
    if (interned != NULL) {
        // This is the taking ownership part. The string is present in the hashtable so we free the memory for the
        // string passed in here.
        FREE_ARRAY(char, chars, length + 1);
        return interned;
    }

    // Allocate memory for the string object and store it in the hashtable.
    return allocateStringObject(chars, length, hash);
}

ObjString *
copyString(const char *chars, int length)
{
    u32 hash = hashString(chars, length);

    // NOTE: See if the string is already there in the strings hashtable in the vm.
    // if it is, we return and forget about adding a duplicate in the table.
    ObjString *interned = tableFindString(&vm.strings, chars, length, hash);
    if (interned != NULL) {
        return interned;
    }

    // Copy the new string and store it in the hash-table.
    char *heapChars = ALLOCATE(char, length + 1);
    memcpy(heapChars, chars, length);
    heapChars[length] = '\0';

    // Allocate memory for the string object and store it in the hashtable.
    return allocateStringObject(heapChars, length, hash);
}

ObjString *
copyStringFormat(const char *format, ...)
{
    va_list args;
    va_start(args, format);
    char buffer[8192];
    int charsWritten = vsnprintf(buffer, 8192, format, args);
    va_end(args);
    _assert(charsWritten <= 8192);

    ObjString *result = copyString(buffer, charsWritten);
    return result;
}

static void
printFunction(ObjFunction *function)
{
    if (function->name == NULL) {
        printf("<main>");
        return;
    }
    printf("<fn %s>", function->name->chars);
}

void
printObject(Value value)
{
    switch(OBJ_TYPE(value))
    {
        case OBJ_UPVALUE:   { printf("upvalue"); }                          break;
        case OBJ_CLOSURE:   { printFunction(AS_CLOSURE(value)->function); } break;
        case OBJ_FUNCTION:  { printFunction(AS_FUNCTION(value)); }          break;
        case OBJ_NATIVE:    { printf("<native function>"); }                break;

        case OBJ_STRING:
        {
            ObjString *str = AS_STRING(value);
            printf("%.*s", str->length, str->chars);
        } break;

        default: { _assert(!"Invalid Path"); } break;
    }
}

#if USE_SINGLE_ALLOCATION
// NOTE: if you want to store the object string and its character array in one single continuous memory block.
static ObjString *
stringObjectConcat(const char *a, int lenA, const char *b, int lenB)
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
makeStringConcat(ObjString *a, ObjString *b)
{
    ObjString *result = stringObjectConcat(a->chars, a->length, b->chars, b->length);

    return result;
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


    return objString;
}
#endif