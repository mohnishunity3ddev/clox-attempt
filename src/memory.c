#include <stdlib.h>
#include "memory.h"
#include "vm.h"

void *
reallocate(void *pointer, size_t oldSize, size_t newSize)
{
    if (newSize == 0) {
        free(pointer);
        return NULL;
    }

    void *result = realloc(pointer, newSize);
    if (result == NULL)
        exit(1);
    return result;
}

static void
freeObject(Obj *object)
{
    switch(object->type)
    {
        case OBJ_STRING:
        {
            ObjString *string = (ObjString *)object;
            // NOTE: Right now, the struct and the character array are one continuous allocation.
            // FREE_ARRAY(char, string->chars, string->length + 1);
            FREE(ObjString, object);
        } break;
        default: { _assert(!"Invalid Path"); } break;
    }
}

void
freeObjects()
{
    Obj *object = vm.objects;
    while (object != NULL)
    {
        Obj *next = object->next;
        freeObject(object);
        object = next;
    }
}
