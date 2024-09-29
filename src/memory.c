#include <stdlib.h>
#include "memory.h"
#include "vm.h"
#include "compiler.h"

#ifdef DEBUG_LOG_GC
#include <stdio.h>
#include "debug.h"
#endif

void *
reallocate(void *pointer, size_t oldSize, size_t newSize)
{
    if (newSize > oldSize) {
#ifdef DEBUG_STRESS_GC
        // running the GC right before we want to allocate more memory. A good stress test here.
        collectGarbage();
#endif
    }

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
#ifdef DEBUG_LOG_GC
    printf("%p free type %d\n", (void *)object, object->type);
#endif

    switch(object->type)
    {
        case OBJ_UPVALUE:
        {
            FREE(ObjUpvalue, object);
        } break;

        case OBJ_CLOSURE:
        {
            ObjClosure *closure = (ObjClosure *)object;
            FREE_ARRAY(ObjUpvalue *, closure->upvalues, closure->upvalueCount);
            closure->upvalues = NULL;
            closure->upvalueCount = 0;
            FREE(ObjClosure, object);
            closure->function = NULL;
        } break;

        case OBJ_FUNCTION:
        {
            ObjFunction *function = (ObjFunction *)object;
            freeChunk(&function->chunk);
            FREE(ObjFunction, object);
        } break;

        case OBJ_NATIVE: { FREE(ObjNative, object); } break;

        case OBJ_STRING:
        {
            ObjString *string = (ObjString *)object;
            FREE_ARRAY(char, string->chars, string->length + 1);
            FREE(ObjString, object);
        } break;

        default: { _assert(!"Invalid Path"); } break;
    }
}

void
markObject(Obj *object)
{
    if (object == NULL)
        return;
#ifdef DEBUG_LOG_GC
    printf("%p mark ", (void *)object);
    printValue(OBJ_VAL(object));
    printf("\n");
#endif
    object->isMarked = true;
}

/// @brief we only need to mark those values which live on the heap. Object types are the only ones which are heap
///        allocated. other literals like numbers, booleans are stored directly inline.
/// @param value if value is of an object type, mark it to initiate the mark sweep collect GC.
void
markValue(Value value)
{
    if (IS_OBJ(value))
        markObject(AS_OBJ(value));
}

/// @brief Mark phase for the mark-sweep-collect GC algo.
///        In this phase, we traverse the entire roots(allocations that are directly reachable and don't need
///        references to reach).
static void
markRoots()
{
    // mark the stack. all objects on the stack are heap allocated and come under GC.
    for (Value *slot = vm.stack.values;
         slot < vm.stack.top;
         slot++)
    {
        markValue(*slot);
    }

    // take care of the call frames used in the vm runtime. They have a pointer to the closure that will be called.
    for (int i = 0; i < vm.frameCount; ++i) {
        markObject((Obj *)vm.frames[i].closure);
    }

    // upvalues used by closures are also roots directly reachable by the VM and are also heap allocated
    for (ObjUpvalue *upvalue = vm.openUpvalues;
         upvalue != NULL;
         upvalue = upvalue->next)
    {
        markObject((Obj *)upvalue);
    }

    // take care of the heap allocated global hash table entries.
    markTable(&vm.globals);
    markCompilerRoots();
}

void
collectGarbage()
{
#ifdef DEBUG_LOG_GC
    printf("-- gc begin\n");
#endif

    markRoots();

#ifdef DEBUG_LOG_GC
    printf("-- gc end\n");
#endif
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
