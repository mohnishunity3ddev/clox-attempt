#include <stdlib.h>
#include "memory.h"
#include "vm.h"
#include "compiler.h"

#ifdef DEBUG_LOG_GC
#include <stdio.h>
#include "debug.h"
#endif

#define GC_HEAP_GROW_FACTOR 2

void *
reallocate(void *pointer, size_t oldSize, size_t newSize)
{
    vm.bytesAllocated += newSize - oldSize;
    if (newSize > oldSize) {
#ifdef DEBUG_STRESS_GC
        // running the GC right before we want to allocate more memory. A good stress test here.
        collectGarbage();
#endif

        if (vm.bytesAllocated > vm.nextGC) {
            collectGarbage();
        }
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
        case OBJ_CLASS:   { FREE(ObjClass, object); } break;

        case OBJ_UPVALUE: { FREE(ObjUpvalue, object); } break;

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

        case OBJ_NATIVE:   { FREE(ObjNative, object); } break;

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
    if (object == NULL) return;
    if (object->isMarked) return;

#ifdef DEBUG_LOG_GC
    printf("%p mark ", (void *)object);
    printValue(OBJ_VAL(object));
    printf("\n");
#endif

    object->isMarked = true;

    // object is seen to be reachable by the VM. Color it Gray.
    if (vm.grayCapacity < vm.grayCount + 1) {
        vm.grayCapacity = GROW_CAPACITY(vm.grayCapacity);
        vm.grayStack = (Obj **)realloc(vm.grayStack, sizeof(Obj *) * vm.grayCapacity);

        // NOTE: OS Kernel error response. Abort.
        if (vm.grayStack == NULL)
            exit(1);
    }

    vm.grayStack[vm.grayCount++] = object;
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

static void
markArray(ValueArray *array)
{
    for (int i = 0; i < array->count; ++i) {
        markValue(array->values[i]);
    }
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

/// @brief go through all outgoing references to heap allocations by the provided object and color them 'black',
///        black objects are not recorded in a separate variable. black objects are those which are marked to be
///        seen and are not inside the VM's grayStack.
/// @param object
static void
blackenObject(Obj *object)
{
#ifdef DEBUG_LOG_GC
    printf("%p blacken ", (void *)object);
    printValue(OBJ_VAL(object));
    printf("\n");
#endif

    switch (object->type)
    {
        // Native functions and string have no outgoing memory references. They are inherently 'black'
        case OBJ_NATIVE:
        case OBJ_STRING:
            break;

        case OBJ_CLASS:
        {
            ObjClass *klass = (ObjClass *)object;
            markObject((Obj *)klass->name);
        } break;

        // Each closure has a reference to the bare function it wraps, as well as an array of pointers to the
        // upvalues it captures. We trace all of those.
        case OBJ_CLOSURE:
        {
            ObjClosure *closure = (ObjClosure *)object;
            markObject((Obj *)closure->function);
            for (int i = 0; i < closure->upvalueCount; ++i) {
                markObject((Obj *)closure->upvalues[i]);
            }
        } break;

        // Each function has a reference to an ObjString containing the function’s name. More importantly, the
        // function has a constant table packed full of references to other objects
        case OBJ_FUNCTION:
        {
            ObjFunction *function = (ObjFunction *)object;
            markObject((Obj *)function->name);
            markArray(&function->chunk.constants);
        } break;

        // When an upvalue is closed, it contains a reference to the closed-over value. Since the value is no
        // longer on the stack, we need to make sure we trace the reference to it from the upvalue.
        case OBJ_UPVALUE:
        {
            markValue(((ObjUpvalue *)object)->closed);
        } break;
    }
}

/// @brief get a gray object(reachable directly by the VM) from the VM's grayStack, and then go through all the
/// allocation that are reachable through this gray object. AFter going through all the references of the gray
/// object, color it 'black'.
static void
traceReferences()
{
    while (vm.grayCount > 0) {
        Obj *object = vm.grayStack[--vm.grayCount];
        blackenObject(object);
    }
}

/// @brief sweep off all the 'white' objects which are not reachable by the GC.
static void
sweep()
{
    Obj *previous = NULL;
    Obj *object = vm.objects;
    while (object != NULL) {
        if (object->isMarked) {
            // Unmark for the next GC Mark-Sweep-Collect Cycle.
            object->isMarked = false;
            previous = object;
            object = object->next;
        } else {
            Obj *unreached = object;
            object = object->next;
            if (previous != NULL) {
                previous->next = object;
            } else {
                vm.objects = object;
            }

            freeObject(unreached);
        }
    }
}

void
collectGarbage()
{
#ifdef DEBUG_LOG_GC
    printf("-- gc begin\n");
    size_t before = vm.bytesAllocated;
#endif

    markRoots();
    traceReferences();
    // the string table only has key no value (its a hash set not a hash map). if we see that key object is not
    // marked in this table(i.e. not reachable) after the mark phase and tracingReferences routines in the GC has
    // completed. that means it should be deleted. There will no problem of dangling pointers because we are
    // calling tableDelete() which does this cleanly in this function
    tableRemoveWhite(&vm.strings);
    // Here, all gray objects are turned 'black' (and we want to hold on to them) and the grayStack is empty, now
    // we want to sweep off all the 'white' objects which are not reachable by the GC.
    sweep();

    vm.nextGC = vm.bytesAllocated * GC_HEAP_GROW_FACTOR;

#ifdef DEBUG_LOG_GC
    printf("-- gc end\n");
    printf("   collected %zu bytes (from %zu to %zu) next GC invocation at %zu\n",
           (before - vm.bytesAllocated), before, vm.bytesAllocated, vm.nextGC);
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
