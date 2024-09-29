#ifndef clox_compiler_h
#define clox_compiler_h

#include "object.h"
#include "vm.h"

ObjFunction *compile(const char *source);
/// @brief mark all root allocations(directly reachable without any references in the middle) as reachable for
/// compiler.
void markCompilerRoots();

#endif