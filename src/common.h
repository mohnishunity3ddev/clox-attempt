#ifndef clox_common_h
#define clox_common_h

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
// #include <string.h>

#define DEBUG_PRINT_CODE
// #undef DEBUG_PRINT_CODE
#define DEBUG_TRACE_EXECUTION
// #undef DEBUG_TRACE_EXECUTION

#define DEBUG_STRESS_GC
#undef DEBUG_STRESS_GC
#define DEBUG_LOG_GC
#undef DEBUG_LOG_GC

typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;

#define UINT8_COUNT (0xff + 1)

#define _assert(expr)                                                                                             \
    if (!(expr))                                                                                                  \
    {                                                                                                             \
        *((volatile unsigned *)0) = 0;                                                                            \
    }

#endif