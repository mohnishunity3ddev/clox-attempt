#ifndef clox_common_h
#define clox_common_h

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
// #include <string.h>

#define DEBUG_TRACE_EXECUTION

typedef uint8_t u8;
typedef uint32_t u32;

#define _assert(expr)                                                                                             \
    if (!(expr))                                                                                                  \
    {                                                                                                             \
        *((volatile unsigned *)0) = 0;                                                                            \
    }

#endif