#ifndef clox_common_h
#define clox_common_h

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

typedef uint8_t u8;
typedef uint32_t u32;

#define _assert(expr)                                                                                             \
    if (!(expr))                                                                                                  \
    {                                                                                                             \
        *((volatile unsigned *)0) = 0;                                                                            \
    }

#endif