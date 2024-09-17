
#include "chunk.h"

// -((1.2 + 3.4) / 5.6) = -(4.6 / 5.6)
void arith01_test(Chunk *chunk);
// (1 * 2) + 3 = 5
void arith02_test(Chunk *chunk);
// 1 + 2 * 3 = 1 + (2 * 3) = 1 + 6 = 7
void arith03_test(Chunk *chunk);
// 3 - 2 - 1 = 0
void arith04_test(Chunk *chunk);
// 1. + 2. * 3. - 4. / -5. = (1. + (2. * 3.)) - (4. / -5.);
void arith05_test(Chunk *chunk);