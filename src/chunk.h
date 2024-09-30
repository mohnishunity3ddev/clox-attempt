#ifndef clox_chunk_h
#define clox_chunk_h

#include "common.h"
#include "value.h"

typedef enum
{
    /// @brief The 3 bytes succeeding this byte code is the index in the constant pool where the constant is
    /// stored.
    OP_CONSTANT_LONG,
    /// @brief The next byte after this opcode stores the index into which the constant was stored in the constant
    /// pool.
    OP_CONSTANT,
    OP_NIL,
    OP_TRUE,
    OP_FALSE,

    /// @brief This instruction pops the top value off the stack and forgets about it.
    OP_POP,
    /// @brief instruction to pop N elements from the stack at once. has an operand telling how many items to pop
    /// from the vm's stack.
    OP_POPN,
    /// @brief operand is 1 byte index into the globals index to get the string name of the global which is sent
    /// into a hashtable keyed by the string. The value set is already on the stack of the runtime vm.
    OP_DEFINE_GLOBAL,
    /// @brief 1 byte operand which is the index into the stack slots. All locals are there on the current call
    /// frame stack slot.
    OP_GET_LOCAL,
    OP_GET_GLOBAL,
    /// @brief  used when we want to access an upvalue variable referenced inside a closure. One byte operand tells
    ///         the index in the closure's upvalue array.
    OP_GET_UPVALUE,
    OP_SET_LOCAL,
    OP_SET_GLOBAL,
    OP_SET_UPVALUE,

    OP_GET_PROPERTY,
    OP_SET_PROPERTY,

    OP_EQUAL,
    OP_NOT_EQUAL,
    OP_GREATER,
    OP_GREATER_EQUAL,
    OP_LESS,
    OP_LESS_EQUAL,

    OP_ADD,
    OP_SUBTRACT,
    OP_MULTIPLY,
    OP_DIVIDE,
    OP_MOD,
    OP_NOT,
    /// @brief Unary operator for negative integers.
    OP_NEGATE,
    /// @brief OpCode to print the result onto the screen.
    OP_PRINT,
    /// @brief Jump instruction for an if statement. Has a 2 byte operand telling how many bytes of code to skip
    ///        (of the 'then' clause) if the 'if' condition was false
    OP_JUMP_IF_FALSE,
    /// @brief Jump instruction to skip over the 'else' clause of the 'if' statement if it's condition evaluates to
    ///        true.
    OP_JUMP,
    /// @brief Jump instruction which jumps the code backwards. The offset is given in it's 2 byte operand.
    OP_LOOP,
    /// @brief instruction to invoke a function call. All the arguments are already placed on the stack.
    OP_CALL,
    /// @brief emitted whenever the code sees a function, which parses the whole thing and then emits this
    /// instruction. one byte operand returns an index into value array having the function object.
    OP_CLOSURE,
    /// @brief Hoists the top local variable from the stack to the heap, allowing closures to access it after the
    /// enclosing function has returned. No operand needed.
    OP_CLOSE_UPVALUE,
    /// @brief This op code means return from a function.
    OP_RETURN,
    /// @brief instruction to create a new class object at runtime.
    OP_CLASS,
} OpCode;

typedef struct
{
    int line;
    int repetition;
} LineInfo;

typedef struct
{
    int count;
    int capacity;
    LineInfo *values;
} LineArray;

/// @brief The structure which groups chunk information
typedef struct
{
    /// @brief The number of bytes in the code
    int count;
    /// @brief The capacity of the code array
    int capacity;
    /// @brief The array which holds all the byte codes
    u8 *code;
    /// @brief The number of lines in this array.
    LineArray lines;
    /// @brief constants array which holds all the constants
    ValueArray constants;
} Chunk;

void initChunk(Chunk *chunk);
void freeChunk(Chunk *chunk);
void writeChunk(Chunk *chunk, u8 byte, int line);

/// @brief Adds a constant to the constants pool
/// @param chunk The chunk which has the constants pool
/// @param value The value to add.
/// @param line The line number where we saw the constant in the code
/// @return returns the index in the constant pool where the constant has been stored.
int addConstant(Chunk *chunk, Value value);
int writeConstant(Chunk *chunk, Value value, int line);

/// @brief returns the line number for an instruction at the given offset inside the chunk's code.
/// @param chunk Code chunk
/// @param offset offset into the chunk's code where the instruction is at.
/// @return returns the line number for the given instruction inside the given chunk.
int getLine(Chunk *chunk, size_t offset);

#endif