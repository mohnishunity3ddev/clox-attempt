#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "common.h"
#include "compiler.h"
#include "scanner.h"
#include "value.h"
#include "memory.h"
#ifdef DEBUG_PRINT_CODE
#include "debug.h"
#endif

#define UNINITIALIZED_MARKER -1

typedef struct Compiler Compiler;
typedef struct
{
    Token current;
    Token previous;
    bool hadError;
    bool panicMode;
} Parser;

typedef enum
{
    PREC_NONE,
    PREC_ASSIGNMENT,
    PREC_OR,
    PREC_AND,
    PREC_EQUALITY,
    PREC_COMPARISON,
    PREC_TERM,
    PREC_FACTOR,
    PREC_MOD,
    PREC_UNARY,
    PREC_CALL,
    PREC_PRIMARY
} Precedence;

typedef void (*ParseFn)(bool canAssign);

/// @brief Defines a rule for parsing expressions
typedef struct {
    /// @brief Function pointer for prefix parse rule
    ParseFn prefix;
    /// @brief Function pointer for infix parse rule
    ParseFn infix;
    /// @brief Precedence level of the rule
    Precedence precedence;
} ParseRule;

/// @brief Represents a local variable in the compiler
typedef struct {
    /// @brief The token representing the variable's name
    Token name;
    /// @brief The depth of the scope in which the variable is declared
    int depth;
    /// @brief is it being referenced by a nested function(closure)?
    bool isCaptured;
} Local;

/// @brief Represents an upvalue (closed-over variable) in the compiler. An upvalue allows a closure to access
/// local variables from its enclosing functions, even after the parent functions have returned and the variable is
/// removed from the stack.
typedef struct {
    /// @brief Index of the upvalue in the upvalues list
    u8 index;
    /// @brief Flag indicating if the upvalue is local to the immediately enclosing function
    bool isLocal;
} Upvalue;

/// @brief Enumerates the types of functions that can be compiled
typedef enum {
    /// @brief User-defined function
    TYPE_FUNCTION_USER_DEFINED,
    /// @brief a method inside a class.
    TYPE_METHOD,
    /// @brief an initializer method inside a class.
    TYPE_INITIALIZER,
    /// @brief The main function of the program
    TYPE_FUNCTION_MAIN,
} FunctionType;

/// @brief Represents the state of the compiler during compilation
struct Compiler {
    /// @brief Pointer to the enclosing compiler (for nested functions)
    Compiler *enclosing;
    /// @brief The function being compiled
    ObjFunction *function;
    /// @brief The type of the function being compiled
    FunctionType type;
    /// @brief Array of local variables in the current scope
    Local locals[UINT8_COUNT];
    /// @brief Count of local variables in the current scope
    int localCount;
    /// @brief Array of upvalues for the function
    Upvalue upvalues[UINT8_COUNT];
    /// @brief Current depth of nested scopes
    int scopeDepth;
};

typedef struct ClassCompiler ClassCompiler;
struct ClassCompiler {
    ClassCompiler *enclosing;
    bool hasSuperClass;
};

Parser parser;
Chunk *compilingChunk;
Chunk *mainFunctionChunk;
Compiler *current = NULL;
// points to the inner most class being compiled.
ClassCompiler *currentClass = NULL;

static void binary(bool canAssign);
static void grouping(bool canAssign);
static void number(bool canAssign);
static void parseExpressionWithPrecedence(Precedence precedence);
static void unary(bool canAssign);
static void literal(bool canAssign);
static void string(bool canAssign);
static void variable(bool canAssign);
static void and_(bool canAssign);
static void or_(bool canAssign);
static void call(bool canAssign);
static void dot(bool canAssign);
static void statement();
static void declaration();
static void this_(bool canAssign);
static void super_(bool canAssign);

ParseRule rules[] = {
  [TOKEN_LEFT_PAREN]    = {grouping, call,   PREC_CALL},
  [TOKEN_RIGHT_PAREN]   = {NULL,     NULL,   PREC_NONE},
  [TOKEN_LEFT_BRACE]    = {NULL,     NULL,   PREC_NONE},
  [TOKEN_RIGHT_BRACE]   = {NULL,     NULL,   PREC_NONE},
  [TOKEN_COMMA]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_DOT]           = {NULL,     dot,    PREC_CALL},
  [TOKEN_MINUS]         = {unary,    binary, PREC_TERM},
  [TOKEN_PLUS]          = {NULL,     binary, PREC_TERM},
  [TOKEN_PERCENTAGE]    = {NULL,     binary, PREC_MOD},
  [TOKEN_SEMICOLON]     = {NULL,     NULL,   PREC_NONE},
  [TOKEN_SLASH]         = {NULL,     binary, PREC_FACTOR},
  [TOKEN_STAR]          = {NULL,     binary, PREC_FACTOR},
  [TOKEN_NOT]           = {unary,    NULL,   PREC_NONE},
  [TOKEN_NOT_EQUAL]     = {NULL,     binary, PREC_EQUALITY},
  [TOKEN_EQUAL]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_EQUAL_EQUAL]   = {NULL,     binary, PREC_EQUALITY},
  [TOKEN_GREATER]       = {NULL,     binary, PREC_COMPARISON},
  [TOKEN_GREATER_EQUAL] = {NULL,     binary, PREC_COMPARISON},
  [TOKEN_LESS]          = {NULL,     binary, PREC_COMPARISON},
  [TOKEN_LESS_EQUAL]    = {NULL,     binary, PREC_COMPARISON},
  [TOKEN_IDENTIFIER]    = {variable, NULL,   PREC_NONE},
  [TOKEN_STRING]        = {string,   NULL,   PREC_NONE},
  [TOKEN_NUMBER]        = {number,   NULL,   PREC_NONE},
  [TOKEN_AND]           = {NULL,     and_,   PREC_AND},
  [TOKEN_CLASS]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_ELSE]          = {NULL,     NULL,   PREC_NONE},
  [TOKEN_FALSE]         = {literal,  NULL,   PREC_NONE},
  [TOKEN_FOR]           = {NULL,     NULL,   PREC_NONE},
  [TOKEN_FUN]           = {NULL,     NULL,   PREC_NONE},
  [TOKEN_IF]            = {NULL,     NULL,   PREC_NONE},
  [TOKEN_NIL]           = {literal,  NULL,   PREC_NONE},
  [TOKEN_OR]            = {NULL,     or_,    PREC_OR},
  [TOKEN_PRINT]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_RETURN]        = {NULL,     NULL,   PREC_NONE},
  [TOKEN_SUPER]         = {super_,   NULL,   PREC_NONE},
  [TOKEN_THIS]          = {this_,    NULL,   PREC_NONE},
  [TOKEN_TRUE]          = {literal,  NULL,   PREC_NONE},
  [TOKEN_VAR]           = {NULL,     NULL,   PREC_NONE},
  [TOKEN_WHILE]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_ERROR]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_EOF]           = {NULL,     NULL,   PREC_NONE},
};

static ParseRule *
getRule(TokenType type)
{
    return &rules[type];
}

static void expression();
static ParseRule *getRule(TokenType type);

static Chunk *
currentChunk()
{
    Chunk *result = &current->function->chunk;
    return result;
}

static void
errorAt(Token *token, const char *message)
{
    if(parser.panicMode)
        return;
    parser.panicMode = true;
    fprintf(stderr, "[line %d] Error", token->line);
    if (token->type == TOKEN_EOF) {
        fprintf(stderr, " at end");
    } else if (token->type == TOKEN_ERROR) {
    } else {
        fprintf(stderr, " at '%.*s'", token->length, token->start);
    }
    fprintf(stderr, ": %s\n", message);
    parser.hadError = true;
}

static void
error(const char *message)
{
    errorAt(&parser.previous, message);
}

static void
errorAtCurrent(const char *message)
{
    errorAt(&(parser.current), message);
}

/// @brief advance the parser to the next token in the source file.
static void
advance()
{
    parser.previous = parser.current;
    for (;;)
    {
        parser.current = scanToken();
        if (parser.current.type != TOKEN_ERROR)
            break;
        errorAtCurrent(parser.current.start);
    }
}

/// @brief advance the parser to the next token if current token is of the expected type.
/// @param type type of the token, the current parser should be expected to be
/// @param message error message if current parser token is not of the expected type.
static void
consume(TokenType type, const char *message)
{
    if (parser.current.type == type) {
        advance();
        return;
    }
    errorAtCurrent(message);
}

/// @brief check if current token is of expected type.
/// @param type the token type to check for
/// @return true, if current token type is of expected type. false, otherwise.
static inline bool
check(TokenType type)
{
    return (parser.current.type == type);
}

/// @brief advance to next token if current is of given type.
/// @param type type of token we expect the current token to be
/// @return true, if expected token was present. false, otherwise.
static bool
match(TokenType type)
{
    if (!check(type))
        return false;
    advance();
    return true;
}

static void
emitByte(u8 byte)
{
    writeChunk(currentChunk(), byte, parser.previous.line);
}

static void
emitBytes(u8 byte1, u8 byte2)
{
    emitByte(byte1);
    emitByte(byte2);
}

static void
emitLoop(int loopStart)
{
    emitByte(OP_LOOP);
    // skips these two as well. That's why the [+2]
    int offset = currentChunk()->count - loopStart + 2;
    if (offset > 0xffff) {
        error("Loop body too large. Only have 2 bytes to store the offset");
    }
    emitByte((offset >> 8) & 0xff);
    emitByte(offset & 0xff);
}

static int
emitJump(u8 instruction)
{
    emitByte(instruction);
    emitByte(0xff);
    emitByte(0xff);
    return currentChunk()->count - 2;
}

static void
emitReturn()
{
    if (current->type == TYPE_INITIALIZER) {
        // load slot zero of the method which contains the instance in case of a class method.
        emitBytes(OP_GET_LOCAL, 0);
    } else {
        emitByte(OP_NIL);
    }

    emitByte(OP_RETURN);
}

/// @brief Add a constant to the constants array for the current compiler.
/// @param value the value to be added to constants array
/// @return index into the constants array where the value was added.
static u8
makeConstant(Value value)
{
    int constantIndex = addConstant(currentChunk(), value);
    if (constantIndex > 0xff)
    {
        error("Too many constants in one chunk");
        return 0;
    }
    return (u8)constantIndex;
}

static void
emitConstant(Value value)
{
    emitBytes(OP_CONSTANT, makeConstant(value));
}

static void
patchJump(int offset)
{
    int jump = currentChunk()->count - offset - 2;
    if (jump > 0xffff) {
        error("Too much code to jump over");
    }

    currentChunk()->code[offset] = (jump >> 8) & 0xff;
    currentChunk()->code[offset+1] = jump & 0xff;
}

/// @brief Initializes the compiler structure for a new function compilation
/// @param compiler Pointer to the Compiler structure to initialize
/// @param funcType Type of the function being compiled
static void
initCompiler(Compiler *compiler, FunctionType funcType)
{
    compiler->enclosing = current;
    compiler->function = NULL;
    compiler->type = funcType;
    compiler->localCount = 0;
    compiler->scopeDepth = 0;
    compiler->function = newFunction();
    current = compiler;
    if (funcType != TYPE_FUNCTION_MAIN) {
        current->function->name = copyString(parser.previous.start, parser.previous.length);
    }
    Local *local = &current->locals[current->localCount++];
    local->depth = 0;
    local->name.start = "";
    local->name.length = 0;
    local->isCaptured = false;

    // slot zero in function localslist is left out by default to represent the start of the function.
    // this is used when parsing 'this' keyword, it can be parsed as a local variable looked up as a local variable.
    // this is the frame slot where we receive the receiver(the object 'this' is referring to).]
    // we only want to do this for methods.
    if (funcType != TYPE_FUNCTION_USER_DEFINED) {
        local->name.start = "this";
        local->name.length = 4;
    } else { // normal functions won't have a 'this', only class methods can use 'this'
        local->name.start = "";
        local->name.length = 0;
    }

}

/// @brief Finalizes the current function compilation
/// @return Pointer to the compiled ObjFunction
static ObjFunction *
endCompiler()
{
    if (currentChunk()->code[currentChunk()->count - 1] != OP_RETURN) {
        emitReturn();
    }
    ObjFunction *function = current->function;
#ifdef DEBUG_PRINT_CODE
    if (!parser.hadError) {
        const char *chunkName = function->name != NULL ? function->name->chars
                                                       : "<main>";
        disassembleChunk(currentChunk(), chunkName);
    }
#endif
    // back to the enclosing function. global if defined in the global context.
    current = current->enclosing;
    return function;
}

/// @brief Increases the scope depth when entering a new block
static void
beginScope()
{
    current->scopeDepth++;
}

/// @brief Decreases the scope depth and emits code to pop local variables
static void
endScope()
{
    current->scopeDepth--;
    int popCount = 0;
    // pop local variables at runtime when scope has ended.
    while (current->localCount > 0 &&
           current->locals[current->localCount - 1].depth > current->scopeDepth)
    {
        if (current->locals[current->localCount - 1].isCaptured) {
            // hoist the local variable from the stack onto the heap since the function it is declared in is
            // exiting.
            emitByte(OP_CLOSE_UPVALUE);
        } else {
            // popCount++;
            emitByte(OP_POP);
        }
        current->localCount--;
    }

    _assert(popCount <= UINT8_COUNT);
    if (popCount > 0)
        emitBytes(OP_POPN, (u8)popCount);
}

/// @brief Parses an expression with a given precedence level
/// @param lowestPrecedenceAllowed The lowest precedence level allowed for this expression
static void
parseExpressionWithPrecedence(Precedence lowestPrecedenceAllowed)
{
    advance();
    ParseFn prefixRule_previousToken = getRule(parser.previous.type)->prefix;
    if (prefixRule_previousToken == NULL)
    {
        error("Expect expression");
        return;
    }

    // NOTE: avoid assigning a value to an addition exp like a + b = c or a + b.c = 3
    // prefix rule for '+' will call binary which will again call this function with precedence PREC_TERM + 1 which
    // will be greater than PREC_ASSIGNMENT, so canAssign will be false.
    bool canAssign = (PREC_ASSIGNMENT >= lowestPrecedenceAllowed);
    prefixRule_previousToken(canAssign);
    Precedence currentTokenPrecedence = getRule(parser.current.type)->precedence;

    while (currentTokenPrecedence >= lowestPrecedenceAllowed)
    {
        advance();
        ParseFn infixRule = getRule(parser.previous.type)->infix;
        infixRule(canAssign);
        currentTokenPrecedence = getRule(parser.current.type)->precedence;
    }
    if (canAssign && match(TOKEN_EQUAL)) {
        error("Invalid Assignment Target.");
    }
}

/// @brief Creates a constant for an identifier and returns its index.
/// @param name Pointer to the Token representing the identifier
/// @return Index into the current function codechunk's constant table where this new constant is stored.
static u8
identifierConstant(Token *name)
{
    ObjString *identifierString = copyString(name->start, name->length);
    u8 constantIndex = makeConstant(OBJ_VAL((Obj *)identifierString));
    return constantIndex;
}

/// @brief Compares two identifiers for equality
/// @param a Pointer to the first Token
/// @param b Pointer to the second Token
/// @return true if identifiers are equal, false otherwise
static bool
identifiersEqual(Token *a, Token *b)
{
    if (a->length != b->length)
        return false;
    bool areEqual = memcmp(a->start, b->start, a->length) == 0;
    return areEqual;
}

/// @brief Resolves a local variable in the current function's scope
/// @param compiler Pointer to the current Compiler
/// @param name Pointer to the Token representing the variable name
/// @return Index of the local variable, or -1 if not found
static int
resolveLocal(Compiler *compiler, Token *name)
{
    int result = -1;
    for (int i = compiler->localCount - 1; i >= 0; i--)
    {
        Local *local = &compiler->locals[i];
        if (identifiersEqual(&local->name, name)) {
            if (local->depth == UNINITIALIZED_MARKER) {
                error("Cannot read local variable in its own initializer.");
            }
            result = i;
            break;
        }
    }
    return result;
}

/// @brief Adds an upvalue to the current function's upvalue list
/// @param compiler Pointer to the current Compiler
/// @param localVarIndex Index of the local variable in the enclosing function
/// @param isLocal true if the variable is local to the immediately enclosing function
/// @return Index of the added upvalue
static int
addUpvalue(Compiler *compiler, u8 localVarIndex, bool isLocal)
{
    // is it an upvalue already there in the list.
    int upvalueCount = compiler->function->upvalueCount;
    for (int i = 0; i < upvalueCount; ++i)
    {
        Upvalue *upvalue = &compiler->upvalues[i];
        if (upvalue->index == localVarIndex &&
            upvalue->isLocal == isLocal) {
            return i;
        }
    }

    if (upvalueCount == UINT8_COUNT) {
        error("Too many unique closure variables in function");
        return 0;
    }

    // add upvalue to the list.
    compiler->upvalues[upvalueCount].isLocal = isLocal;
    compiler->upvalues[upvalueCount].index = localVarIndex;
    return compiler->function->upvalueCount++;
}

/// @brief Resolves an upvalue in the current or enclosing functions
/// @param compiler Pointer to the current Compiler
/// @param name Pointer to the Token representing the variable name
/// @return Index of the resolved upvalue, or -1 if not found
static int
resolveUpvalue(Compiler *compiler, Token *name)
{
    if (compiler->enclosing == NULL)
        return -1;

    Compiler *enclosingCompiler = compiler->enclosing;
    // is it a local in an immediate enclosing function
    int localsIndex = resolveLocal(enclosingCompiler, name);
    if (localsIndex != -1) {
        // local variable in this function is getting captured by a closure.
        enclosingCompiler->locals[localsIndex].isCaptured = true;
        return addUpvalue(compiler, (u8)localsIndex, true);
    }

    // if not, look up if it is referencing variable declared in parent functions hierarchy.
    int upvalue = resolveUpvalue(compiler->enclosing, name);
    if (upvalue != -1) {
        return addUpvalue(compiler, (u8)upvalue, false);
    }
    // this is not an upvalue.
    return -1;
}

/// @brief Adds a new local variable to the current function
/// @param name Token representing the variable name
static void
addLocal(Token name)
{
    if (current->localCount == UINT8_COUNT) {
        error("Too many local variables inside the current-scope/function.");
        return;
    }

    Local *local = &current->locals[current->localCount++];
    local->name = name;
    local->depth = UNINITIALIZED_MARKER;
    local->isCaptured = false;
}

/// @brief Declares a new local variable in the current scope, add to locals list. does nothing if global variable.
static void
declareVariable()
{
    // if global variable, return
    if (current->scopeDepth == 0)
        return;

    // is it already there in the locals list.
    Token *name = &parser.previous;
    for (int i = current->localCount - 1; i >= 0; i--)
    {
        Local *local = &current->locals[i];
        if (local->depth != UNINITIALIZED_MARKER &&
            local->depth < current->scopeDepth)
        {
            break;
        }

        if (identifiersEqual(name, &local->name)) {
            error("Already a variable with the same name inside this scope.");
        }
    }

    // add local to the locals list.
    addLocal(*name);
}

/// @brief Parses a variable declaration and returns its constant index
/// @param errorMessage Error message to display if parsing fails
/// @return Constant index of the parsed variable
static u8
parseVariable(const char *errorMessage)
{
    consume(TOKEN_IDENTIFIER, errorMessage);

    declareVariable();

    // global variables have scope = 0.
    if (current->scopeDepth > 0) return 0;

    // make new constant of the var name, return the constant index.
    u8 constantIndex = identifierConstant(&parser.previous);
    return constantIndex;
}

/// @brief Marks the most recently declared local variable as initialized
static inline void
markInitialized()
{
    if (current->scopeDepth == 0)
        return;

    current->locals[current->localCount - 1].depth = current->scopeDepth;
}

/// @brief Emits bytecode to define a global variable, initialize if just local variable
/// @param global Index of the global variable in the constant table, unused if local variable.
static void
defineVariable(u8 global)
{
    if (current->scopeDepth > 0) {
        markInitialized();
        return;
    }

    emitBytes(OP_DEFINE_GLOBAL, global);
}

/// @brief Parses function arguments and returns the argument count
/// @return Number of arguments parsed
static u8
argumentList()
{
    u8 argCount = 0;
    if (!check(TOKEN_RIGHT_PAREN)) {
        do {
            expression();
            if (argCount == 255) {
                error("Cannot have more than 255 arguments.");
            }
            argCount++;
        } while (match(TOKEN_COMMA));
    }

    consume(TOKEN_RIGHT_PAREN, "Expected ')' after arguments.");
    return argCount;
}

/// @brief Compiles a binary operation
/// @param canAssign true if the expression can be assigned to, Unused here.
static void
binary(bool canAssign)
{
    TokenType previousTokenOperatorType = parser.previous.type;
    ParseRule *rule = getRule(previousTokenOperatorType);
    parseExpressionWithPrecedence((Precedence)rule->precedence + 1);
    switch(previousTokenOperatorType)
    {
        case TOKEN_NOT_EQUAL:       { emitByte(OP_NOT_EQUAL); }     break;
        case TOKEN_EQUAL_EQUAL:     { emitByte(OP_EQUAL); }         break;
        case TOKEN_GREATER:         { emitByte(OP_GREATER); }       break;
        case TOKEN_GREATER_EQUAL:   { emitByte(OP_GREATER_EQUAL); } break;
        case TOKEN_LESS:            { emitByte(OP_LESS); }          break;
        case TOKEN_LESS_EQUAL:      { emitByte(OP_LESS_EQUAL); }    break;
        case TOKEN_PLUS:            { emitByte(OP_ADD);      }      break;
        case TOKEN_MINUS:           { emitByte(OP_SUBTRACT); }      break;
        case TOKEN_STAR:            { emitByte(OP_MULTIPLY); }      break;
        case TOKEN_SLASH:           { emitByte(OP_DIVIDE);   }      break;
        case TOKEN_PERCENTAGE:      { emitByte(OP_MOD);   }         break;
        default: return;
    }
}

/// @brief Compiles a function call
/// @param canAssign true if the expression can be assigned to, Unused here.
static void
call(bool canAssign)
{
    u8 argCount = argumentList();
    emitBytes(OP_CALL, argCount);
}

/// @brief Compiles a dot infix operator when accessing or setting fields of an object instance
/// @param canAssign true if this expression can be assigned a value(set)
static void
dot(bool canAssign)
{
    consume(TOKEN_IDENTIFIER, "Expect a property name after '.'");
    // put the field/property string into the constants array and return the index where it got put.
    u8 fieldNameIndex = identifierConstant(&parser.previous);

    // a + b.c = 3 should be error.
    if (canAssign && match(TOKEN_EQUAL)) {
        expression();
        emitBytes(OP_SET_PROPERTY, fieldNameIndex);
    } else
    // saw a property and a left paren after that, means this is a method call.
    // fuses the slow, OP_GET_PROPERTY and OP_CALL path into one instruction - more optimized.
    if (match(TOKEN_LEFT_PAREN)) {
        u8 argCount = argumentList();
        emitBytes(OP_INVOKE, fieldNameIndex);
        emitByte(argCount);
    } else {
        emitBytes(OP_GET_PROPERTY, fieldNameIndex);
    }
}

/// @brief Compiles a grouping expression (parentheses)
/// @param canAssign true if the expression can be assigned to, unused here.
static void
grouping(bool canAssign)
{
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

/// @brief Compiles a number literal
/// @param canAssign true if the expression can be assigned to, Unused here.
static void
number(bool canAssign)
{
    double value = strtod(parser.previous.start, NULL);
    emitConstant(NUMBER_VAL(value));
}

/// @brief Compiles a logical AND operation
/// @param canAssign true if the expression can be assigned to, Unused here.
static void
and_(bool canAssign)
{
    int endJump = emitJump(OP_JUMP_IF_FALSE);
    emitByte(OP_POP);
    parseExpressionWithPrecedence(PREC_AND);
    patchJump(endJump);
}

/// @brief Compiles a logical OR operation
/// @param canAssign true if the expression can be assigned to, Unused here.
static void
or_(bool canAssign)
{
    int elseJump = emitJump(OP_JUMP_IF_FALSE);
    int endJump = emitJump(OP_JUMP);
    patchJump(elseJump);
    emitByte(OP_POP);
    parseExpressionWithPrecedence(PREC_OR);
    patchJump(endJump);
}

/// @brief Compiles a unary operation
/// @param canAssign true if the expression can be assigned to, Unused here.
static void
unary(bool canAssign)
{
    TokenType operatorType = parser.previous.type;
    parseExpressionWithPrecedence(PREC_UNARY);
    switch(operatorType)
    {
        case TOKEN_NOT:   { emitByte(OP_NOT); }     return;
        case TOKEN_MINUS: { emitByte(OP_NEGATE); }  return;
        default: return;
    }
}

/// @brief Compiles a literal (true, false, nil)
/// @param canAssign true if the expression can be assigned to, Unused here.
static void
literal(bool canAssign)
{
    switch(parser.previous.type)
    {
        case TOKEN_FALSE: { emitByte(OP_FALSE); } break;
        case TOKEN_NIL:   { emitByte(OP_NIL);   } break;
        case TOKEN_TRUE:  { emitByte(OP_TRUE);  } break;
        default: return;
    }
}

/// @brief Compiles a string literal
/// @param canAssign true if the expression can be assigned to, Unused here.
static void
string(bool canAssign)
{
    ObjString *string = copyString(parser.previous.start + 1, parser.previous.length - 2);
    Value stringValue = OBJ_VAL((Obj *)string);
    emitConstant(stringValue);
}

/// @brief emits code  OP_GET/SET_LOCAL/GLOBAL/UPVALUE to load a variable with the given name onto the VM stack.
/// @param name Token representing the variable name
/// @param canAssign true if the expression can be assigned to
static void
namedVariable(Token name, bool canAssign)
{
    u8 getOp, setOp;
    // referenced variable can be a local inside the scope.
    int arg = resolveLocal(current, &name);
    if (arg != -1) {
        getOp = OP_GET_LOCAL;
        setOp = OP_SET_LOCAL;
    } else
    // variable referenced can be an upvalue in enclosing functions.
    if ((arg = resolveUpvalue(current, &name)) != -1) {
        getOp = OP_GET_UPVALUE;
        setOp = OP_SET_UPVALUE;
    } else {
        // referenced variable can be a global variable.
        getOp = OP_GET_GLOBAL;
        setOp = OP_SET_GLOBAL;
        arg = stringValueIndex(&mainFunctionChunk->constants, name.start, name.length);
        if (arg == -1)
        {
            if (check(TOKEN_LEFT_PAREN)) {
                arg = identifierConstant(&parser.previous);
            } else {
                errorAt(&name, "CompilerError (Undefined global variable)");
            }
        }
    }

    if (canAssign && match(TOKEN_EQUAL)) {
        expression();
        emitBytes(setOp, (u8)arg);
    } else {
        emitBytes(getOp, (u8)arg);
    }
}

/// @brief Compiles a variable expression. emits code  OP_GET/SET_LOCAL/GLOBAL/UPVALUE to load a variable with the
///        given name onto the VM stack.
/// @param canAssign true if the expression can be assigned to
static void
variable(bool canAssign)
{
    namedVariable(parser.previous, canAssign);
}

static Token
syntheticToken(const char *text)
{
    Token token;
    token.start = text;
    token.length = (int)strlen(text);
    return token;
}

/// @brief generate code for the VM to access a superclass method on the current instance where the "super" was
/// accessed.
/// @param canAssign unused here.
static void
super_(bool canAssign)
{
    if (currentClass == NULL) {
        error("Can't use 'super' outside of a class");
    } else if (!currentClass->hasSuperClass){
        error("Can't use 'super' in a class which does not have a superclass");
    }

    consume(TOKEN_DOT, "Expected '.' after the super keyword");
    consume(TOKEN_IDENTIFIER, "Expected superclass method name");
    u8 superclassMethodIndex = identifierConstant(&parser.previous);

    // push the instance stored in the hidden "this" onto the stack. (OP_GET_LOCAL)
    namedVariable(syntheticToken("this"), false);
    // we see a '(' immediately after the super's methodName when we want to invoke it.
    if (match(TOKEN_LEFT_PAREN)) {
        // push all the arguments onto the stack.
        u8 argCount = argumentList();
        // load the superclass onto the stack. (OP_GET_UPVALUE)
        namedVariable(syntheticToken("super"), false);
        // fuses the OP_GET_SUPER and OP_CALL into one optimized instruction which does the same thing.
        emitBytes(OP_SUPER_INVOKE, superclassMethodIndex);
        emitByte(argCount);
    } else {
        // push the superclass of the current class's method onto the stack. superclass is in its "super" variable
        // (OP_GET_UPVALUE)
        namedVariable(syntheticToken("super"), false);
        emitBytes(OP_GET_SUPER, superclassMethodIndex);
    }
}

/// @brief compiles the this keyword appearing inside a class method as a prefix op.
/// @param canAssign unused here.
static void
this_(bool canAssign)
{
    if (currentClass == NULL) {
        error("Cannot use 'this' outside a class declaration");
        return;
    }

    variable(false);
}

/// @brief Compiles any expression
static void
expression()
{
    parseExpressionWithPrecedence(PREC_ASSIGNMENT);
}

/// @brief Compiles a block of statements
static void
block()
{
    while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
        declaration();
    }
    consume(TOKEN_RIGHT_BRACE, "Expected '}' after a block.");
}

/// @brief Compiles a function declaration. pushes the closure value on top of vm stack.
/// @param type Type of the function being compiled
static void
function(FunctionType type)
{
    Compiler compiler;
    initCompiler(&compiler, type);
    beginScope();
    consume(TOKEN_LEFT_PAREN, "Expected a '(' after a function declaration");
    if (!check(TOKEN_RIGHT_PAREN)) {
        do {
            current->function->arity++;
            if (current->function->arity > 255) {
                errorAtCurrent("cannot have more than 255 arguments");
            }
            u8 constant = parseVariable("Expected Parameter Name");
            defineVariable(constant);
        } while(match(TOKEN_COMMA));
    }
    consume(TOKEN_RIGHT_PAREN, "Expected a ')' after a function declaration has ended.");
    consume(TOKEN_LEFT_BRACE, "Expected a '{' before function body.");
    block();

    ObjFunction *function = endCompiler();
    emitBytes(OP_CLOSURE, makeConstant(OBJ_VAL((Obj *)function)));
    for (int i = 0; i < function->upvalueCount; ++i) {
        emitByte(compiler.upvalues[i].isLocal ? 1 : 0);
        emitByte(compiler.upvalues[i].index);
    }
}

/// @brief parse class methods. emits OP_METHOD
static void
method()
{
    consume(TOKEN_IDENTIFIER, "Expected a method name");
    // parse method name
    u8 methodNameIndex = identifierConstant(&parser.previous);
    // parse method body
    FunctionType type = TYPE_METHOD;
    // Initializer method.
    if (parser.previous.length == 4 &&
        memcmp(parser.previous.start, "init", 4) == 0)
    {
        type = TYPE_INITIALIZER;
    }
    function(type);
    // which class these methods have to bind to. function closure is top of stack at this point(due to function())
    // abovc. class value under it.
    emitBytes(OP_METHOD, methodNameIndex);
}

static void
classDeclaration()
{
    consume(TOKEN_IDENTIFIER, "Expected a class name");

    Token className = parser.previous;

    // expecteed a name given for the class.
    u8 classNameIndex = identifierConstant(&parser.previous);
    declareVariable(); // does nothing if global class

    emitBytes(OP_CLASS, classNameIndex); // pushes the class value on stack
    defineVariable(classNameIndex);      // pops the class from the stack(OP_DEFINE_GLOBAL)

    // back link to current class. Support for nested classes.
    ClassCompiler classCompiler;
    classCompiler.enclosing = currentClass;
    classCompiler.hasSuperClass = false;
    currentClass = &classCompiler;

    // does this class inherit another class.
    if (match(TOKEN_LESS) || match(TOKEN_INHERITANCE)) {
        // consume the identifier meant for the super class name, equals parser.previous
        consume(TOKEN_IDENTIFIER, "Expected a superclass name");
        // push the super class onto the stack.
        variable(false);
        // subclass and superclass names cannot be the same.
        if (identifiersEqual(&className, &parser.previous)) {
            error("A class cannot inherit itself");
        }
        // add a local variable "super" in the subclass in it's own scope.
        // Ensures that if we declare two classes in the same scope, each has a different local slot to store its superclass
        // these 3 lines of code gives access to the superclass object from within the subclass's methods.
        ///
        // NOTE: the access is given by a variable called "super" which is a local outside of the method body, our
        // existing upvalue support enables the VM to capture that "super" local inside the body of a method
        // or even in functions nested inside that method.
        beginScope();
        addLocal(syntheticToken("super"));
        defineVariable(0);
        // push the subclass (which is inheriting) onto the stack.
        namedVariable(className, false);
        // emit the Inherit function.
        emitByte(OP_INHERIT);
        currentClass->hasSuperClass = true;
    }

    namedVariable(className, false);     // push class on stack. useful for binding the methods after this.
    consume(TOKEN_LEFT_BRACE, "Expected '{' before class body");
    // parse all class methods.
    while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
        method();
    }
    consume(TOKEN_RIGHT_BRACE, "Expected '}' after class body");
    // parsed all methods, don't need the class. pop it off.
    emitByte(OP_POP);
    if (classCompiler.hasSuperClass) {
        endScope();
    }

    // get back to the enclosing class. when outermost class body ends, enclosing will be NULL.
    currentClass = currentClass->enclosing;
}

/// @brief Compiles a function declaration statement
static void
functionDeclaration()
{
    u8 global = parseVariable("Expected function name");
    markInitialized();
    function(TYPE_FUNCTION_USER_DEFINED);
    defineVariable(global);
}

/// @brief Compiles a variable declaration statement
static void
varDeclaration()
{
    u8 global = parseVariable("Expected a variable name.");
    if (match(TOKEN_EQUAL)) {
        expression();
    } else {
        emitByte(OP_NIL);
    }
    consume(TOKEN_SEMICOLON, "Expected ';' after variable declaration.");
    defineVariable(global);
}

static void
expressionStatement()
{
    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after expression.");
    emitByte(OP_POP);
}

static void
forStatement()
{
    beginScope();
    consume(TOKEN_LEFT_PAREN, "Expected '(' after for statement.");

    if (match(TOKEN_SEMICOLON)) {
    } else if (match(TOKEN_VAR)) {
        varDeclaration();
    } else {
        expressionStatement();
    }

    int loopStart = currentChunk()->count;
    int exitJump = -1;

    if (!match(TOKEN_SEMICOLON)) {
        expression();
        consume(TOKEN_SEMICOLON, "Expected a ';' after loop condition.");
        exitJump = emitJump(OP_JUMP_IF_FALSE);
        emitByte(OP_POP);
    }

    if (!match(TOKEN_RIGHT_PAREN)) {
        int bodyJump = emitJump(OP_JUMP);
        int incrementStart = currentChunk()->count;
        expression();
        emitByte(OP_POP);
        consume(TOKEN_RIGHT_PAREN, "Expect ')' after for clause.");
        emitLoop(loopStart);
        loopStart = incrementStart;
        patchJump(bodyJump);
    }

    statement();
    emitLoop(loopStart);

    if (exitJump != -1) {
        patchJump(exitJump);
        emitByte(OP_POP);
    }

    endScope();
}

static void
ifStatement()
{
    consume(TOKEN_LEFT_PAREN, "[IF]: Expect '(' after an if statement");
    expression();
    consume(TOKEN_RIGHT_PAREN, "[IF]: Expect ')' after condition");
    int thenJump = emitJump(OP_JUMP_IF_FALSE);
    emitByte(OP_POP);
    statement();
    int elseJump = emitJump(OP_JUMP);
    patchJump(thenJump);
    emitByte(OP_POP);

    if (match(TOKEN_ELSE)) {
        statement();
    }
    patchJump(elseJump);
}

static void
printStatement()
{
    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after value.");
    emitByte(OP_PRINT);
}

static void
returnStatement()
{
    if (current->type == TYPE_FUNCTION_MAIN) {
        error("Cannot return from the implicit main function");
    }

    if (match(TOKEN_SEMICOLON)) {
        emitReturn();
    } else  {
        if (current->type == TYPE_INITIALIZER) {
            error("Cannot return a value from an initializer");
        }

        expression();
        consume(TOKEN_SEMICOLON, "Expected a ';' after the return statement");
        emitByte(OP_RETURN);
    }
}

static void
whileStatement()
{
    int loopStart = currentChunk()->count;
    consume(TOKEN_LEFT_PAREN, "Expect '(' after while.");
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after while condition.");
    int exitJump = emitJump(OP_JUMP_IF_FALSE);
    emitByte(OP_POP);
    statement();
    emitLoop(loopStart);
    patchJump(exitJump);
    emitByte(OP_POP);
}

static void
statement()
{
    if (match(TOKEN_PRINT)) {
        printStatement();
    } else if (match(TOKEN_FOR)) {
        forStatement();
    } else if (match(TOKEN_IF)) {
        ifStatement();
    } else if (match(TOKEN_RETURN)) {
        returnStatement();
    } else if (match(TOKEN_WHILE)) {
        whileStatement();
    } else if (match(TOKEN_LEFT_BRACE)) {
        beginScope();
        block();
        endScope();
    } else {
        expressionStatement();
    }
}

/// @brief Synchronizes the parser after an error
static void
synchronize()
{
    parser.panicMode = false;
    while (parser.current.type != TOKEN_EOF) {
        if (parser.previous.type == TOKEN_SEMICOLON)
            return;
        switch (parser.current.type) {
            case TOKEN_CLASS:
            case TOKEN_FUN:
            case TOKEN_VAR:
            case TOKEN_FOR:
            case TOKEN_IF:
            case TOKEN_WHILE:
            case TOKEN_PRINT:
            case TOKEN_RETURN:
                return;
            default:
            ;
        }
        advance();
    }
}

static void
declaration()
{
    if (match(TOKEN_CLASS)) {
        classDeclaration();
    } else if (match(TOKEN_FUN)) {
        functionDeclaration();
    } else if (match(TOKEN_VAR)) {
        varDeclaration();
    } else {
        statement();
    }

    if (parser.panicMode) {
        synchronize();
    }
}

/// @brief Main compilation function that processes the entire source code
/// @param source Pointer to the source code string
/// @return Pointer to the compiled ObjFunction, or NULL if compilation failed
ObjFunction *
compile(const char *source)
{
    initScanner(source);
    Compiler compiler;
    initCompiler(&compiler, TYPE_FUNCTION_MAIN);
    compilingChunk = &compiler.function->chunk;
    mainFunctionChunk = compilingChunk;
    parser.hadError  = false;
    parser.panicMode = false;
    advance();
    while(!match(TOKEN_EOF)) {
        declaration();
    }
    ObjFunction *function = endCompiler();
    return parser.hadError ? NULL : function;
}

void
markCompilerRoots()
{
    Compiler *compiler = current;
    while (compiler != NULL) {
        markObject((Obj *)compiler->function);
        compiler = compiler->enclosing;
    }
}