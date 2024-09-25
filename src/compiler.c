#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "compiler.h"
#include "scanner.h"
#include "value.h"

#ifdef DEBUG_PRINT_CODE
#include "debug.h"
#endif

#define UNINITIALIZED_MARKER -1

typedef struct
{
    Token current;
    Token previous;
    // were there errors during compilation.
    bool hadError;
    // The first error can confuse the compiler about where it is in the grammar.
    // and we dont want it to spew out errors left right and center. (Error Cascades).
    bool panicMode;
} Parser;

// precedence levels from lowest to highest.
typedef enum
{
    PREC_NONE,
    PREC_ASSIGNMENT, // =
    PREC_OR,         // or
    PREC_AND,        // and
    PREC_EQUALITY,   // == !=
    PREC_COMPARISON, // < > <= >=
    PREC_TERM,       // + -
    PREC_FACTOR,     // * /
    PREC_UNARY,      // ! -
    PREC_CALL,       // . ()
    PREC_PRIMARY
} Precedence;

typedef void (*ParseFn)(bool canAssign);
typedef struct
{
    ParseFn prefix;
    ParseFn infix;
    Precedence precedence;
} ParseRule;

/// @brief struct to describe info about a local variable.
typedef struct {
    /// @brief name / 'lexeme' of the local variable.
    Token name;
    /// @brief scope depth of the block where the local variable was declared.
    int depth;
    // TODO: Implement const variables. Do the same for global variables.
} Local;

/// @brief struct to keep track of the compiler state.
typedef struct {
    /// @brief flat array of all locals that are in scope during each point in the compilation process. ordered in
    ///        the array in the order that their declarations appear in code. Since we have a hard-limit on the
    ///        number of constants that the compiler can access(only one byte is used to get the index into this
    ///        array), so we have a fixed size of max locals that there can be inside one scope.Therefore, the max
    ///        number of locals that are allowed is 256 since 0-255 is the range that fits inside a byte.
    Local locals[UINT8_COUNT];

    /// @brief Number of locals in one local scope + earlier lesser scope depths since earlier less scope depth
    /// variables are visible in the current scope.
    int localCount;

    /// @brief Number of blocks 'surrounding' the bit of code that we are processing currently.
    ///        0 depth is the global scope, 1 is the first scope, 2 is the nested scope inside the first scope and
    ///        so on.
    int scopeDepth;
} Compiler;

Parser parser;
Chunk *compilingChunk;
Compiler *current = NULL;

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

// Blocks can contain declarations, and control flow statements can contain other statements. That means that these
// two functions will eventually be recursive. Hence, forward declaring them here.
static void statement();
static void declaration();

ParseRule rules[] = {
  [TOKEN_LEFT_PAREN]    = {grouping, NULL,   PREC_NONE},
  [TOKEN_RIGHT_PAREN]   = {NULL,     NULL,   PREC_NONE},
  [TOKEN_LEFT_BRACE]    = {NULL,     NULL,   PREC_NONE},
  [TOKEN_RIGHT_BRACE]   = {NULL,     NULL,   PREC_NONE},
  [TOKEN_COMMA]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_DOT]           = {NULL,     NULL,   PREC_NONE},
  [TOKEN_MINUS]         = {unary,    binary, PREC_TERM},
  [TOKEN_PLUS]          = {NULL,     binary, PREC_TERM},
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
  [TOKEN_SUPER]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_THIS]          = {NULL,     NULL,   PREC_NONE},
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
static void parseExpressionWithPrecedence(Precedence precedence);

static Chunk *
currentChunk()
{
    return compilingChunk;
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
        // Nothing to say.
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

static void
advance()
{
    parser.previous = parser.current;

    // After this, rest of the parser does not see any invalid token.
    // so we stop at the first valid token that the parser saw.
    for (;;)
    {
        parser.current = scanToken();
        if (parser.current.type != TOKEN_ERROR)
            break;

        errorAtCurrent(parser.current.start);
    }
}

// Checks to see if the current token is of the expected type, and then advances to the next token.
static void
consume(TokenType type, const char *message)
{
    if (parser.current.type == type) {
        advance();
        return;
    }

    errorAtCurrent(message);
}

// Returns true if the current token in the parser is of the given type.
static inline bool
check(TokenType type)
{
    return (parser.current.type == type);
}

// if the current token has the given type, we consume the token and return true. Otherwise, we leave the token
// alone and return false.
static bool
match(TokenType type)
{
    if (!check(type))
        return false;

    advance();
    return true;
}

// Emit bytecode (Actually generate bytecode instructions)
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

// [LOOP] instruction which jumps the code backwards instead of forwards like the normal [JUMP] instruction.
static void
emitLoop(int loopStart)
{
    emitByte(OP_LOOP);

    // how far back the instruction pointer is moved to the execute the loop code again.
    //
    // [loopstart] points to the offset in the chunk where the condition of the while statements gets executed.
    //
    // The operand to the loop instruction is 2 bytes, when we reach here we need to add these 2 bytes so that ip
    // skips these two as well. That's why the [+2]
    int offset = currentChunk()->count - loopStart + 2;
    if (offset > 0xffff) {
        error("Loop body too large. Only have 2 bytes to store the offset");
    }

    // high bits first.
    emitByte((offset >> 8) & 0xff);
    // low bits second.
    emitByte(offset & 0xff);
}

// Emits the jump instruction
static int
emitJump(u8 instruction)
{
    // Emit the actual jump instruction passed in.
    emitByte(instruction);
    // we use two bytes for the jump offset operand. A 16 bit offset lets us jump over 65536 bytes of bytecode.
    emitByte(0xff);
    emitByte(0xff);
    return currentChunk()->count - 2;
}

static void
emitReturn()
{
    emitByte(OP_RETURN);
}

static u8
makeConstant(Value value)
{
    // The index in the constants array where this constant will be stored in the chunk.
    int constantIndex = addConstant(currentChunk(), value);

    if (constantIndex > 0xff)
    {
        error("Too many constants in one chunk");
        return 0;
    }

    return (u8)constantIndex;
}

// emits the OP_CONSTANT bytecode instruction and then emits the index into the constant table for the actual
// constant so that it can be retrieved by the VM's run method.
static void
emitConstant(Value value)
{
    emitBytes(OP_CONSTANT, makeConstant(value));
}

static void
patchJump(int offset)
{
    // -2 to adjust for the bytecode for the jump offset itself which we are taking to be 16 bits.
    // this jump is basically telling how many bytes of code in there in the chunk after we emitted jump
    // instruction and its 2 byte operand.
    int jump = currentChunk()->count - offset - 2;

    // if the jump offset should fit inside 16 bits.
    if (jump > 0xffff) {
        error("Too much code to jump over");
    }

    // NOTE: patch the jump offset in the chunk's code with the actual value of the jump.
    // high 8 bits of the jump offset get stored first.
    currentChunk()->code[offset] = (jump >> 8) & 0xff;
    // low 8 bits of the jump offset get stored next.
    currentChunk()->code[offset+1] = jump & 0xff;

}

static void
initCompiler(Compiler *compiler)
{
    compiler->localCount = 0;
    compiler->scopeDepth = 0;
    current = compiler;
}

static void
endCompiler()
{
    emitReturn();
#ifdef DEBUG_PRINT_CODE
    if (!parser.hadError)
    {
        disassembleChunk(currentChunk(), "code");
    }
#endif
}

static void
beginScope()
{
    current->scopeDepth++;
}

static void
endScope()
{
    current->scopeDepth--;

    // when a scope ends, we need to remove all the local variables from the scope which ended just now. so that
    // they are invisible in the current scope.
    int popCount = 0;
    while (current->localCount > 0 &&
           current->locals[current->localCount - 1].depth > current->scopeDepth)
    {
        // Runtime component. Local variables occupy slots on the stack. when a local variable goes out of scope,
        // that slot is no longer needed and should be freed.
        // emitByte(OP_POP);
        ++popCount;

        // remove the local variable from the variable's list.
        current->localCount--;
    }

    // TODO: Allow more than 256 item pops.
    _assert(popCount <= UINT8_COUNT);

    emitBytes(OP_POPN, (u8)popCount);
}

static void
binary(bool canAssign)
{
    TokenType previousTokenOperatorType = parser.previous.type;
    ParseRule *rule = getRule(previousTokenOperatorType);

    // NOTE: if the expression is 1+2+3+4. After we see the first '+'
    // we only want the '2' after it, not the other ones coming after it.
    // I.E. we want to calculate it as (((1+2)+3)+4).
    // NOTE: When we call parsePrecedence, we want to compile the expression
    // for infix (not prefix) operators with precedence higher or equal to the one passed in.
    // In this example, if we have 1+2+3+4. if current is at 2 and previous is '+'
    // then parse precedence will advance with current at '+' and prev at 2. If will execute
    // number() to push the 2 onto the stack and the infix for the next '+'(before the 3)
    // will not execute since the '+' has the same precedence as rule->precedence here in
    // this function, and since we passed in the '+'s precedence + 1 to parsePrecedence
    // the '+' before the 3 will not execute for infix in parsePrecedence function will NOT
    // execute so we will return here and push the '+' operator here. Since the two numbers
    // 1 and 2 are already pushed to stack with the third thing pushed is the '+', the
    // parser correctly calculates 1+2 before proceeding 3 and 4 later.
    parseExpressionWithPrecedence((Precedence)rule->precedence + 1);
    // IMPORTANT: The above call to parsePrecedence is to evaluate the right hand
    // side of the binary operator.

    // After evaluating the right hand side of the bianry operation, NOW, we emit the bytecode instructor for the
    // binary operator.
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
        default: return;
    }
}

static void
grouping(bool canAssign)
{
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

static void
number(bool canAssign)
{
    double value = strtod(parser.previous.start, NULL);
    emitConstant(NUMBER_VAL(value));
}

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

    // NOTE: This is to stop a prefix rule like 'variable' to start parsing its right hand side of an assignment
    // operation if it is inside an expression which has an operator with an increased precedence.
    // For example: if we were parsing a * b = c + d in this function with current token at '*'
    // a) Prefix_rule will call variable() to parse the 'a' variable and return here.
    // b) After returning, we will enter the infix loop below, it will advance current to 'b' and call infix_rule
    //    for '*' which is binary()
    // c) binary will recursively call here(parseExpressionWithPrecedence) to parse the right hand side of the '*'
    //    operator.
    // d) At the beginning of this function, current token will be 'advance'd to next which is the '=' token.
    // e) then b's prefix_rule will be called which is variable() to parse variable 'b'.
    // f) Inside variable(), b will see that the next token is '=' and call this function again recursively and
    //    parse the assignment operator '=' if this canAssign was not passed in.
    // g) We had to do this because if the expression is a*b=c+d, then b=c+d should not be compiled because b is
    //    inside the 'a*b' expression. so, this canAssign checks whether the variable can be assigned to anything
    //    which is at the right hand side of the '=' operator if '=' operator is indeed the next token.
    // h) And this canAssign will be false if '*' from its infix rule called this function because there the
    //    precedence passed in WILL be higher than the PREC_ASSIGNMENT precedence value. Since this will be false,
    //    the prefix rule for parsing 'b' gets called with this canAssign set to false (inside variable()) and if
    //    it is false then even if b has an immediate next operator which is '=', it will NOT call this function
    //    again and we will not evaluate the right hand side of the '=' operator. Instead, we will move to the end
    //    of this function which will signal the error since the current token was '=' and still the right hand
    //    side has not been evaluated which is wrong.
    bool canAssign = lowestPrecedenceAllowed <= PREC_ASSIGNMENT;
    prefixRule_previousToken(canAssign);

    Precedence currentTokenPrecedence = getRule(parser.current.type)->precedence;
    // NOTE: Let's take an example - 1-2*3, as the parser is going through reading the expression from left to
    // right, we should evaluate 2*3 before evaluating 1-2, since * has higher precedence than '-'.
    // that's what we check here - the currentToken Precedence if its larger than the precedence passed in
    // THEN we evaluate the right hand operand for the infix operator.
    // NOTE: If it were 1*2-3, we see - here which has lower precedence than the previous *, the currentToken(-)'s
    // precedence is lower than '*'s precedence, so we dont compile the right hand side of the '-' operator,
    // and evaulate 1*2 and not 2-3 before it.
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

// Input is the token representing the identifier token presumably for a var declaration.
// this function makes a constant out of it, stores it in the vm's constants table and returns the index into the
// constants array where the identifier constant was placed.
static u8
identifierConstant(Token *name)
{
    ObjString *identifierString = copyString(name->start, name->length);
    u8 constantIndex = makeConstant(OBJ_VAL((Obj *)identifierString));

    return constantIndex;
}

// return true if the token names are the same.
static bool
identifiersEqual(Token *a, Token *b)
{
    if (a->length != b->length)
        return false;

    bool areEqual = memcmp(a->start, b->start, a->length) == 0;
    return areEqual;
}

// Try to find a local variable with the given "name". "compiler" only has variables in the current block scope and
// earlier scopes where the current scope is nested inside.
static int
resolveLocal(Compiler *compiler, Token *name)
{
    // if we go through the array without finding the local's name, then it should assumed that the token sent in
    // here represents a global variable instead. That's signaled by returning a -1 to the caller.
    int result = -1;

    // IMPORTANT: NOTE: We walk the array backwards so that we use the last declared variable with the same name.
    // This makes sure that we use a variable which was declared in the highest block scope. Since variables in
    // another lesser scope can have the same name.
    // This ensures that inner local variables correctly shadow locals with the same name in surrounding scopes.
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

// At this stage, the compiler records the presence of a local variable by adding the variable info into its list
// of local variables for the "current" scope.
static void
addLocal(Token name)
{
    // There is a hard limit to the number of local variables we can have inside a scope/function. since the index
    // is emitted as a byte and a byte can only store a max unsigned value of 255.
    if (current->localCount == UINT8_COUNT) {
        // TODO: Allow more than 256 local variables in the current scope.
        error("Too many local variables inside the current-scope/function.");
        return;
    }

    Local *local = &current->locals[current->localCount++];
    local->name = name;

    // NOTE: whenever we declare a variable we mark it uninitialized. Then when it is assigned a value its set to
    // initialized. if we try to access an uninitialized variable, compiler will report an error saying its
    // uninitialized.
    local->depth = UNINITIALIZED_MARKER;
}

// "declare" a local variable inside the current scope.
// This is how compiler records the presence of a local variable inside the current scope.
static void
declareLocalVariable()
{
    // this function only declares local variables inside a scopeDepth which is > 0.
    if (current->scopeDepth == 0)
        return;

    Token *name = &parser.previous;

    // Two local variables with the same name are allowed if they belong to different scopes. But two variables
    // with the same name are not allowed to be declared inside the same scope.
    for (int i = current->localCount - 1; i >= 0; i--)
    {
        Local *local = &current->locals[i];

        // we only need to check for locals inside the current scope. if we have reached a local which has
        // non-negative scope(i.e. is initialized to a value) and its depth is less than the current scope then it
        // is guaranteed that we have seen all local variables in the "current" scope and so that's when we break
        // out of the loop.
        if (local->depth != UNINITIALIZED_MARKER &&
            local->depth < current->scopeDepth)
        {
            break;
        }

        // if the names are same, that means there are two variable declarations with the same name.
        if (identifiersEqual(name, &local->name)) {
            error("Already a variable with the same name inside this scope.");
        }
    }

    // Add the local variable name to the list of local variables the compiler has seen for the current scope.
    addLocal(*name);
}

// after the 'var' keyword, we expect a name for the variable. which is an identifier.
// The name of the variable is stored in the constant table as a string and then returns the index into the
// constants array so that it can be retrieved later.
static u8
parseVariable(const char *errorMessage)
{
    consume(TOKEN_IDENTIFIER, errorMessage);

    // Add support for local variables.
    declareLocalVariable();
    // we return if the variable is a local variable. That only happens if the current scope depth is > 0 which
    // means the variable was seen declared inside a local scope. Local variables unlike global variables are not
    // looked up by their name in the VM. So we return a dummy table index. Global variable names are stored in a
    // constant table and the calling function expects the index into the contants array where the global's name's
    // string is stored.
    if (current->scopeDepth > 0)
        return 0;

    // store the global variable's (in scope 0) name in a constant table. Global variables are looked up by name in
    // the VM's run loop. Local variables are not.
    u8 constantIndex = identifierConstant(&parser.previous);

    return constantIndex;
}

static inline void
markInitialized()
{
    // Giving the last local variable a non-negative depth(current scope depth) means it is initialized.
    current->locals[current->localCount - 1].depth = current->scopeDepth;
}

// This outputs the bytecode instruction that defines the new variable and stores its initial value in the vm's
// hash table.
// The index of the variable's name in the constant array is the instruction's operand (representing the name of
// the var).
static void
defineVariable(u8 global)
{
    // return from the function if it is local variable i.e. it is declared inside a local block with scopeDepth >
    // 0.
    if (current->scopeDepth > 0) {
        markInitialized();
        return;
    }

    emitBytes(OP_DEFINE_GLOBAL, global);
}

// The and operator evaluates to true if both its left hand and right hand expressions are true.
// if the left hand expression is false, then it does not need to evaluate its right hand expression, becuase the
// whole thing will evaluate to false if either one of them is false.
// Control Flow:
// 1. left hand expression      { the result of this is on top of the stack. }
// 2. OP_JUMP_IF_FALSE          { instruction to check the condition and if false, skip code based on the operand}
// 3. OP_POP                    { pop the result of the left hand expression evaluated before coming here. }
// 4. right hand expression     { evaluate the right hand expression and post it's result on top of the stack. }
static void
and_(bool canAssign)
{
    int endJump = emitJump(OP_JUMP_IF_FALSE);

    // if the left expression true, vm comes here and we pop it off the stack.
    // if the left expression came out to be false, we let it remain on top of the stack since that just tells the
    // result of the whole and expression(guaranteed to be false).
    emitByte(OP_POP);
    // evaluate the right hand expression
    parseExpressionWithPrecedence(PREC_AND);

    // write the number of bytes of code to skip if the jump happened(when the condition was false)
    // this skips the parseWithExpression call above to evaluate the right hand expression.
    patchJump(endJump);
}

// This is the infix OR operator.
//
// This binary operation evaluates to true if either one of the expression it is between is true. so if we see the
// left hand expression and that is true, then we skip the right hand expression because the result of the whole
// expression will be true.
//
// Control Flow:
// 1. left hand expression      { the result of this is on top of the stack. }
// 2. OP_JUMP_IF_FALSE          { Jump code if the above expression came out to be false. }
// 3. OP_JUMP                   { Jump code regardless }
// 4. OP_POP                    { pop off the result of the left hand expression from the stack(guaranteed to be false). }
// 5. right hand expression     { parse right and put the result of this whole binary op on top of the stack. }
static void
or_(bool canAssign)
{
    int elseJump = emitJump(OP_JUMP_IF_FALSE);
    int endJump = emitJump(OP_JUMP);
    // -> skip the above endJump statement - that should be 3 bytes of code - JUMP_IF_FALSE + 2 bytes of operand.
    //
    // -> if the left hand exp spits out false, then we need to evaluate the right hand expression, so we skip the
    //    above 'endJump' statement and let the code parse the right hand expression.
    //
    // -> if the left hand exp spits out true, then we get to the above 'endJump' statement, which does skip the
    //    code parsing the right hand expression - which is right since 'or' is true when either one of the 'or'
    //    operands is true. so if the left exp is true, we skip parsing the right hand exp.
    patchJump(elseJump);
    // pop the result of the left hand expression off the stackTop since that is true, we are going to evaluate
    // the right hand expression which will decide the result of this 'or' expression.
    emitByte(OP_POP);
    // parse the right hand expression.
    parseExpressionWithPrecedence(PREC_OR);
    // jump the right hand expression code.
    patchJump(endJump);
}

static void
unary(bool canAssign)
{
    TokenType operatorType = parser.previous.type;

    // Compile the operand.
    // look for the Unary operator and all operators of higher precedence than unary in the expression.
    parseExpressionWithPrecedence(PREC_UNARY);

    // Emit the operator instruction
    switch(operatorType)
    {
        case TOKEN_NOT:   { emitByte(OP_NOT); }     return;
        case TOKEN_MINUS: { emitByte(OP_NEGATE); }  return;
        default: return;
    }
}

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

static void
string(bool canAssign)
{
    ObjString *string = copyString(parser.previous.start + 1, parser.previous.length - 2);

    // NOTE: This was for allocating a single contiguous memory for both the string object and the character array
    // for the strings.
    // ObjString *string = makeString(parser.previous.start + 1, parser.previous.length - 2, false);
    Value stringValue = OBJ_VAL((Obj *)string);
    emitConstant(stringValue);
}

// This function gets called when the compiler encounters an identifier for a variable which should already be
// declared before - which means it should already be present in the constant array of the compiler. So we get
// the index of the constant in the constant array.
static void
namedVariable(Token name, bool canAssign)
{
    u8 getOp, setOp;

    // Try to find a local variable with the given name. If we find one, we use the instructions for working
    // with locals. Otherwise, it is a global variable.
    int arg = resolveLocal(current, &name);
    if (arg != -1) {
        getOp = OP_GET_LOCAL;
        setOp = OP_SET_LOCAL;
    } else {
        getOp = OP_GET_GLOBAL;
        setOp = OP_SET_GLOBAL;

        // Get the index into the constants array where the name of the global variable is stored.
        // This will be -1 if the global variable name is not present in the constants array(i.e. it has not been
        // declared yet)
        arg = stringValueIndex(&currentChunk()->constants, name.start);
        if (arg == -1)
        {
            // add the name token's lexeme into the constants array and get the index.
            // NOTE: We are doing this to support users using a variable before it is declared. if the string is not
            // already in the constants array, then we add a new string constant into the array so that it can be
            // looked up by the vm's runtime to fetch the value before it is defined and not produce a "undefined
            // variable" error.
            // arg = identifierConstant(&name); // NOTE: Not doing this right now.
            errorAt(&name, "CompilerError (Undefined global variable)");
        }
    }

    // If there is an equal '=' sign after the variable, that means this is a setter for that variable.
    // Otherwise this must be for value access i.e. a getter.
    if (canAssign && match(TOKEN_EQUAL)) {
        expression();
        emitBytes(setOp, (u8)arg);
    } else {
        // emit the instructions "get" to tell the vm that we want to retrieve the value of this  variable.
        emitBytes(getOp, (u8)arg);
    }
}

static void
variable(bool canAssign)
{
    namedVariable(parser.previous, canAssign);
}

static void
expression()
{
    // compile the expression only looking for literals that are given and literals that have higher precedence
    // than the given one.
    parseExpressionWithPrecedence(PREC_ASSIGNMENT);
}

static void
block()
{
    // A block starts with a leftBrace '{' and goes till its ending closingBrace '}'.
    // It has a collection of declarations and statements inside its body.
    while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
        declaration();
    }

    consume(TOKEN_RIGHT_BRACE, "Expected '}' after a block.");
}

// variable declaration parsing begins here.
static void
varDeclaration()
{
    // var keyword is followed by a variable name. That's handled here.
    // NOTE: Global Variables are looked up by name at runtime. That means the VM - the bytecode interpreter loop -
    // needs access to its name. A whole string is too big to stuff into the bytecode stream as an operand.
    // Instead, we store the string in the constant table and the instruction then refers to the name by its index
    // in the table. Here, 'global' is that index into the constant table.
    u8 global = parseVariable("Expected a variable name.");

    // after the variable declaration, we look for an = followed by an initialization expression.
    // The right hand side of the '=' most likely will be a value/expression which is parsed and the value is also
    // pushed into the compiling chunk.
    if (match(TOKEN_EQUAL)) {
        expression();
    } else {
    // if there is no = after the var declaration, the compiler implicitly initializes it to nil.
        emitByte(OP_NIL);
    }

    // We expect a variable declaration to be ended by a ';'.
    consume(TOKEN_SEMICOLON, "Expected ';' after variable declaration.");

    // IMPORTANT: NOTE:
    // When we get here, the name of the variable is stored in the constants array(if its a global variable). The
    // index of which is this 'global' variable here. The value it stores inside of it is also pushed into the
    // chunk already. So the OP_CONSTANT comes before this OP_GLOBAL_DEFINE instruction (followed by the name of
    // the variable) which is the value this var represents. We do this so that when the VM encounters the
    // OP_GLOBAL_DEFINE in it's 'loop', the OP_CONSTANT is already pushed onto the stack(since it came before this
    // OP_DEFINE instruction) and so the vm can comfortably get the value by using pop() from the stack and it
    // would correctly 'assign' this constant value to this variable.
    defineVariable(global);
}

static void
expressionStatement()
{
    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after expression.");
    // an expression statement is just an expression followed by a semicolon.
    // Semantically, an expression statement evaluates the expression and discards the result. The compiler
    // directly encodes this behavior. It compiles the expression, and then emits the OP_POP instruction.
    emitByte(OP_POP);
}

// For loops - for (var i = 0; i < 10; i += 1)
// Control Flow:
// 1.  Initializer Clause           <-> var i = 0;
// 2.  Condition Expression.        <-> check if i < 10. If it is execute the 'body' of the loop.
// 3.  OP_JUMP_IF_FALSE             <-> **Jumps to 11** skip the for loop altogether.
// 4.  OP_POP                       <-> Pop the condition result off the stack.
// 5.  OP_JUMP                      <-> **Jumps to 9** execute the 'body' of the 'for' loop.
// 6.  Increment Expression         <-> increment i - i += 1
// 7.  OP_POP                       <-> pop the value of i off the stack before executing the body.
// 8.  OP_LOOP                      <-> **Jumps to 2** it is after incrementing that we go back and check the condition clause.
// 9.  Body Statement               <-> compile the code inside the 'for' body.
// 10. OP_LOOP                      <-> **Jumps to 6** run the increment expression after the execution of the 'body'
// 11. OP_POP                       <-> Pop the condition result off the stack.
// continues...
static void
forStatement()
{
    // if there are variable declarations inside the for loop clause, we need them to be visible until the for loop
    // has not been done.
    beginScope();
    consume(TOKEN_LEFT_PAREN, "Expected '(' after for statement.");

    // -- Initialization Clause --
    // Handling the for loop initialization part.
    if (match(TOKEN_SEMICOLON)) {
        // No Initializer of the 'for' loop.
    } else if (match(TOKEN_VAR)) {
        // Initializer present
        varDeclaration();
    } else {
        // The initializer variable has been declared before the start of the for loop. So no 'var' present here.
        // This consumes the semicolon and pops the value of the expression result off the stack.
        expressionStatement();
    }

    // -- Condition Clause --
    // 'for' loop loops back to this condition check. The initialization above only happens the first time a 'for'
    // loop was encountered. Right before the condition expression.
    int loopStart = currentChunk()->count;
    int exitJump = -1;
    // This is not an infinite loop - there's an actual condition check present here.
    if (!match(TOKEN_SEMICOLON)) {
        // compile the condition expression.
        expression();
        // eat up the semicolon after the condition expression.
        consume(TOKEN_SEMICOLON, "Expected a ';' after loop condition.");
        // Jump out of the 'for' loop when the condition is false. This is the instruction that causes the end of a
        // for loop.
        exitJump = emitJump(OP_JUMP_IF_FALSE);
        // pop the condition exp result off the stack before executing the body of the 'for' loop.
        emitByte(OP_POP);
    }

    // -- Increment Clause --
    // The peculiar thing about the increment clause is that in code, it appears textually before the 'for' body
    // but is executed after the completion of one iteration of the 'for' body if the condition exp evaluates to
    // true.
    //
    // what we do here is we jump over the increment, run the body, jump back up to the increment, run it, and then
    // go to the next iteration of the loop.
    if (!match(TOKEN_RIGHT_PAREN)) {
        // Unconditional Jump instruction.
        // Jump over the increment expression since that is executed after the 'for' body has executed.
        int bodyJump = emitJump(OP_JUMP);
        // offset into the chunk's code where the increment code is actually present.
        int incrementStart = currentChunk()->count;
        // compile the increment expression.
        expression();
        // the above expression is likely an assignment. compile it for it's side effect and remove the value off
        // the stack.
        emitByte(OP_POP);
        // consume the ')' marking the end of the for expression.
        consume(TOKEN_RIGHT_PAREN, "Expect ')' after for clause.");
        // this emits the OP_LOOP instruction which moves the instruction pointer back to the beginning of the for
        // loop - right before the condition expression if there is one. Doing this here because the increment
        // 'appears' before but is actually executed at the end of the body.
        emitLoop(loopStart);
        // change loopStart to point to the offset where the increment expression begins in the chunk's code.
        // This will make it so that after we emit the loop instruction after the end of the body below, the loop
        // instruction there will make the instruction pointer move to the point where the increment expression is
        // done. This is how we weave the increment in to run after the execution of the body.
        loopStart = incrementStart;
        // skip all increment code and jump straight to the code of the 'for' body.
        patchJump(bodyJump);
    }

    // -- For Loop Body Compilation --
    statement();

    emitLoop(loopStart);
    // if the condition was false(inside the condition expression), the for loop body should be skipped entirely.
    // exitJump will not be equal to -1 if there existed a condition clause.
    if (exitJump != -1) {
        // we do this only when there is a condition clause, without it - we have no jump instruction to patch and
        // no value of the condition exp result to pop from the stack.
        patchJump(exitJump);
        emitByte(OP_POP);
    }
    // Ends the scope for the loop.
    endScope();
}

// We 'match'ed with an if statement
// Flow that we want is:
// 1 - if (condition)       { condition gets put on top of the stack }
// 2 - OP_JUMP_IF_FALSE     { jumps to 6 if the condition was false }
// 3 - OP_POP               { pops the condition off the stack }
// 4 - then {}              { executes the then block. }
// 5 - OP_JUMP              { Jumps to 8 }
// 6 - OP_POP               { pops the condition off the stack. }
// 7 - else {}              { executes the else block }
// 8 - IF_THEN_ELSE_END     { end of the if statement blocks }
static void
ifStatement()
{
    // After 'if', expect '(' and compile the condition expression.
    consume(TOKEN_LEFT_PAREN, "[IF]: Expect '(' after an if statement");
    expression();
    consume(TOKEN_RIGHT_PAREN, "[IF]: Expect ')' after condition");

    // Emit OP_JUMP_IF_FALSE with a placeholder offset to be patched later.
    int thenJump = emitJump(OP_JUMP_IF_FALSE);
    // Pop the condition result if the jump did not happen (i.e., condition is true).
    emitByte(OP_POP);

    // Compile the 'then' clause.
    statement();

    // Emit OP_JUMP to skip the 'else' clause if the 'then' clause was executed.
    int elseJump = emitJump(OP_JUMP);

    // Backpatch the offset for OP_JUMP_IF_FALSE to skip the 'then' clause if the condition is false.
    patchJump(thenJump);
    emitByte(OP_POP);

    // If there's an 'else', compile it.
    if (match(TOKEN_ELSE)) {
        statement();
    }

    // Patch the OP_JUMP to skip the 'else' clause if the 'then' clause was executed.
    patchJump(elseJump);
}

// A print statement evaluates an expression and then prints the result. So we first parse and compile the
// expression. The grammar requires a ';' after the print statement, so we consume it and then emit the print token
// to the stack to print the result.
static void
printStatement()
{
    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after value.");
    emitByte(OP_PRINT);
}

// difference between while and if here is just the loop instruction, otherwise, its all the same.
// Control Flow:
// 1. Condition Expression          { Condition expression after the while statement }
// 2. OP_JUMP_IF_FALSE              { Jump forwards if the condition above evaluates to false. } **Jumps to 6**
// 3. OP_POP                        { Pop the condition result off the stack. }
// 4. Body of the while statement   {}
// 5. OP_LOOP                       { Loop instruction to jump the code backwards to the while condition to check it again. }
///                                 <--This one Jumps to 1-->
// 6. OP_POP                        { Pop the condition expression result off the stack. }
static void
whileStatement()
{
    // caching the address after the while for the loop instruction which will jump the ip backwards to this point
    // so that the condition is evaluated once again.
    int loopStart = currentChunk()->count;

    consume(TOKEN_LEFT_PAREN, "Expect '(' after while.");
    // puts the condition result on top of the stack.
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after while condition.");

    // condition jump if the condition result for the above expression is false.
    int exitJump = emitJump(OP_JUMP_IF_FALSE);
    // pop the result of the condition off the top of the stack.
    emitByte(OP_POP);
    // parse the while block.
    statement();
    // Jump back to the start of the while so that the code keeps looping until the condition is false.
    emitLoop(loopStart);
    // backpatch the operand of the jump instruction, so that it can skip the while block code when the condition
    // is false.
    patchJump(exitJump);
    // if the above jump happened, the condition exp is still on the top of stack. Pop it off since we dont need
    // it.
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
    } else if (match(TOKEN_WHILE)) {
        whileStatement();
    } else if (match(TOKEN_LEFT_BRACE)) {
        beginScope();
        block();
        endScope();
    } else {
        // if we don't see a print keyword, then we must be looking at an expression statement.
        expressionStatement();
    }
}

// if we hit a compile error while parsing the previous statement, we enter panic mode.
// When that happens, after the statement we start synchronizing.
static void
synchronize()
{
    parser.panicMode = false;

    // We skip tokens indiscriminately until we reach something that looks like a statement boundary.
    // We recognize the boundary by looking for a preceding token that can end a statement, like a semicolon.
    // Or we'll look for a subsequent token that begins a statement, usually one of the control flow or declaration
    // keywords.
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
            ; // Do nothing.
        }

        advance();
    }
}

static void
declaration()
{
    // if we match a var declaration, we jump to parsing variable declarations.
    if (match(TOKEN_VAR)) {
        varDeclaration();
    } else {
        statement();
    }

    // We use panic mode error recovery to minimize the number of cascaded compile errors that it reports.
    // The compiler exits panic mode when it reaches a synchronization point. Here, we choose statement boundaries
    // as that point.
    if (parser.panicMode) {
        synchronize();
    }
}

bool
compile(const char *source, Chunk *chunk)
{
    initScanner(source);

    // Initialize the compiler from an empty state.
    Compiler compiler;
    initCompiler(&compiler);

    compilingChunk = chunk;

    parser.hadError  = false;
    parser.panicMode = false;

    advance();

    // keep compiling declarations until we reach the end of the file.
    while(!match(TOKEN_EOF)) {
        declaration();
    }

    endCompiler();
    return !parser.hadError;
}