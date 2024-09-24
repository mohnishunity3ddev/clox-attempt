#include <stdio.h>
#include <stdlib.h>

#include "common.h"
#include "compiler.h"
#include "scanner.h"
#include "value.h"

#ifdef DEBUG_PRINT_CODE
#include "debug.h"
#endif

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

Parser parser;
Chunk *compilingChunk;

static void binary(bool canAssign);
static void grouping(bool canAssign);
static void number(bool canAssign);
static void parseExpressionWithPrecedence(Precedence precedence);
static void unary(bool canAssign);
static void literal(bool canAssign);
static void string(bool canAssign);
static void variable(bool canAssign);

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
  [TOKEN_AND]           = {NULL,     NULL,   PREC_NONE},
  [TOKEN_CLASS]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_ELSE]          = {NULL,     NULL,   PREC_NONE},
  [TOKEN_FALSE]         = {literal,  NULL,   PREC_NONE},
  [TOKEN_FOR]           = {NULL,     NULL,   PREC_NONE},
  [TOKEN_FUN]           = {NULL,     NULL,   PREC_NONE},
  [TOKEN_IF]            = {NULL,     NULL,   PREC_NONE},
  [TOKEN_NIL]           = {literal,  NULL,   PREC_NONE},
  [TOKEN_OR]            = {NULL,     NULL,   PREC_NONE},
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
        fprintf(stderr, " at '%*.s'", token->length, token->start);
    }

    fprintf(stderr, ": %s\n", message);
    parser.hadError = true;
}

static void
error(const char *message)
{
    errorAt(&(parser.previous), message);
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

// after the 'var' keyword, we expect a name for the variable. which is an identifier.
// The name of the variable is stored in the constant table as a string and then returns the index into the
// constants array so that it can be retrieved later.
static u8
parseVariable(const char *errorMessage)
{
    consume(TOKEN_IDENTIFIER, errorMessage);
    u8 constantIndex = identifierConstant(&parser.previous);

    return constantIndex;
}

// This outputs the bytecode instruction that defines the new variable and stores its initial value.
// The index of the variable's name in the constant array is the instruction's operand (representing the name of
// the var).
static void
defineVariable(u8 global)
{
    emitBytes(OP_DEFINE_GLOBAL, global);
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

static void
namedVariable(Token name, bool canAssign)
{
    // This function gets called when the compiler encounters an identifier for a variable which should already be
    // declared before - which means it should already be present in the constant array of the compiler. So we get
    // the index of the constant in the constant array.
    const char *variableName = parser.previous.start;
    int stringIndex = stringValueIndex(&currentChunk()->constants, variableName);

    u8 arg;
    if (stringIndex >= 0) {
        arg = (u8)stringIndex;
    } else {
        // add the name token's lexeme into the constants array and get the index.
        // NOTE: We are doing this to support users using a variable before it is declared. if the string is not
        // already in the constants array, then we add a new string constant into the array so that it can be
        // looked up by the vm's runtime to fetch the value before it is defined and not produce a "undefined
        // variable" error.
        // arg = identifierConstant(&name); // NOTE: Not doing this right now.
    }

    // If there is an equal '=' sign after the variable, that means this is a setter for that variable.
    // Otherwise this must be for value access i.e. a getter.
    if (canAssign && match(TOKEN_EQUAL)) {
        expression();
        emitBytes(OP_SET_GLOBAL, arg);
    } else {
        // emit the instructions get global to tell the vm that we want to retrieve the value of this global variable.
        emitBytes(OP_GET_GLOBAL, arg);
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
    // When we get here, the name of the variable is stored in the constants array. The index of which is this
    // 'global' The value it stores inside of it is also pushed into the chunk already. So the OP_CONSTANT comes
    // before this OP_GLOBAL_DEFINE instruction (followed by the name of the variable) which is the value this var
    // represents. We do this so that when the VM encounters the OP_GLOBAL_DEFINE in it's 'loop', the OP_CONSTANT
    // is already pushed onto the stack(since it came before this OP_DEFINE instruction) and so the vm can
    // comfortably get the value by using pop() from the stack and it would correctly 'assign' this constant value
    // to this variable.
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

static void
statement()
{
    if (match(TOKEN_PRINT)) {
        printStatement();
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