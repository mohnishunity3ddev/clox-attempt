#include <stdio.h>
#include <stdlib.h>

#include "common.h"
#include "compiler.h"
#include "scanner.h"

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

typedef void (*ParseFn)();
typedef struct
{
    ParseFn prefix;
    ParseFn infix;
    Precedence precedence;
} ParseRule;

Parser parser;
Chunk *compilingChunk;

static void binary();
static void grouping();
static void number();
static void parsePrecedence(Precedence precedence);
static void unary();
static void literal();
static void string();

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
  [TOKEN_IDENTIFIER]    = {NULL,     NULL,   PREC_NONE},
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
static void parsePrecedence(Precedence precedence);

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

static void
consume(TokenType type, const char *message)
{
    if (parser.current.type == type) {
        advance();
        return;
    }

    errorAtCurrent(message);
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
binary()
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
    parsePrecedence((Precedence)rule->precedence + 1);

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
grouping()
{
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

static void
number()
{
    double value = strtod(parser.previous.start, NULL);
    emitConstant(NUMBER_VAL(value));
}

static void
parsePrecedence(Precedence precedence)
{
    advance();
    ParseFn prefixRule_previousToken = getRule(parser.previous.type)->prefix;
    if (prefixRule_previousToken == NULL)
    {
        error("Expect expression");
        return;
    }

    prefixRule_previousToken();
    Precedence currentTokenPrecedence = getRule(parser.current.type)->precedence;
    // NOTE: Let's take an example - 1-2*3, as the parser is going through reading the expression from left to
    // right, we should evaluate 2*3 before evaluating 1-2, since * has higher precedence than '-'.
    // that's what we check here - the currentToken Precedence if its larger than the precedence passed in
    // THEN we evaluate the right hand operand for the infix operator.
    // NOTE: If it were 1*2-3, we see - here which has lower precedence than the previous *, the currentToken(-)'s
    // precedence is lower than '*'s precedence, so we dont compile the right hand side of the '-' operator,
    // and evaulate 1*2 and not 2-3 before it.
    while (currentTokenPrecedence >= precedence)
    {
        advance();
        ParseFn infixRule = getRule(parser.previous.type)->infix;
        infixRule();
        currentTokenPrecedence = getRule(parser.current.type)->precedence;
    }
}

static void
unary()
{
    TokenType operatorType = parser.previous.type;

    // Compile the operand.
    // look for the Unary operator and all operators of higher precedence than unary in the expression.
    parsePrecedence(PREC_UNARY);

    // Emit the operator instruction
    switch(operatorType)
    {
        case TOKEN_NOT:   { emitByte(OP_NOT); }     return;
        case TOKEN_MINUS: { emitByte(OP_NEGATE); }  return;
        default: return;
    }
}

static void
literal()
{
    switch(parser.previous.type)
    {
        case TOKEN_FALSE: { emitByte(OP_FALSE); } break;
        case TOKEN_NIL:   { emitByte(OP_NIL);   } break;
        case TOKEN_TRUE:  { emitByte(OP_TRUE);  } break;
        default: return;
    }
}

static void string()
{
    ObjString *string = copyString(parser.previous.start + 1, parser.previous.length - 2);
    Value stringValue = OBJ_VAL((Obj *)string);
    emitConstant(stringValue);
}

static void
expression()
{
    // compile the expression only looking for literals that are given and literals that have higher precedence
    // than the given one.
    parsePrecedence(PREC_ASSIGNMENT);
}

bool
compile(const char *source, Chunk *chunk)
{
    initScanner(source);
    compilingChunk = chunk;

    parser.hadError  = false;
    parser.panicMode = false;

    advance();
    expression();

    // At this point, the current token should be the EOF Token.
    consume(TOKEN_EOF, "Expect end of expression!");
    endCompiler();
    return !parser.hadError;
}