#include "common.h"
#include "chunk.h"
#include "debug.h"
#include "vm.h"
#include <string.h>
#include <errno.h>

#define DO_TESTS
#ifdef DO_TESTS
#include "tests/test.h"
#endif

static void
repl()
{
    char line[1024];

    for (;;) {
        printf("> ");
        // read a line of text max characters 1024 until a '\n' is encountered.
        if (!fgets(line, sizeof(line), stdin)) {
            printf("\n");
            break;
        }

        if (strcmp(line, "exit") == 0) {
            break;
        }

        interpret(line);
    }
}

static char *
readFile(const char *path)
{
    FILE *fp = fopen(path, "rb");
    if (fp == NULL) {
        const char *errMsg = strerror(errno);
        fprintf(stderr, "Could not open the file \"%s\". Error: %s\n", path, errMsg);
        exit(74);
    }

    fseek(fp, 0L, SEEK_END);
    size_t fileSize = ftell(fp);
    rewind(fp);

    char *buffer = (char *)malloc(fileSize + 1);
    size_t bytesRead = fread(buffer, sizeof(char), fileSize, fp);
    if (bytesRead < fileSize) {
        fprintf(stderr, "Could not open the file \"%s\".\n", path);
        exit(74);
    }
    buffer[bytesRead] = '\0';

    fclose(fp);
    return buffer;
}

static void
runFile(const char *path)
{
    char *source = readFile(path);
    InterpretResult result = interpret(source);
    free(source);

    if (result == INTERPRET_COMPILE_ERROR) exit(65);
    if (result == INTERPRET_RUNTIME_ERROR) exit(70);
}

int main(int argc, const char **argv)
{
    initVM();

    if (argc == 1) {
        repl();
    } else if (argc == 2) {
        runFile(argv[1]);
    } else {
        fprintf(stderr, "Usage: clox [path]\n");
        exit(64);
    }

    freeVM();
    return 0;
}
