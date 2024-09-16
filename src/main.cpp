#include <cstdio>
// #include <test_class/test.h>

int main(int argc, char **argv)
{
    printf("Hello, World!");
    printf("argCount: %d\n", argc);
#ifdef _DEBUG
    printf("We are in Debug Mode!\n");
#endif
    return 0;
}
