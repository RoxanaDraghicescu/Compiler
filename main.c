#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

#include "utils.h"
#include "lexer.h"
#include "parser.h"

int main()
{
    //char *file=loadFile("tests\\testlex.c");
    char *file=loadFile("tests\\testparser.c");
    Token *tokens=tokenize(file);
    //showTokens(tokens);
    parse(tokens);
    printf("Sintaxa programului este corecta!\n");

    free(file);

    return 0;

}