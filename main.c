#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

#include "utils.h"
#include "lexer.h"
#include "parser.h"
#include "ad.h"
#include "at.h"
#include "vm.h"

int main()
{
    //char *file=loadFile("tests\\testlex.c");
    //char *file=loadFile("tests\\testparser.c");
    //char *file=loadFile("tests\\testad.c");
    /*char *file=loadFile("tests\\testat.c");
    Token *tokens=tokenize(file);
    //showTokens(tokens);
    parse(tokens);
    printf("Sintaxa programului este corecta!\n");

    free(file);*/
    
    /*pushDomain();
    vmInit();
    Instr *testCode=genTestProgramDouble();
    run(testCode);
    dropDomain();*/

    char *file=loadFile("tests\\testgc.c");
    Token *tokens=tokenize(file);
    parse(tokens);
    Symbol *symMain = findSymbolInDomain(symTable, "main");
    if(!symMain) err("missing main function");
    
    Instr *entryCode = NULL;
    addInstr(&entryCode, OP_CALL)->arg.instr = symMain->fn.instr;
    addInstr(&entryCode, OP_HALT);
    
    run(entryCode);
    dropDomain();
    return 0;

}