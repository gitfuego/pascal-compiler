/**********************************************
        CS415  Compilers  Project 2
**********************************************/

#ifndef SYMTAB_H
#define SYMTAB_H

#include <string.h>
#include <stdbool.h>
#include "attr.h"

typedef struct {
    char *name;
    TypeExpression type;
    int offset;
    bool isArray;
} SymTabEntry;

void MakeSymTab();
SymTabEntry *lookup(char *name);
void insert(char *name, TypeExpression type, int offset, bool isArray);

#endif

