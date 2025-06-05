/**********************************************
        CS415  Compilers  Project 2
**********************************************/

#include <stdio.h>
#include <stdlib.h>
#include "symtab.h"
#include <string.h>
#include <stdbool.h>

#define HASH_TABLE_SIZE 509

static SymTabEntry **symbolTable;

static int computeHash(const char *name) {
    unsigned long hashValue = 5381;
    int character;
    while ((character = *name++))
        hashValue = ((hashValue << 5) + hashValue) + character;
    return hashValue % HASH_TABLE_SIZE;
}

void MakeSymTab() {
    symbolTable = malloc(sizeof(SymTabEntry *) * HASH_TABLE_SIZE);
    for (int index = 0; index < HASH_TABLE_SIZE; index++) symbolTable[index] = NULL;
}

SymTabEntry *lookup(char *name) {
    int index = computeHash(name);
    int checkedSlots = 0;

    while (symbolTable[index] && checkedSlots < HASH_TABLE_SIZE) {
        if (strcmp(symbolTable[index]->name, name) == 0)
            return symbolTable[index];
        index = (index + 1) % HASH_TABLE_SIZE;
        checkedSlots++;
    }
    return NULL;
}

void insert(char *name, TypeExpression type, int offset, bool isArray) {
    int index = computeHash(name);
    int checkedSlots = 0;

    while (symbolTable[index] && checkedSlots < HASH_TABLE_SIZE) {
        index = (index + 1) % HASH_TABLE_SIZE;
        checkedSlots++;
    }

    if (checkedSlots == HASH_TABLE_SIZE) {
        printf("Error: Symbol table full, failed insertion of %s\n", name);
        return;
    }

    SymTabEntry *entry = malloc(sizeof(SymTabEntry));
    entry->name = malloc(strlen(name) + 1);
    strcpy(entry->name, name);
    entry->type = type;
    entry->offset = offset;
    entry->isArray = isArray;
    symbolTable[index] = entry;
}


