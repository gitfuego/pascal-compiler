/**********************************************
			 CS415 Compilers Project 2
**********************************************/

#ifndef ATTR_H
#define ATTR_H

typedef union {int num; char *str;} tokentype;
typedef enum typeexpression {TYPE_INT=0, TYPE_BOOL, TYPE_ERROR} TypeExpression;

typedef struct _Node {
    char *name;
    struct _Node *next;
} Node;

typedef struct {
    TypeExpression type;
    int targetRegister;
    int isArray;
    int size;
    Node *head;
} regInfo;

typedef struct {
    int targetRegister;
    int label1;
    int label2;
    int label3;
} labelInfo;


Node *push(Node *head, Node *new_node);
void kill(Node *head);

#endif


