%{
#include <stdio.h>
#include "attr.h"
#include "instrutil.h"
int yylex();
void yyerror(char * s);
#include "symtab.h"

FILE *outfile;
char *CommentBuffer;
 
%}

%union {tokentype token;
        regInfo targetReg;
        labelInfo label;
        }

%token PROG PERIOD VAR 
%token INT BOOL ARRAY RANGE OF WRITELN THEN IF 
%token BEG END ASG DO FOR
%token EQ NEQ LT LEQ 
%token AND OR XOR NOT TRUE FALSE 
%token ELSE
%token WHILE
%token <token> ID ICONST 

%type <targetReg> exp
%type <targetReg> vardcl
%type <targetReg> idlist
%type <targetReg> type
%type <targetReg> stype
%type <label> ifhead
%type <label> forprefix
%type <targetReg> condexp
%type <targetReg> lvalue
%type <targetReg> rvalue
%type <targetReg> constant
%type <targetReg> integer_constant
%type <targetReg> boolean_constant


%start program

%nonassoc EQ NEQ LT LEQ 
%left '+' '-' 
%left '*' 
%left OR
%left XOR
%left AND
%right NOT
%nonassoc THEN
%nonassoc ELSE

%%
program : {emitComment("Assign STATIC_AREA_ADDRESS to register \"r0\"");
           emit(NOLABEL, LOADI, STATIC_AREA_ADDRESS, 0, EMPTY); } 
           PROG ID ';' block PERIOD { }
	;

block	: variables cmpdstmt { }
	;

variables: /* empty */
	| VAR vardcls { }
	;

vardcls	: vardcls vardcl ';' { }
	| vardcl ';' { }
	| error ';' { yyerror("***Error: illegal variable declaration\n");}  
	;

vardcl	: idlist ':' type { 
        int offset = 0;
                char *id_name;
                Node *ptr = $1.head;

                while (ptr) {
                    id_name = ptr->name;
                    if ($3.isArray) {
                        offset = NextOffset($3.size);
                    }
                    else {
                        offset = NextOffset(1);
                    }
                    insert(id_name, $3.type, offset, $3.isArray);
                    ptr = ptr->next;
                }
                kill($1.head);
 }
	;

idlist	: idlist ',' ID {
        Node *new_node = malloc(sizeof(Node));
                if (!new_node) printf("\nERROR: Malloc error\n");
                new_node->name = $3.str;
                new_node->next = NULL;

                $$.head = push($$.head, new_node);
 }
	| ID		{
                Node *new_node = malloc(sizeof(Node));
                new_node->name = $1.str;
                new_node->next = NULL;
                $$.head = push($$.head, new_node);
         }
	;

type: ARRAY '[' ICONST RANGE ICONST ']' OF stype
      {
        $$.type = $8.type;
        $$.isArray = 1;
        $$.size = $5.num - $3.num + 1;
      }
    | stype
      {
        $$.type = $1.type;
        $$.isArray = $1.isArray;
      }
    ;


stype:      INT
            {
                $$.type = TYPE_INT;
                $$.isArray = 0;
            }
            | BOOL 
            { 
                $$.type = TYPE_BOOL; 
                $$.isArray = 0;
            }
            ;

stmtlist : stmtlist ';' stmt { }
	| stmt { }
    | error { yyerror("***Error: illegal statement \n");}
	;

stmt    : ifstmt { }
	| wstmt { }
	| fstmt { }
	| astmt { }
	| writestmt { }
	| cmpdstmt { }
	;

wstmt
  : WHILE {
      $<label>$ = (labelInfo){NextLabel(), NextLabel(), NextLabel()};
      emit($<label>$.label1, NOP, EMPTY, EMPTY, EMPTY);
      emitComment("Control code for \"WHILE DO\"");
    }
    condexp {
      emit(NOLABEL, CBR, $3.targetRegister, $<label>2.label2, $<label>2.label3);
      emit($<label>2.label2, NOP, EMPTY, EMPTY, EMPTY);
      emitComment("Body of \"WHILE\" construct starts here");
    }
    DO stmt {
      emit(NOLABEL, BR, $<label>2.label1, EMPTY, EMPTY);
      emit($<label>2.label3, NOP, EMPTY, EMPTY, EMPTY);
    }
  ;



/* NEEDS WORK */
forprefix
  : FOR ID ASG ICONST ',' ICONST DO {
      $$.label1 = NextLabel();
      $$.label2 = NextLabel();
      $$.label3 = NextLabel();

      SymTabEntry *entry = lookup($2.str);
      int reg_init = NextRegister();
      emit(NOLABEL, LOADI, $4.num, reg_init, EMPTY);

      int reg_addr = NextRegister();
      emit(NOLABEL, LOADI, entry->offset, reg_addr, EMPTY);
      emit(NOLABEL, ADD, 0, reg_addr, reg_addr);
      emit(NOLABEL, STORE, reg_init, reg_addr, EMPTY);

      emit($$.label1, NOP, EMPTY, EMPTY, EMPTY);
      int reg_curr = NextRegister();
      emit(NOLABEL, LOAD, reg_addr, reg_curr, EMPTY);

      int reg_bound = NextRegister();
      emit(NOLABEL, LOADI, $6.num, reg_bound, EMPTY);

      int reg_cond = NextRegister();
      emit(NOLABEL, CMPLE, reg_curr, reg_bound, reg_cond);

      emit(NOLABEL, CBR, reg_cond, $$.label2, $$.label3);
      emit($$.label2, NOP, EMPTY, EMPTY, EMPTY);

      $$.targetRegister = reg_addr;
    }
  ;

fstmt
  : forprefix stmt {
      int reg_curr2 = NextRegister();
      emit(NOLABEL, LOAD, $1.targetRegister, reg_curr2, EMPTY);

      int reg_one = NextRegister();
      emit(NOLABEL, LOADI, 1, reg_one, EMPTY);

      int reg_inc = NextRegister();
      emit(NOLABEL, ADD, reg_curr2, reg_one, reg_inc);
      emit(NOLABEL, STORE, reg_inc, $1.targetRegister, EMPTY);

      emit(NOLABEL, BR, $1.label1, EMPTY, EMPTY);
      emit($1.label3, NOP, EMPTY, EMPTY, EMPTY);
    }
  ;





ifstmt :  ifhead THEN stmt { 
        emitComment("This is the \"false\" branch");
        emit($<label>1.label2, NOP, EMPTY, EMPTY, EMPTY);
    } 
    | ifhead THEN stmt ELSE {
        emit(NOLABEL, BR, $<label>1.label3, EMPTY, EMPTY);
        emit($<label>1.label2, NOP, EMPTY, EMPTY, EMPTY);
        emitComment("This is the \"false\" branch");
    }
    stmt { 
        emit($<label>1.label3, NOP, EMPTY, EMPTY, EMPTY);
    }
	;


ifhead: IF condexp
  {
    $$.targetRegister = $2.targetRegister;

    int label1 = NextLabel();
    int label2 = NextLabel();
    int label3 = NextLabel();

    $<label>$.label1 = label1;
    $<label>$.label2 = label2;
    $<label>$.label3 = label3;

    emit(NOLABEL, CBR, $$.targetRegister, label1, label2);
    emit(label1, NOP, EMPTY, EMPTY, EMPTY);
    emitComment("This is the \"true\" branch");
  }
;




cmpdstmt: BEG stmtlist END { }
	;

writestmt: WRITELN '(' exp ')' {
  sprintf(CommentBuffer, "Code for WRITELN");
  emitComment(CommentBuffer);
  emit(NOLABEL, STOREAI, $3.targetRegister, 0, -4);
  emit(NOLABEL, OUTPUT, 1020, EMPTY, EMPTY);
}




astmt : lvalue ASG exp {
            emit(NOLABEL, STORE, $3.targetRegister, $1.targetRegister, EMPTY);
        }
	;

exp	: rvalue {}
        | exp '+' exp		{ int newReg = NextRegister();
                                  $$.targetRegister = newReg;
                                  emit(NOLABEL, 
                                       ADD, 
                                       $1.targetRegister, 
                                       $3.targetRegister, 
                                       newReg);
                                }

        | exp '-' exp		{ int newReg = NextRegister(); 
                                  $$.targetRegister = newReg;
                                  emit(NOLABEL, 
                                       SUB, 
                                       $1.targetRegister, 
                                       $3.targetRegister, 
                                       newReg);
                                }

	| exp '*' exp		{ int newReg = NextRegister(); 
                                  $$.targetRegister = newReg;
                                  emit(NOLABEL, 
                                       MULT, 
                                       $1.targetRegister, 
                                       $3.targetRegister, 
                                       newReg);
                                }
        | exp AND exp
                                {
                                int newReg = NextRegister();
                                $$.targetRegister = newReg;
                                emit(NOLABEL, L_AND, $1.targetRegister, $3.targetRegister, newReg);
                                } 
        | exp OR exp 
                                {  
                                int newReg = NextRegister();
                                $$.targetRegister = newReg;
                                emit(NOLABEL, L_OR, $1.targetRegister, $3.targetRegister, newReg);
                                }
        | exp XOR exp 
                                {  
                                int newReg = NextRegister();
                                $$.targetRegister = newReg;
                                emit(NOLABEL, L_XOR, $1.targetRegister, $3.targetRegister, newReg);
                                }
        | NOT exp { 
            int newReg1 = NextRegister();
            int newReg2 = NextRegister();
            emit(NOLABEL, LOADI, 1, newReg1, EMPTY);
            emit(NOLABEL, SUB, newReg1, $2.targetRegister, newReg2);
            $$.targetRegister = newReg2;
            $$.type = $2.type;            
        }
        | '(' exp ')' { $$.targetRegister = $2.targetRegister;}
	| constant {}
	| error { yyerror("***Error: illegal expression\n");}  
	;

condexp	: exp NEQ exp		{ 
                int newReg = NextRegister();
                $$.targetRegister = newReg;
                emit(NOLABEL, CMPNE, $1.targetRegister, $3.targetRegister, newReg);
}
	| exp EQ exp	{ 
                int newReg = NextRegister();
                $$.targetRegister = newReg;
                emit(NOLABEL, CMPEQ, $1.targetRegister, $3.targetRegister, newReg);
        }
	| exp LT exp	{ 
                int newReg = NextRegister();
                $$.targetRegister = newReg;
                emit(NOLABEL, CMPLT, $1.targetRegister, $3.targetRegister, newReg);
        }
	| exp LEQ exp	{
                int newReg = NextRegister();
                $$.targetRegister = newReg;
                emit(NOLABEL, CMPLE, $1.targetRegister, $3.targetRegister, newReg);
         }
    | ID { 
                int newReg1 = NextRegister();
                int newReg2 = NextRegister();
                int offset = 0;
                SymTabEntry *entry = lookup($1.str);

                if (entry) {
                    $$.targetRegister = newReg2;
                    $$.type = entry->type;
                    offset = entry->offset;
                    emit(NOLABEL, LOADI, offset, newReg1, EMPTY);
                    emit(NOLABEL, ADD, 0, newReg1, newReg2);
                }
                else {
                    printf("\n*** ERROR ***: Variable %s not declared.\n", $1.str);
                }
        }
    | boolean_constant {}
	| error { yyerror("***Error: illegal conditional expression\n");}  
        ;

lvalue	: ID { 
            int newReg1 = NextRegister();
            int newReg2 = NextRegister();
            int offset = 0;
            SymTabEntry *entry = lookup($1.str);
            $$.targetRegister = newReg2;
            $$.type = entry->type;
            offset = entry->offset;
            emit(NOLABEL, LOADI, offset, newReg1, EMPTY);
            emit(NOLABEL, ADD, 0, newReg1, newReg2);        
}
        |  ID '[' exp ']' { 
                int targetRegister = NextRegister();
                int newReg1 = NextRegister();
                int newReg2 = NextRegister();
                int newReg3 = NextRegister();
                int newReg4 = NextRegister();
                int baseAddr = 0;
                SymTabEntry *entry = lookup($1.str);

                baseAddr = entry->offset;
                $$.targetRegister = targetRegister;
                $$.type = entry->type;

                emit(NOLABEL, LOADI, 4, newReg1, EMPTY);
                emit(NOLABEL, MULT,  $3.targetRegister, newReg1, newReg2);
                emit(NOLABEL, LOADI, baseAddr, newReg3, EMPTY);
                emit(NOLABEL, ADD,  newReg3, newReg2, newReg4);
                emit(NOLABEL, ADD,  0, newReg4, targetRegister);
        }
        ;

rvalue : ID { 
    int newReg1 = NextRegister(); // to hold the offset
    int newReg2 = NextRegister(); // to hold the address
    int newReg3 = NextRegister(); // to hold the loaded value
    int offset = 0;
    SymTabEntry *entry = lookup($1.str);

    $$.type = entry->type;
    offset = entry->offset;

    emit(NOLABEL, LOADI, offset, newReg1, EMPTY);
    emit(NOLABEL, ADD, 0, newReg1, newReg2);

    emit(NOLABEL, LOAD, newReg2, newReg3, EMPTY);
    $$.targetRegister = newReg3;
}
        |  ID '[' exp ']' { 
                int targetRegister = NextRegister();
                int newReg1 = NextRegister();
                int newReg2 = NextRegister();
                int newReg3 = NextRegister();
                int newReg4 = NextRegister();
                int baseAddress= 0;
                SymTabEntry *entry = lookup($1.str);

                baseAddress = entry->offset;
                $$.targetRegister = targetRegister;
                $$.type = entry->type;

                emit(NOLABEL, LOADI, 4, newReg1, EMPTY);
                emit(NOLABEL, MULT,  $3.targetRegister, newReg1, newReg2);
                emit(NOLABEL, LOADI, baseAddress, newReg3, EMPTY); 
                emit(NOLABEL, ADD,  newReg3, newReg2, newReg4); 
                emit(NOLABEL, ADD,  0, newReg4, targetRegister);
                int valueReg = NextRegister();
                emit(NOLABEL, LOAD, targetRegister, valueReg, EMPTY);

                $$.targetRegister = valueReg;
                $$.type = entry->type;
                
        }
        ;

constant: integer_constant {}
        | boolean_constant {}
        ;

integer_constant: ICONST 
            {
                int newReg = NextRegister();

                $$.targetRegister = newReg;
                $$.type = TYPE_INT;
                emit(NOLABEL, LOADI, $1.num, newReg, EMPTY); 
            }
            ;

boolean_constant:   TRUE 
                    {
                        int newReg = NextRegister();

                        $$.targetRegister = newReg;
                        $$.type = TYPE_BOOL;
                        emit(NOLABEL, LOADI, 1, newReg, EMPTY);
                    }
                    | FALSE 
                    {
                        int newReg = NextRegister();

                        $$.targetRegister = newReg;
                        $$.type = TYPE_BOOL;
                        emit(NOLABEL, LOADI, 0, newReg, EMPTY);
                    }
                    ;
%%



void yyerror(char* s) {
        fprintf(stderr,"%s\n",s);
	fflush(stderr);
        }

int
main() {
  printf("\n          CS415 Project 2: Code Generator\n\n");
  
  outfile = fopen("iloc.out", "w");
  if (outfile == NULL) { 
    printf("ERROR: cannot open output file \"iloc.out\".\n");
    return -1;
  }

  CommentBuffer = (char *) malloc(500);  

  MakeSymTab();

  printf("1\t");
  yyparse();
  printf("\n");

  fclose(outfile);
  
  return 1;
}




