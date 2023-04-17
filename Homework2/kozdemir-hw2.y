%{
#include "stdio.h"

void yyerror(const char *msg) { return; }
%}

%token tSTRING
%token tNUM
%token tPRINT
%token tGET
%token tSET
%token tFUNCTION
%token tRETURN
%token tIDENT
%token tIF
%token tEQUALITY
%token tGT
%token tLT
%token tGEQ
%token tLEQ
%token tADD
%token tSUB
%token tMUL
%token tDIV
%token tINC
%token tDEC

%start prog

%%

prog: '[' stmt_list ']'
    | '[' ']' 
;
stmt_list: stmt_list stmt
         | stmt 
;
stmt: set_stmt
    | inc_stmt
    | dec_stmt
    | if_stmt
    | prt_stmt
    | ret_stmt
    | expr
;
set_stmt: '[' tSET ',' tIDENT ',' expr ']'
;
if_stmt: '[' tIF ',' cond ',' then ']'
       | '[' tIF ',' cond ',' then else ']'
;
then: '[' stmt_list ']'
    | '[' ']'
;
else: '[' stmt_list ']'
    | '[' ']'
;
prt_stmt: '[' tPRINT ',' expr ']'
;
inc_stmt: '[' tINC ',' tIDENT ']'
;
dec_stmt: '[' tDEC ',' tIDENT ']'
;
cond: '[' tLEQ ',' expr ',' expr ']'
    | '[' tGEQ ',' expr ',' expr ']'
    | '[' tLT ',' expr ',' expr ']'
    | '[' tGT ',' expr ',' expr ']'
    | '[' tEQUALITY ',' expr ',' expr ']'
;
expr_list: expr_list ',' expr
         | expr
;
expr: tNUM
    | tSTRING
    | get_expr
    | func_stmt
    | opr_app
    | cond
;
get_expr: '[' tGET ',' tIDENT ']'
        | '[' tGET ',' tIDENT ',' '[' expr_list ']' ']'
        | '[' tGET ',' tIDENT ',' '[' ']' ']'
;
func_stmt: '[' tFUNCTION ',' '[' param_list ']' ',' '[' stmt_list ']' ']'
         | '[' tFUNCTION ',' '[' ']' ',' '[' stmt_list ']' ']'
         | '[' tFUNCTION ',' '[' param_list ']' ',' '[' ']' ']'
         | '[' tFUNCTION ',' '[' ']' ',' '[' ']' ']'
;
param_list: param_list ',' param
          | param
;
param: tIDENT
;
opr_app: '[' opr ',' expr ',' expr ']'
;
opr: tADD
   | tSUB
   | tMUL
   | tDIV
;
ret_stmt: '[' tRETURN ']'
        | '[' tRETURN ',' expr ']'
;        

%%

int main() {
    if (yyparse()) {
        printf("ERROR\n");
        return 1;
    }
    else {
        printf("OK\n");
        return 0;
    }
}