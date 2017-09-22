/*
  Coloque aqui o identificador do grupo e dos seus membros

  - Leonardo Hahn
  - Adriano Benin
*/
%{
#include "main.h"
%}

/* Declaração dos tokens da linguagem */
%token TK_PR_INT
%token TK_PR_FLOAT
%token TK_PR_BOOL
%token TK_PR_CHAR
%token TK_PR_STRING
%token TK_PR_IF
%token TK_PR_THEN
%token TK_PR_ELSE
%token TK_PR_WHILE
%token TK_PR_DO
%token TK_PR_INPUT
%token TK_PR_OUTPUT
%token TK_PR_RETURN
%token TK_PR_CONST
%token TK_PR_STATIC
%token TK_PR_FOREACH
%token TK_PR_FOR
%token TK_PR_SWITCH
%token TK_PR_CASE
%token TK_PR_BREAK
%token TK_PR_CONTINUE
%token TK_PR_CLASS
%token TK_PR_PRIVATE
%token TK_PR_PUBLIC
%token TK_PR_PROTECTED
%token TK_OC_LE
%token TK_OC_GE
%token TK_OC_EQ
%token TK_OC_NE
%token TK_OC_AND
%token TK_OC_OR
%token TK_OC_SL
%token TK_OC_SR
%token TK_LIT_INT
%token TK_LIT_FLOAT
%token TK_LIT_FALSE
%token TK_LIT_TRUE
%token TK_LIT_CHAR
%token TK_LIT_STRING
%token TK_IDENTIFICADOR
%token TOKEN_ERRO


%right '='
%left '<'
%left '>'
%left '+' '-'
%left '*' '/'
%left '!'


%%
/* Regras (e ações) da gramática */

programa:
        programa decl_global | programa decl_func | decl_tipos | {};

decl_tipos:
        TK_PR_CLASS TK_IDENTIFICADOR '[' lista_campos ']' ';'

lista_campos:
        campo | campo ':' lista_campos

campo:
        TK_PR_PROTECTED tipo TK_IDENTIFICADOR |
        TK_PR_PRIVATE tipo TK_IDENTIFICADOR |
        TK_PR_PUBLIC tipo TK_IDENTIFICADOR

tipo:
        TK_PR_INT | TK_PR_FLOAT | TK_PR_BOOL | TK_PR_CHAR | TK_PR_STRING

/* @TODO vetores */
decl_global:
        tipo TK_IDENTIFICADOR ';' | TK_PR_STATIC tipo TK_IDENTIFICADOR ';'

decl_func:
        cabecalho corpo_func

cabecalho:
        tipo TK_IDENTIFICADOR lista_entrada

lista_entrada:
        '(' parametros_entrada ')'

parametros_entrada:
        parametro_entrada | parametro_entrada ',' parametros_entrada

parametro_entrada:
        tipo TK_IDENTIFICADOR | TK_PR_CONST tipo TK_IDENTIFICADOR

/* @TODO comandos */
corpo_func:
        '{' '}'


%%
