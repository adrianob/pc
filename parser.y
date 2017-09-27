/*
  Coloque aqui o identificador do grupo e dos seus membros

  - Leonardo Hahn
  - Adriano Benin
*/
%{
#include "main.h"
%}

%define parse.error verbose
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
        programa decl_global | programa decl_tipos | programa decl_func | {}
        ;

decl_tipos:
        TK_PR_CLASS TK_IDENTIFICADOR '[' lista_campos ']' ';'
        ;

lista_campos:
        campo | lista_campos ':' campo
        ;

campo:
          TK_PR_PROTECTED tipo_primitivo TK_IDENTIFICADOR
        | TK_PR_PRIVATE tipo_primitivo TK_IDENTIFICADOR
        | TK_PR_PUBLIC tipo_primitivo TK_IDENTIFICADOR
        | TK_PR_PROTECTED TK_IDENTIFICADOR TK_IDENTIFICADOR
        | TK_PR_PRIVATE TK_IDENTIFICADOR TK_IDENTIFICADOR
        | TK_PR_PUBLIC TK_IDENTIFICADOR TK_IDENTIFICADOR
        | TK_PR_PUBLIC tipo_primitivo TK_IDENTIFICADOR '[' TK_LIT_INT ']'
        | TK_PR_PRIVATE tipo_primitivo TK_IDENTIFICADOR '[' TK_LIT_INT ']'
        | TK_PR_PROTECTED tipo_primitivo TK_IDENTIFICADOR '[' TK_LIT_INT ']'
        ;

tipo_primitivo:
        TK_PR_INT | TK_PR_FLOAT | TK_PR_BOOL | TK_PR_CHAR | TK_PR_STRING
        ;

decl_global:
          decl_global_non_static
        | TK_PR_STATIC decl_global_non_static
        ;

decl_global_non_static:
          tipo_primitivo TK_IDENTIFICADOR ';'
        | tipo_primitivo TK_IDENTIFICADOR '[' TK_LIT_INT ']' ';'
        | TK_IDENTIFICADOR TK_IDENTIFICADOR ';'
        ;

decl_func:
        cabecalho bloco_comandos;

cabecalho:
                      tipo_primitivo TK_IDENTIFICADOR lista_entrada
       | TK_PR_STATIC tipo_primitivo TK_IDENTIFICADOR lista_entrada
       |              TK_IDENTIFICADOR TK_IDENTIFICADOR lista_entrada
       | TK_PR_STATIC TK_IDENTIFICADOR TK_IDENTIFICADOR lista_entrada
       ;

lista_entrada: '(' parametros_entrada ')';

parametros_entrada:
        parametro_entrada | parametros_entrada ',' parametro_entrada | {};

parametro_entrada:
                      tipo_primitivo TK_IDENTIFICADOR
        | TK_PR_CONST tipo_primitivo TK_IDENTIFICADOR
        |             TK_IDENTIFICADOR TK_IDENTIFICADOR
        | TK_PR_CONST TK_IDENTIFICADOR TK_IDENTIFICADOR
        ;

/* FIXME: Bloco de comandos tem ponto e virgula? */
bloco_comandos:
          '{' seq_comandos '}'
        | '{' '}'
        ;

seq_comandos:
        comando ';' | seq_comandos comando ';';

/* @TODO adicionar outros comandos */
comando:
          comando_decl_var
        | comando_decl_var_init
        | bloco_comandos
        | comando_entrada_saida
        | chamada_func
        | TK_PR_CONTINUE
        | TK_PR_BREAK
        | TK_PR_RETURN
        | TK_PR_CASE TK_LIT_INT ':'
        /* | comando_atribuicao */
        ;
/*         | comando_entrada_saida */
/*         | comando_shift */
/*         | comando_case */
/*         | comando_controle_fluxo */
/*         ; */
/*  */
/* lista_comandos: */
/*           comando */
/*         | lista_comandos ',' comando */
/*         ; */
/*  */

comando_decl_var:
          comando_decl_var_2
        | TK_PR_STATIC comando_decl_var_2
        | TK_PR_CONST comando_decl_var_2
        | TK_PR_STATIC TK_PR_CONST comando_decl_var_2
        ;

comando_decl_var_2:
          tipo_primitivo TK_IDENTIFICADOR
        | TK_IDENTIFICADOR TK_IDENTIFICADOR
        ;

comando_decl_var_init:
          comando_decl_var TK_OC_LE TK_IDENTIFICADOR
        | comando_decl_var TK_OC_LE token_lit
        ;

token_lit:
          lit_numerico
        | TK_LIT_FALSE
        | TK_LIT_TRUE
        | TK_LIT_CHAR
        | TK_LIT_STRING
        ;

lit_numerico:
        TK_LIT_INT | TK_LIT_FLOAT;


/* comando_controle_fluxo: */
/*           comando_if */
/*         | comando_for */
/*         | comando_while */
/*         | comando_do_while */
/*         | comando_foreach */
/*         ; */
/*  */
/* comando_if: */
/*           TK_PR_IF '(' expressao ')' TK_PR_THEN bloco_comandos */
/*         | TK_PR_IF '(' expressao ')' TK_PR_THEN bloco_comandos TK_PR_ELSE bloco_comandos */
/*         ; */
/*  */
/* comando_foreach: */
/*         TK_PR_FOREACH '(' TK_IDENTIFICADOR ':' lista_expressoes ')' bloco_comandos; */
/*  */
/* comando_for: */
/*         TK_PR_FOR '(' lista_comandos ':' expressao ':' lista_comandos ')' bloco_comandos; */
/*  */
/* comando_while: */
/*         TK_PR_WHILE '(' expressao ')' TK_PR_DO bloco_comandos; */
/*  */
/* comando_do_while: */
/*         TK_PR_DO bloco_comandos TK_PR_WHILE '(' expressao ')' */
/*  */
/* comando_shift: */
/*         TK_IDENTIFICADOR TK_OC_SL TK_LIT_INT | */
/*         TK_IDENTIFICADOR TK_OC_SR TK_LIT_INT */
/*         ; */

/* Sem ponto e virgula? Perguntar pro professor */
comando_entrada_saida:
          TK_PR_INPUT expressao
        | TK_PR_OUTPUT lista_expressoes
        ;

chamada_func:
          TK_IDENTIFICADOR '(' lista_expressoes ')';
        | TK_IDENTIFICADOR '(' ')'

lista_expressoes:
          expressao
        | lista_expressoes ',' expressao
        ;

/* @TODO sintaxe ident$campo[i] deve ser utilizada? */
/* comando_atribuicao: */
/*         TK_IDENTIFICADOR '=' expressao | */
/*         TK_IDENTIFICADOR '[' expressao ']' '=' expressao */
/*         TK_IDENTIFICADOR '$' TK_IDENTIFICADOR '=' expressao */
/*         ; */

/* @TODO definir expressao corretamente */
expressao:
          expressao_arit
        /* | TK_IDENTIFICADOR */
        /* | TK_IDENTIFICADOR '[' expressao ']' */
        /* | token_lit */
        ;

expressao_arit:
          expressao_arit '+' expressao_arit_term1
        | expressao_arit_term1
        ;

expressao_arit_term1:
          expressao_arit_term1 '-' expressao_arit_term2
        | expressao_arit_term2
        ;

expressao_arit_term2:
          expressao_arit_term2 '*' expressao_arit_term3
        | expressao_arit_term3
        ;

expressao_arit_term3:
          expressao_arit_term3 '/' expressao_arit_operando
        | expressao_arit_operando
        ;

expressao_arit_operando:
          TK_IDENTIFICADOR
        | TK_IDENTIFICADOR '[' expressao ']'
        | lit_numerico
        | chamada_func
        ;

/* @TODO testar */
/* expressao_logica: */
/*           expressao operador_logico expressao */
/*         ; */

/* operator_relacional: */
/*         TK_OC_LE | TK_OC_GE | TK_OC_EQ | TK_OC_NE; */

/* operador_logico: */
/*         TK_OC_AND | TK_OC_OR | '!' | operator_relacional; */

/* @TODO verificar se operadores logicos e aritmeticos estao corretos*/
/* operador_arit: */
/*         '+' | '-' | '*' | '/'; */
