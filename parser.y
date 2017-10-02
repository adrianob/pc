/*
  Coloque aqui o identificador do grupo e dos seus membros

  - Leonardo Hahn
  - Adriano Benin
*/
%{
#include "main.h"
#include "cc_dict.h"
%}

%union {
    comp_dict_item_t *valor_lexico;
}

%define parse.error verbose
%start programa

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
%left TK_OC_GE
%left TK_OC_LE
%left '+' '-'
%left '*' '/'
%left '!'

%nonassoc "end_list_expressions"
%nonassoc ','

%%
programa:
        %empty | programa decl_global | programa decl_tipos | programa decl_func;

decl_tipos:
        TK_PR_CLASS TK_IDENTIFICADOR '[' lista_campos ']' ';';

lista_campos:
        campo | lista_campos ':' campo;

campo:
          TK_PR_PROTECTED tipo_primitivo TK_IDENTIFICADOR
        | TK_PR_PRIVATE tipo_primitivo TK_IDENTIFICADOR
        | TK_PR_PUBLIC tipo_primitivo TK_IDENTIFICADOR
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

decl_func: cabecalho bloco_comandos;

cabecalho:
                       tipo_primitivo TK_IDENTIFICADOR lista_entrada
        | TK_PR_STATIC tipo_primitivo TK_IDENTIFICADOR lista_entrada
        |              TK_IDENTIFICADOR TK_IDENTIFICADOR lista_entrada
        | TK_PR_STATIC TK_IDENTIFICADOR TK_IDENTIFICADOR lista_entrada
        ;

lista_entrada:
         '(' ')'
        |'(' parametros_entrada ')'
        ;

parametros_entrada:
        parametro_entrada | parametros_entrada ',' parametro_entrada;

parametro_entrada:
                      tipo_primitivo TK_IDENTIFICADOR
        | TK_PR_CONST tipo_primitivo TK_IDENTIFICADOR
        |             TK_IDENTIFICADOR TK_IDENTIFICADOR
        | TK_PR_CONST TK_IDENTIFICADOR TK_IDENTIFICADOR
        ;

bloco_comandos:
          '{' seq_comandos '}'
        | '{' '}'
        ;

seq_comandos:
          comando ';'
        | comando_case
        | seq_comandos comando ';'
        | seq_comandos comando_case
        ;

comando_case:
        TK_PR_CASE TK_LIT_INT ':';

comando_sem_entrada_saida:
          comando_decl_var
        | comando_decl_var_init
        | bloco_comandos
        | chamada_func
        | TK_PR_CONTINUE
        | TK_PR_BREAK
        | TK_PR_RETURN expressao
        | comando_atribuicao
        | comando_shift
        | comando_controle_fluxo
        ;

comando:
          comando_sem_entrada_saida
        | comando_entrada_saida
        ;

comando_decl_var:
                                   comando_decl_var_2
        | TK_PR_STATIC             comando_decl_var_2
        |              TK_PR_CONST comando_decl_var_2
        | TK_PR_STATIC TK_PR_CONST comando_decl_var_2
        ;

comando_decl_var_2:
          tipo_primitivo TK_IDENTIFICADOR
        | TK_IDENTIFICADOR TK_IDENTIFICADOR
        ;

comando_decl_var_init:
                                   tipo_primitivo TK_IDENTIFICADOR TK_OC_LE TK_IDENTIFICADOR
        |                          tipo_primitivo TK_IDENTIFICADOR TK_OC_LE token_lit
        | TK_PR_STATIC             tipo_primitivo TK_IDENTIFICADOR TK_OC_LE TK_IDENTIFICADOR
        | TK_PR_STATIC             tipo_primitivo TK_IDENTIFICADOR TK_OC_LE token_lit
        |              TK_PR_CONST tipo_primitivo TK_IDENTIFICADOR TK_OC_LE TK_IDENTIFICADOR
        |              TK_PR_CONST tipo_primitivo TK_IDENTIFICADOR TK_OC_LE token_lit
        | TK_PR_STATIC TK_PR_CONST tipo_primitivo TK_IDENTIFICADOR TK_OC_LE TK_IDENTIFICADOR
        | TK_PR_STATIC TK_PR_CONST tipo_primitivo TK_IDENTIFICADOR TK_OC_LE token_lit
        ;

token_lit:
          lit_numerico
        | TK_LIT_FALSE
        | TK_LIT_TRUE
        | TK_LIT_CHAR
        | TK_LIT_STRING
        ;

lit_numerico: TK_LIT_INT | TK_LIT_FLOAT;

comando_controle_fluxo:
          comando_if
        | comando_for
        | comando_while
        | comando_do_while
        | comando_foreach
        | comando_switch_case
        ;

comando_if:
          TK_PR_IF '(' expressao ')' TK_PR_THEN bloco_comandos
        | TK_PR_IF '(' expressao ')' TK_PR_THEN bloco_comandos TK_PR_ELSE bloco_comandos
        ;

comando_foreach:
        TK_PR_FOREACH '(' TK_IDENTIFICADOR ':' lista_expressoes ')' bloco_comandos;

comando_for:
        TK_PR_FOR '(' lista_comandos ':' expressao ':' lista_comandos ')' bloco_comandos;

lista_comandos:
          comando_sem_entrada_saida
        | lista_comandos ',' comando_sem_entrada_saida
        ;

comando_while:
        TK_PR_WHILE '(' expressao ')' TK_PR_DO bloco_comandos;

comando_do_while:
        TK_PR_DO bloco_comandos TK_PR_WHILE '(' expressao ')';

comando_switch_case:
          TK_PR_SWITCH '(' expressao ')' bloco_comandos;

comando_shift:
          TK_IDENTIFICADOR TK_OC_SL TK_LIT_INT
        | TK_IDENTIFICADOR TK_OC_SR TK_LIT_INT
        ;

comando_entrada_saida:
          TK_PR_INPUT expressao
        | TK_PR_OUTPUT lista_expressoes      %prec "end_list_expressions"
        ;

chamada_func:
          TK_IDENTIFICADOR '(' lista_expressoes ')'
        | TK_IDENTIFICADOR '(' ')'
        ;

lista_expressoes:
          expressao
        | lista_expressoes ',' expressao
        ;

comando_atribuicao:
          TK_IDENTIFICADOR '=' expressao
        | TK_IDENTIFICADOR '[' expressao ']' '=' expressao
        | TK_IDENTIFICADOR '$' TK_IDENTIFICADOR '=' expressao
        ;

expressao:
          expressao_arit
        | expressao_logica
        | TK_LIT_CHAR
        | TK_LIT_STRING
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
        | '(' expressao_arit ')'
        ;

expressao_logica:
          expressao_arit operator_relacional expressao_arit
        | expressao_logica TK_OC_AND expressao_logica1
        | expressao_logica1
        ;

expressao_logica1:
          expressao_logica1 TK_OC_OR expressao_logica2
        | expressao_logica2
        ;

expressao_logica2:
          expressao_logica2 '!' expressao_logica_operando
        | expressao_logica_operando
        ;

expressao_logica_operando:
          TK_LIT_FALSE
        | TK_LIT_TRUE
        | '(' expressao_logica ')'
       ;

operator_relacional:
       TK_OC_LE | TK_OC_GE | TK_OC_EQ | TK_OC_NE;
