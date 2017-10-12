/*
  Coloque aqui o identificador do grupo e dos seus membros

  - Leonardo Hahn
  - Adriano Benin
*/
%{
#include "main.h"
#include "cc_dict.h"
#include "cc_ast.h"
#include "table_symbol.h"

AST_Program *g_program = NULL;
%}

%union {
    comp_dict_item_t *valor_lexico;
    AST_Function *ast_function;
    AST_CommandHeader *ast_command_header;
    AST_ExprHeader *ast_expr_header;
    int op;
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

%type<ast_function>       decl_func;
%type<ast_command_header> comando_if;
%type<ast_command_header> comando_shift;
%type<ast_command_header> comando_while;
%type<ast_command_header> comando_do_while;
%type<ast_command_header> comando_atribuicao;
%type<ast_command_header> comando_return;
%type<ast_command_header> bloco_comandos;
%type<ast_expr_header>    expressao;
%type<ast_expr_header>    expressao_arit;
%type<ast_expr_header>    expressao_arit_term1;
%type<ast_expr_header>    expressao_arit_term2;
%type<ast_expr_header>    expressao_arit_term3;
%type<ast_expr_header>    expressao_arit_operando;
%type<ast_expr_header>    chamada_func;
%type<ast_expr_header>    lista_expressoes;
%type<ast_expr_header>    lit_numerico;
%type<op>                 operator_relacional;

%type<ast_expr_header>    expressao_logica;
%type<ast_expr_header>    expressao_logica1;
%type<ast_expr_header>    expressao_logica2;
%type<ast_expr_header>    expressao_logica3;
%type<ast_expr_header>    expressao_logica4;
%type<ast_expr_header>    expressao_logica_operando;

%%
programa:
          %empty                {if (!g_program) g_program = ast_program_make();}
        | programa decl_global {if (!g_program) g_program = ast_program_make();}
        | programa decl_tipos {if (!g_program) g_program = ast_program_make();}
        | programa decl_func {
            if (!g_program) g_program = ast_program_make();

            if (!g_program->first_func) {
                g_program->first_func = $2;
            } else {
                AST_Function *search = g_program->first_func;
                while (search->next) {
                    search = search->next;
                }
                search->next = $2;
            }
        }
        ;

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

decl_func: cabecalho bloco_comandos {
               $$ = ast_function_make();
               $$->first_command = ((AST_Block*)$2)->first_command;
               // Free block, since inside function we use the pointer to the command
               // directly.
               ast_block_free((AST_Block*)$2);
          };

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
          '{' seq_comandos '}' {
              $$ = ast_block_make();
          }
        | '{' '}' {$$ = ast_block_make();}
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
        | comando_continue
        | comando_break
        | comando_return
        | comando_atribuicao
        | comando_shift
        | comando_controle_fluxo
        ;

comando:
          comando_sem_entrada_saida
        | comando_entrada_saida
        ;

comando_return: TK_PR_RETURN expressao {
              $$ = ast_return_make($2);
          };

comando_continue: TK_PR_CONTINUE;

comando_break:  TK_PR_BREAK;

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

lit_numerico:
          TK_LIT_INT {
	      $$ = ast_literal_make(yylval.valor_lexico);
	  }
        | TK_LIT_FLOAT {
	      $$ = ast_literal_make(yylval.valor_lexico);
	  }
        ;

comando_controle_fluxo:
          comando_if
        | comando_for
        | comando_while
        | comando_do_while
        | comando_foreach
        | comando_switch_case
        ;

comando_if:
          TK_PR_IF '(' expressao ')' TK_PR_THEN bloco_comandos {
	      AST_Block *then_block = (AST_Block*)$6;
	      $$ = ast_if_make($3, then_block->first_command, NULL);
	      ast_block_free((AST_Block*)$6);
	  }
        | TK_PR_IF '(' expressao ')' TK_PR_THEN bloco_comandos TK_PR_ELSE bloco_comandos {
	      AST_Block *then_block = (AST_Block*)$6;
	      AST_Block *else_block = (AST_Block*)$8;
	      $$ = ast_if_make($3, then_block->first_command, else_block->first_command);
	      ast_block_free((AST_Block*)$6);
	      ast_block_free((AST_Block*)$8);
	  }
        ;

/* TODO: Finish for and foreach */
comando_foreach:
        TK_PR_FOREACH '(' TK_IDENTIFICADOR ':' lista_expressoes ')' bloco_comandos;

comando_for:
        TK_PR_FOR '(' lista_comandos ':' expressao ':' lista_comandos ')' bloco_comandos;

lista_comandos:
          comando_sem_entrada_saida
        | lista_comandos ',' comando_sem_entrada_saida
        ;

comando_while:
        TK_PR_WHILE '(' expressao ')' TK_PR_DO bloco_comandos {
            $$ = ast_while_make();
        };

comando_do_while:
        TK_PR_DO bloco_comandos TK_PR_WHILE '(' expressao ')' {
            $$ = ast_while_make();
        };

comando_switch_case:
          TK_PR_SWITCH '(' expressao ')' bloco_comandos;

comando_shift:
          TK_IDENTIFICADOR TK_OC_SL TK_LIT_INT {
	      // @Todo: What to do here?
	      // yyvlval.valor_lexico will always point to the last entry in the table.
	      AST_Identifier *id = (AST_Identifier*)ast_identifier_make(yylval.valor_lexico);
	      AST_Literal *number = (AST_Literal*)ast_literal_make(yylval.valor_lexico);
	      $$ = ast_shift_make(id, number, false);
	  }
        | TK_IDENTIFICADOR TK_OC_SR TK_LIT_INT {
	      // @Todo: What to do here?
	      // yyvlval.valor_lexico will always point to the last entry in the table.
	      AST_Identifier *id = (AST_Identifier*)ast_identifier_make(yylval.valor_lexico);
	      AST_Literal *number = (AST_Literal*)ast_literal_make(yylval.valor_lexico);
	      $$ = ast_shift_make(id, number, true);
	  }
        ;

comando_entrada_saida:
          TK_PR_INPUT expressao
        | TK_PR_OUTPUT lista_expressoes      %prec "end_list_expressions"
        ;

chamada_func:
          TK_IDENTIFICADOR '(' lista_expressoes ')' {
              $$ = ast_function_call_make(yylval.valor_lexico, $3);
          }
        | TK_IDENTIFICADOR '(' ')' {
              $$ = ast_function_call_make(yylval.valor_lexico, NULL);
          }
        ;

lista_expressoes:
          expressao
        | lista_expressoes ',' expressao {
              $$ = $3;
              $$->next = $1;
          }
        ;

comando_atribuicao:
          TK_IDENTIFICADOR '=' expressao {
	      AST_Identifier *id = (AST_Identifier*)ast_identifier_make(yylval.valor_lexico);
	      $$ = ast_assignment_make(&id->header, $3);
	  }
        | TK_IDENTIFICADOR '[' expressao ']' '=' expressao {
	      AST_ExprHeader *vec = ast_indexed_vector_make(yylval.valor_lexico, $3);
	      $$ = ast_assignment_make(vec, $6);
	  }
        | TK_IDENTIFICADOR '$' TK_IDENTIFICADOR '=' expressao {
	      // @Todo: Acho que aqui o yylval.valor_lexico nao funciona corretamente, já que
	      // ele aponta para a ultima entrada na tabela de símbolos, e aqui existem 2 identificadores.
	      // @Fixme: Perguntar para o professor.
	      AST_Identifier *user_type_id = (AST_Identifier*)ast_identifier_make(yylval.valor_lexico);
	      AST_ExprHeader *id = ast_identifier_make(yylval.valor_lexico);
	      $$ = ast_assignment_user_type_make(user_type_id, id, $5);
	  }
        ;

expressao:
          expressao_arit
        | expressao_logica
        | TK_LIT_CHAR {$$ = ast_literal_make(yylval.valor_lexico);}
        | TK_LIT_STRING {$$ = ast_literal_make(yylval.valor_lexico);}
        ;

expressao_arit:
          expressao_arit '+' expressao_arit_term1 {
              $$ = ast_arit_expr_make('+', $1, $3);
          }
        | expressao_arit_term1
        ;

expressao_arit_term1:
          expressao_arit_term1 '-' expressao_arit_term2 {
              $$ = ast_arit_expr_make(AST_ARIM_SUBTRACAO, $1, $3);
          }
        | expressao_arit_term2
        ;

expressao_arit_term2:
          expressao_arit_term2 '*' expressao_arit_term3 {
              $$ = ast_arit_expr_make(AST_ARIM_MULTIPLICACAO, $1, $3);
          }
        | expressao_arit_term3
        ;

expressao_arit_term3:
          expressao_arit_term3 '/' expressao_arit_operando {
              $$ = ast_arit_expr_make(AST_ARIM_DIVISAO, $1, $3);
          }
        | expressao_arit_operando
        ;

expressao_arit_operando:
          TK_IDENTIFICADOR {$$ = ast_identifier_make(yyval.valor_lexico);}
        | TK_IDENTIFICADOR '[' expressao ']' {$$ = ast_indexed_vector_make(yyval.valor_lexico, $3);}
        | lit_numerico {$$ = ast_literal_make(yyval.valor_lexico);}
        | '-' lit_numerico {$$ = ast_arit_expr_make(AST_ARIM_INVERSAO, $2, NULL);}
        | chamada_func
        | '(' expressao_arit ')' {$$ = $2;}
        ;

expressao_logica:
          expressao_arit operator_relacional expressao_arit {
              $$ = ast_logic_expr_make($2, $1, $3);
          }
        | expressao_logica TK_OC_AND expressao_logica1 {
              $$ = ast_logic_expr_make(AST_LOGICO_E, $1, $3);
          }
        | expressao_logica1
        ;

expressao_logica1:
          expressao_logica1 TK_OC_OR expressao_logica2 {
              $$ = ast_logic_expr_make(AST_LOGICO_OU, $1, $3);
          }
        | expressao_logica2
        ;

expressao_logica2:
          expressao_logica2 TK_OC_EQ expressao_logica3 {
              $$ = ast_logic_expr_make(AST_LOGICO_COMP_IGUAL, $1, $3);
          }
        | expressao_logica3
        ;

expressao_logica3:
          expressao_logica3 TK_OC_NE expressao_logica4 {
              $$ = ast_logic_expr_make(AST_LOGICO_COMP_DIF, $1, $3);
          }
        | expressao_logica4
        ;

expressao_logica4:
         '!' expressao_logica4 {
             $$ = ast_logic_expr_make(AST_LOGICO_COMP_NEGACAO, $2, NULL);
         }
        | expressao_logica_operando
        ;

expressao_logica_operando:
          TK_LIT_FALSE {$$ = ast_literal_make(yylval.valor_lexico);}
        | TK_LIT_TRUE {$$ = ast_literal_make(yylval.valor_lexico);}
        | '(' expressao_logica ')' {$$ = $2;}
       ;

operator_relacional:
          TK_OC_LE {$$ = AST_LOGICO_COMP_LE;}
        | TK_OC_GE {$$ = AST_LOGICO_COMP_GE;}
        | TK_OC_EQ {$$ = AST_LOGICO_COMP_IGUAL;}
        | TK_OC_NE {$$ = AST_LOGICO_COMP_DIF;}
        ;
