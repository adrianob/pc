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
#include "enums.h"
#include "semantic.h"
#include "sds.h"
#include "stack.h"

AST_Program *g_program = NULL;
STACK_T *g_scopes = NULL;
Array(SemanticError) g_semantic_errors = NULL;

static void comp_print_table2(comp_dict_t * dict) {
    printf("================ Printing symbols table ==============\n");
    for (int hash = 0; hash < dict->size; ++hash) {
        comp_dict_item_t *search_item = dict->data[hash];
        while (search_item) {
            TableSymbol *symbol = (TableSymbol *)search_item->value;
            cc_dict_etapa_2_print_entrada(search_item->key, symbol->line_number, symbol->token_type);
            search_item = search_item->next;
        }
    }
    printf("======================= Done =========================\n");
}

static inline char *get_key_from_identifier(AST_Identifier *id) {
    return ((TableSymbol*)id->entry->value)->value_string_or_ident;
}

static inline int get_line_from_identifier(AST_Identifier *id) {
    return ((TableSymbol*)id->entry->value)->line_number;
}

static inline comp_dict_t *dict_from_tree(comp_tree_t *node) {
    return (comp_dict_t*)node->value;
}

static void push_declared_error(AST_Identifier *id) {
    SemanticError err;
    err.type = IKS_ERROR_DECLARED;
    err.description = sdscatprintf(sdsempty(),
                                   "%d: Identifier %s already declared.",
                                   get_line_from_identifier(id),
                                   get_key_from_identifier(id));
    array_push(g_semantic_errors, err);
}

static void push_undeclared_error(AST_Identifier *id) {
    SemanticError err;
    err.type = IKS_ERROR_UNDECLARED;
    err.description = sdscatprintf(sdsempty(),
                                   "%d: Identifier %s was not declared.",
                                   get_line_from_identifier(id),
                                   get_key_from_identifier(id));
    array_push(g_semantic_errors, err);
}

static void push_variable_error(AST_Identifier *id) {
    SemanticError err;
    err.type = IKS_ERROR_VARIABLE;
    err.description = sdscatprintf(sdsempty(),
                                   "%d: Identifier %s has to be used as a variable.",
                                   get_line_from_identifier(id),
                                   get_key_from_identifier(id));
    array_push(g_semantic_errors, err);
}

static void push_vector_error(AST_Identifier *id) {
    SemanticError err;
    err.type = IKS_ERROR_VARIABLE;
    err.description = sdscatprintf(sdsempty(),
                                   "%d: Identifier %s has to be used as a vector.",
                                   get_line_from_identifier(id),
                                   get_key_from_identifier(id));
    array_push(g_semantic_errors, err);
}

static void push_missing_args_error(AST_Identifier *id) {
    SemanticError err;
    err.type = IKS_ERROR_MISSING_ARGS;
    err.description = sdscatprintf(sdsempty(),
                                   "%d: %s called with missing arguments.",
                                   get_line_from_identifier(id),
                                   get_key_from_identifier(id));
    array_push(g_semantic_errors, err);
}

static void push_excess_args_error(AST_Identifier *id) {
    SemanticError err;
    err.type = IKS_ERROR_EXCESS_ARGS;
    err.description = sdscatprintf(sdsempty(),
                                   "%d: %s called with excessive arguments.",
                                   get_line_from_identifier(id),
                                   get_key_from_identifier(id));
    array_push(g_semantic_errors, err);
}

static DeclarationHeader *find_declaration_recursive(AST_Identifier *id) {
    comp_dict_t *scope_dict = top(g_scopes);
    char *id_key = get_key_from_identifier(id);

    comp_dict_item_t *entry = NULL;

    if ((entry = dict_get_entry(scope_dict, id_key))) {
        return (DeclarationHeader*)entry->value;
    } else {
        //didn't find declaration in current scope
        STACK_T *head = g_scopes;
        bool found = false;

        while(head) {
            comp_dict_t *dict = top(head);
            entry = dict_get_entry(dict, id_key);
            if (entry) {
                return (DeclarationHeader*)entry->value;
            }
            head = head->next;
        }
    }

    return NULL;
}

static void find_declaration(AST_Identifier *id, IKS_Type type) {
    //check if already declared
    comp_dict_t *scope_dict = top(g_scopes);
    char *id_key = get_key_from_identifier(id);
    if (dict_get_entry(scope_dict, id_key)) {
        push_declared_error(id);
    } else {
        DeclarationHeader *decl = variable_declaration_make(id, NULL, type);
        dict_put(scope_dict, id_key, decl);
    }
}

%}

%union {
    comp_dict_item_t *valor_lexico;
    AST_Function *ast_function;
    AST_Header *ast_header;
    AST_Identifier *ast_identifier;
    int op;
    IKS_Type iks_type;
    UserTypeField *user_type_field;
    DeclarationHeader *declaration_header;
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
%token<valor_lexico> TK_LIT_INT
%token<valor_lexico> TK_LIT_FLOAT
%token<valor_lexico> TK_LIT_FALSE
%token<valor_lexico> TK_LIT_TRUE
%token<valor_lexico> TK_LIT_CHAR
%token<valor_lexico> TK_LIT_STRING
%token<valor_lexico> TK_IDENTIFICADOR
%token TOKEN_ERRO

%right '='
%left TK_OC_GE
%left TK_OC_LE
%left '+' '-'
%left '*' '/'
%left '!'

%nonassoc "end_list_expressions"
%nonassoc ','

%type<ast_function>  decl_func;
%type<ast_function>  decl_func2;
%type<ast_header>    token_lit;
%type<ast_header>    comando_case;
%type<ast_header>    comando_if;
%type<ast_header>    comando_shift;
%type<ast_header>    comando_decl_var_init;
%type<ast_header>    comando_decl_var;
%type<ast_header>    comando_while;
%type<ast_header>    comando_entrada_saida;
%type<ast_header>    comando_controle_fluxo;
%type<ast_header>    comando_for;
%type<ast_header>    comando_foreach;
%type<ast_header>    comando_switch_case;
%type<ast_header>    comando_do_while;
%type<ast_header>    comando_atribuicao;
%type<ast_header>    comando_return;
%type<ast_header>    comando_continue;
%type<ast_header>    comando_break;
%type<ast_header>    bloco_comandos;
%type<ast_header>    seq_comandos;
%type<ast_header>    comando;
%type<ast_header>    comando_sem_entrada_saida;
%type<ast_header>    expressao;
%type<ast_header>    expressao_arit;
%type<ast_header>    expressao_arit_term1;
%type<ast_header>    expressao_arit_term2;
%type<ast_header>    expressao_arit_term3;
%type<ast_header>    expressao_arit_operando;
%type<ast_header>    chamada_func;
%type<ast_header>    lista_expressoes;
%type<ast_header>    lista_comandos;
%type<ast_header>    lit_numerico;
%type<user_type_field> campo;
%type<user_type_field> lista_campos;
%type<declaration_header> parametro_entrada;
%type<declaration_header> parametros_entrada;
%type<declaration_header> lista_entrada;

%type<op>            operator_relacional;
%type<iks_type>      tipo_primitivo;

%type<ast_header>    expressao_logica;
%type<ast_header>    expressao_logica1;
%type<ast_header>    expressao_logica2;
%type<ast_header>    expressao_logica3;
%type<ast_header>    expressao_logica4;
%type<ast_header>    expressao_logica_operando;

%%
programa:
        %empty
        {
            if (!g_program) g_program = ast_program_make();
            if (!g_semantic_errors) array_init(g_semantic_errors);
            if (list_size(g_scopes) == 0) {
                comp_dict_t * dic = dict_new();
                push(&g_scopes, dic);
            }
        }
        |   programa decl_global
        {
            if (!g_program) g_program = ast_program_make();
            if (!g_semantic_errors) array_init(g_semantic_errors);
            if (list_size(g_scopes) == 0) {
                comp_dict_t * dic = dict_new();
                push(&g_scopes, dic);
            }
        }
        |   programa decl_tipos
        {
            if (!g_program) g_program = ast_program_make();
            if (!g_semantic_errors) array_init(g_semantic_errors);
            if (list_size(g_scopes) == 0) {
                comp_dict_t * dic = dict_new();
                push(&g_scopes, dic);
            }
        }
        |   programa decl_func
        {
            if (!g_program) g_program = ast_program_make();
            if (!g_semantic_errors) array_init(g_semantic_errors);
            if (list_size(g_scopes) == 0) {
                comp_dict_t * dic = dict_new();
                push(&g_scopes, dic);
            }

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
        TK_PR_CLASS TK_IDENTIFICADOR '[' lista_campos ']' ';'
        {
            comp_dict_t *scope_dict = top(g_scopes);
            AST_Identifier *id = (AST_Identifier*)ast_identifier_make($2);
            // Add user type to global scope
            char *id_key = get_key_from_identifier(id);

            if (dict_get_entry(scope_dict, id_key)) {
                push_declared_error(id);
            // @Todo(leo): free contents of $2 and $4
            } else {
                DeclarationHeader *ud = user_type_declaration_make(id, $4);
                dict_put(scope_dict, id_key, ud);
            }
        };

lista_campos:
                campo
    |   lista_campos ':' campo
        {
            if ($$ == NULL) {
                $$ = $3;
            } else {
                $$ = $1;
                UserTypeField *search = $$;
                while (search->next) search = search->next;
                search->next = $3;
            }
        };

campo:
        TK_PR_PROTECTED tipo_primitivo TK_IDENTIFICADOR
        {
            AST_Header *id = ast_identifier_make($3);
            $$ = user_type_field_make($2, id, FV_PROTECTED);
        }
        |   TK_PR_PRIVATE tipo_primitivo TK_IDENTIFICADOR
        {
            AST_Header *id = ast_identifier_make($3);
            $$ = user_type_field_make($2, id, FV_PRIVATE);
        }
        |   TK_PR_PUBLIC tipo_primitivo TK_IDENTIFICADOR
        {
            AST_Header *id = ast_identifier_make($3);
            $$ = user_type_field_make($2, id, FV_PUBLIC);
        }
        |   TK_PR_PUBLIC tipo_primitivo TK_IDENTIFICADOR '[' TK_LIT_INT ']'
        {
            AST_Header *expr = ast_literal_make($5);
            AST_Header *indexed_vector = ast_indexed_vector_make($3, expr);
            $$ = user_type_field_make($2, indexed_vector, FV_PUBLIC);
        }
        |   TK_PR_PRIVATE tipo_primitivo TK_IDENTIFICADOR '[' TK_LIT_INT ']'
        {
            AST_Header *expr = ast_literal_make($5);
            AST_Header *indexed_vector = ast_indexed_vector_make($3, expr);
            $$ = user_type_field_make($2, indexed_vector, FV_PRIVATE);
        }
        |   TK_PR_PROTECTED tipo_primitivo TK_IDENTIFICADOR '[' TK_LIT_INT ']'
        {
            AST_Header *expr = ast_literal_make($5);
            AST_Header *indexed_vector = ast_indexed_vector_make($3, expr);
            $$ = user_type_field_make($2, indexed_vector, FV_PROTECTED);
        }
        ;

tipo_primitivo:
        TK_PR_INT {$$ = IKS_INT;}
    |   TK_PR_FLOAT {$$ = IKS_FLOAT;}
    |   TK_PR_BOOL {$$ = IKS_BOOL;}
    |   TK_PR_CHAR {$$ = IKS_CHAR;}
    |   TK_PR_STRING {$$ = IKS_STRING;}
        ;

decl_global:
        decl_global_non_static
        | TK_PR_STATIC decl_global_non_static
        ;

decl_global_non_static:
        tipo_primitivo TK_IDENTIFICADOR ';'
        {
            comp_dict_t *scope_dict = top(g_scopes);
            AST_Identifier *id = (AST_Identifier*)ast_identifier_make($2);

            char *id_key = get_key_from_identifier(id);

            printf("Declaring global variable\n");

            if (dict_get_entry(scope_dict, id_key)) {
                push_declared_error(id);
            } else {
                DeclarationHeader *decl = variable_declaration_make(id, NULL, $1);
                dict_put(scope_dict, id_key, decl);
            }
        }
        |   tipo_primitivo TK_IDENTIFICADOR '[' TK_LIT_INT ']' ';'
        {
            AST_Identifier *id = (AST_Identifier*)ast_identifier_make($2);
            AST_Literal *count = (AST_Literal*)ast_literal_make($4);

            comp_dict_t *scope_dict = top(g_scopes);
            char *id_key = get_key_from_identifier(id);

            if (dict_get_entry(scope_dict, id_key)) {
                push_declared_error(id);
            } else {
                DeclarationHeader *decl = vector_declaration_make(id, count, $1);
                dict_put(scope_dict, id_key, decl);
            }
        }
        |   TK_IDENTIFICADOR TK_IDENTIFICADOR ';'
        {
            AST_Identifier *id = (AST_Identifier*)ast_identifier_make($2);
            AST_Identifier *ret_id = (AST_Identifier*)ast_identifier_make($1);

            comp_dict_t *scope_dict = top(g_scopes);
            char *id_key = get_key_from_identifier(id);

            if (dict_get_entry(scope_dict, id_key)) {
                // If name already is declared, push error and do nothing.
                push_declared_error(id);
            } else {
                char *ret_id_key = get_key_from_identifier(ret_id);
                DeclarationHeader *ret_decl_hdr = (DeclarationHeader*)dict_get_entry(scope_dict, ret_id_key);

                if (!ret_decl_hdr) push_undeclared_error(ret_id);

                DeclarationHeader *decl = variable_declaration_make(
                    id, ret_id, (ret_decl_hdr) ? ret_decl_hdr->type : IKS_UNDEFINED
                );

                dict_put(scope_dict, id_key, decl);
            }
        }
        ;

decl_func:
        TK_PR_STATIC decl_func2 {$$ = $2;}
    |   decl_func2
    ;

decl_func2:
        tipo_primitivo TK_IDENTIFICADOR { comp_dict_t * dic = dict_new(); push(&g_scopes, dic); } lista_entrada bloco_comandos
        {
            AST_Identifier *id = (AST_Identifier*)ast_identifier_make($2);
            AST_Block *block = (AST_Block*)$5;

            $$ = ast_function_make(id, block->first_command, $1, NULL);
            char *id_key = get_key_from_identifier(id);

            //check for function identifier validity
            comp_dict_t *global_scope_dict = top(g_scopes);

            if (dict_get_entry(global_scope_dict, id_key)) {
                push_declared_error(id);
            } else {
                DeclarationHeader *decl = function_declaration_make(id, NULL, $1, $4);
                dict_put(global_scope_dict, id_key, decl);
            }
            pop(&g_scopes);
            block->first_command = NULL;
            ast_block_free(block);
        }
        |   TK_IDENTIFICADOR TK_IDENTIFICADOR { comp_dict_t * dic = dict_new(); push(&g_scopes, dic); } lista_entrada bloco_comandos
        {
            AST_Identifier *return_id = (AST_Identifier*)ast_identifier_make($1);
            AST_Identifier *id = (AST_Identifier*)ast_identifier_make($2);
            AST_Block *block = (AST_Block*)$5;

            $$ = ast_function_make(id, block->first_command, IKS_UNDECIDED, return_id);

            block->first_command = NULL;

            comp_dict_t *global_scope_dict = top(g_scopes);
            char *id_key = get_key_from_identifier(id);

            //check for function identifier validity
            if (dict_get_entry(global_scope_dict, id_key)) {
                push_declared_error(id);
            } else {
                char *ret_id_key = get_key_from_identifier(return_id);
                DeclarationHeader *ret_decl_hdr = (DeclarationHeader*)dict_get_entry(global_scope_dict, ret_id_key);

                if (!ret_decl_hdr) push_undeclared_error(return_id);

                DeclarationHeader *decl = function_declaration_make(
                    id, return_id, (ret_decl_hdr) ? ret_decl_hdr->type : IKS_UNDEFINED, $4
                );

                dict_put(global_scope_dict, id_key, decl);

            }
            ast_block_free(block);
            pop(&g_scopes);
        }
        ;

lista_entrada:
        '(' ')' { $$ = NULL; }
        |'(' parametros_entrada ')' { $$ = $2; }
        ;

parametros_entrada:
                parametro_entrada
        |       parametros_entrada ',' parametro_entrada
                {
                    if ($$ == NULL) {
                        $$ = $3;
                    } else {
                        $$ = $1;
                        DeclarationHeader *search = $$;
                        while (search->next) search = search->next;
                        search->next = $3;
                    }
                }
                ;

parametro_entrada:
        tipo_primitivo TK_IDENTIFICADOR
        {
            AST_Identifier *id = (AST_Identifier*)ast_identifier_make($2);
            DeclarationHeader *decl = variable_declaration_make(id, NULL, $1);
            comp_dict_t *scope_dict = top(g_scopes);
            char *id_key = get_key_from_identifier(id);
            dict_put(scope_dict, id_key, decl);
            $$ = decl;
        }
        | TK_PR_CONST tipo_primitivo TK_IDENTIFICADOR
        {
            AST_Identifier *id = (AST_Identifier*)ast_identifier_make($3);
            DeclarationHeader *decl = variable_declaration_make(id, NULL, $2);
            comp_dict_t *scope_dict = top(g_scopes);
            char *id_key = get_key_from_identifier(id);
            dict_put(scope_dict, id_key, decl);
            $$ = decl;
        }
        |             TK_IDENTIFICADOR TK_IDENTIFICADOR
        {
            AST_Identifier *type_id = (AST_Identifier*)ast_identifier_make($1);
            AST_Identifier *id = (AST_Identifier*)ast_identifier_make($2);
            comp_dict_t *scope_dict = top(g_scopes);
            char *id_key = get_key_from_identifier(id);
            char *type_id_key = get_key_from_identifier(type_id);
            DeclarationHeader *type_decl_hdr = (DeclarationHeader*)dict_get_entry(scope_dict, type_id_key);

            if (!type_decl_hdr) push_undeclared_error(type_id);

            DeclarationHeader *decl = variable_declaration_make(
                id, type_id, (type_decl_hdr) ? type_decl_hdr->type : IKS_UNDEFINED
            );

            dict_put(scope_dict, id_key, decl);
            $$ = decl;
        }
        | TK_PR_CONST TK_IDENTIFICADOR TK_IDENTIFICADOR
        {
            AST_Identifier *type_id = (AST_Identifier*)ast_identifier_make($2);
            AST_Identifier *id = (AST_Identifier*)ast_identifier_make($3);
            comp_dict_t *scope_dict = top(g_scopes);
            char *id_key = get_key_from_identifier(id);
            char *type_id_key = get_key_from_identifier(type_id);
            DeclarationHeader *type_decl_hdr = (DeclarationHeader*)dict_get_entry(scope_dict, type_id_key);

            if (!type_decl_hdr) push_undeclared_error(type_id);

            DeclarationHeader *decl = variable_declaration_make(
                id, type_id, (type_decl_hdr) ? type_decl_hdr->type : IKS_UNDEFINED
            );

            dict_put(scope_dict, id_key, decl);
            $$ = decl;
        }
        ;

bloco_comandos:
        '{' seq_comandos '}' {
            $$ = ast_block_make($2);
        }
        | '{' '}' {$$ = ast_block_make(NULL);}
        ;

seq_comandos:
        comando ';'
        | comando_case
        | seq_comandos comando ';' {
            if ($$ == NULL) {
                $$ = $2;
            } else {
                $$ = $1;
                if ($2) {
                    AST_Header *search = $$;
                    while (search->next) search = search->next;
                    search->next = $2;
                }
            }
        }
        | seq_comandos comando_case {
            if ($$ == NULL) {
                $$ = $2;
            } else {
                $$ = $1;
                AST_Header *search = $$;
                while (search->next) search = search->next;
                search->next = $2;
            }
        }
        ;

comando_case:
        TK_PR_CASE TK_LIT_INT ':' {
        AST_Literal *lit = (AST_Literal*)ast_literal_make($2);
        $$ = ast_case_make(lit);
    };

comando_sem_entrada_saida:
        comando_decl_var
        | comando_decl_var_init
        | {
            //create new scope for block
            comp_dict_t * block_dic = dict_new();
            push(&g_scopes, block_dic);
        } bloco_comandos {$$ = NULL; pop(&g_scopes);}
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

comando_continue: TK_PR_CONTINUE {
            $$ = ast_continue_make();
        };

comando_break:  TK_PR_BREAK {
            $$ = ast_break_make();
        };

comando_decl_var:
        comando_decl_var_2                          { $$ = NULL; }
        |   TK_PR_STATIC             comando_decl_var_2 { $$ = NULL; }
        |   TK_PR_CONST comando_decl_var_2              { $$ = NULL; }
        |   TK_PR_STATIC TK_PR_CONST comando_decl_var_2 { $$ = NULL; }
        ;

comando_decl_var_2:
        tipo_primitivo TK_IDENTIFICADOR
        {
            AST_Identifier *id = (AST_Identifier*)ast_identifier_make($2);
            //check if already declared
            comp_dict_t *scope_dict = top(g_scopes);
            char *id_key = get_key_from_identifier(id);
            if (dict_get_entry(scope_dict, id_key)) {
                push_declared_error(id);
            } else {
                DeclarationHeader *decl = variable_declaration_make(id, NULL, $1);
                dict_put(scope_dict, id_key, decl);
            }
        }
        |   TK_IDENTIFICADOR TK_IDENTIFICADOR
        {
            AST_Identifier *return_id = (AST_Identifier*)ast_identifier_make($1);
            AST_Identifier *id = (AST_Identifier*)ast_identifier_make($2);

            comp_dict_t *scope_dict = top(g_scopes);
            char *id_key = get_key_from_identifier(id);

            if (dict_get_entry(scope_dict, id_key)) {
                push_declared_error(id);
            } else {
                char *ret_id_key = get_key_from_identifier(return_id);
                DeclarationHeader *ret_decl_hdr = NULL;
                STACK_T *head = g_scopes;
                bool found = false;
                while(head){
                    comp_dict_t *dict = top(head);
                    ret_decl_hdr = (DeclarationHeader*)dict_get_entry(dict, ret_id_key);
                    if (ret_decl_hdr) found = true;
                    head = head->next;
                }

                if (!found) push_undeclared_error(return_id);

                DeclarationHeader *decl = variable_declaration_make(
                    id, return_id, (ret_decl_hdr) ? ret_decl_hdr->type : IKS_UNDEFINED
                );

                dict_put(scope_dict, id_key, decl);
            }
        }
        ;

comando_decl_var_init:
        tipo_primitivo TK_IDENTIFICADOR TK_OC_LE TK_IDENTIFICADOR
        {
            AST_Identifier *id = (AST_Identifier*)ast_identifier_make($2);
            AST_Identifier *id2 = (AST_Identifier*)ast_identifier_make($4);
            $$ = ast_assignment_make(&id->header, &id2->header);

            find_declaration(id, $1);

        }
        |   tipo_primitivo TK_IDENTIFICADOR TK_OC_LE token_lit
        {
            AST_Identifier *id = (AST_Identifier*)ast_identifier_make($2);
            $$ = ast_assignment_make(&id->header, $4);

            find_declaration(id, $1);
        }
        |  TK_PR_STATIC             tipo_primitivo TK_IDENTIFICADOR TK_OC_LE TK_IDENTIFICADOR
        {
            AST_Identifier *id = (AST_Identifier*)ast_identifier_make($3);
            AST_Identifier *id2 = (AST_Identifier*)ast_identifier_make($5);
            $$ = ast_assignment_make(&id->header, &id2->header);
            find_declaration(id, $2);
            //@TODO look for second identifier
        }
        |  TK_PR_STATIC             tipo_primitivo TK_IDENTIFICADOR TK_OC_LE token_lit
        {
            AST_Identifier *id = (AST_Identifier*)ast_identifier_make($3);
            $$ = ast_assignment_make(&id->header, $5);
            find_declaration(id, $2);
        }
        |  TK_PR_CONST tipo_primitivo TK_IDENTIFICADOR TK_OC_LE TK_IDENTIFICADOR
        {
            AST_Identifier *id = (AST_Identifier*)ast_identifier_make($3);
            AST_Identifier *id2 = (AST_Identifier*)ast_identifier_make($5);
            $$ = ast_assignment_make(&id->header, &id2->header);
            find_declaration(id, $2);
        }
        |   TK_PR_CONST tipo_primitivo TK_IDENTIFICADOR TK_OC_LE token_lit
        {
            AST_Identifier *id = (AST_Identifier*)ast_identifier_make($3);
            $$ = ast_assignment_make(&id->header, $5);
            find_declaration(id, $2);
        }
        | TK_PR_STATIC TK_PR_CONST tipo_primitivo TK_IDENTIFICADOR TK_OC_LE TK_IDENTIFICADOR
        {
            AST_Identifier *id = (AST_Identifier*)ast_identifier_make($4);
            AST_Identifier *id2 = (AST_Identifier*)ast_identifier_make($6);
            $$ = ast_assignment_make(&id->header, &id2->header);
            find_declaration(id, $3);
        }
        | TK_PR_STATIC TK_PR_CONST tipo_primitivo TK_IDENTIFICADOR TK_OC_LE token_lit
        {
            AST_Identifier *id = (AST_Identifier*)ast_identifier_make($4);
            $$ = ast_assignment_make(&id->header, $6);
            find_declaration(id, $3);
        }
        ;

token_lit:
        lit_numerico
        | TK_LIT_FALSE {$$ = ast_literal_make($1);}
        | TK_LIT_TRUE {$$ = ast_literal_make($1);}
        | TK_LIT_CHAR {$$ = ast_literal_make($1);}
        | TK_LIT_STRING {$$ = ast_literal_make($1);}
        ;

lit_numerico:
        TK_LIT_INT {
            $$ = ast_literal_make($1);
        }
        | TK_LIT_FLOAT {
            $$ = ast_literal_make($1);
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

        then_block->first_command = NULL;
        ast_block_free((AST_Block*)$6);
    }
        | TK_PR_IF '(' expressao ')' TK_PR_THEN bloco_comandos TK_PR_ELSE bloco_comandos {
        AST_Block *then_block = (AST_Block*)$6;
        AST_Block *else_block = (AST_Block*)$8;
        $$ = ast_if_make($3, then_block->first_command, else_block->first_command);

        then_block->first_command = NULL;
        else_block->first_command = NULL;
        ast_block_free(then_block);
        ast_block_free(else_block);
    }
        ;

/* @Todo: Finish for and foreach */
comando_foreach:
        TK_PR_FOREACH '(' TK_IDENTIFICADOR ':' lista_expressoes ')' bloco_comandos {
            AST_Block *block = (AST_Block*)$7;
            AST_Identifier *id = (AST_Identifier*)ast_identifier_make($3);
            $$ = ast_foreach_make(id, $5, block->first_command);

            block->first_command = NULL;
            ast_block_free(block);
        };

comando_for:
        TK_PR_FOR '(' lista_comandos ':' expressao ':' lista_comandos ')' bloco_comandos {
            AST_Block *block = (AST_Block*)$9;
            $$ = ast_for_make($5, block->first_command, $3, $7);

            block->first_command = NULL;
            ast_block_free(block);
        };

lista_comandos:
        comando_sem_entrada_saida
        | lista_comandos ',' comando_sem_entrada_saida {
            if ($$ == NULL) {
                $$ = $3;
            } else {
                $$ = $1;
                if ($3) {
                    AST_Header *search = $$;
                    while (search->next) search = search->next;
                    search->next = $3;
                }
            }
        }
        ;

comando_while:
        TK_PR_WHILE '(' expressao ')' TK_PR_DO bloco_comandos {
            AST_Block *block = (AST_Block*)$6;
            $$ = ast_while_make($3, block->first_command, false);

            block->first_command = NULL;
            ast_block_free(block);
        };

comando_do_while:
        TK_PR_DO bloco_comandos TK_PR_WHILE '(' expressao ')' {
            AST_Block *block = (AST_Block*)$2;
            $$ = ast_while_make($5, block->first_command, true);

            block->first_command = NULL;
            ast_block_free(block);
        };

comando_switch_case:
        TK_PR_SWITCH '(' expressao ')' bloco_comandos {
            AST_Block *block = (AST_Block*)$5;
            $$ = ast_switch_make($3, block->first_command);

            block->first_command = NULL;
            ast_block_free(block);
        };

comando_shift:
        TK_IDENTIFICADOR TK_OC_SL TK_LIT_INT {
            AST_Identifier *id = (AST_Identifier*)ast_identifier_make($1);
            AST_Literal *number = (AST_Literal*)ast_literal_make($3);
            $$ = ast_shift_make(id, number, false);
        }
        | TK_IDENTIFICADOR TK_OC_SR TK_LIT_INT {
            AST_Identifier *id = (AST_Identifier*)ast_identifier_make($1);
            AST_Literal *number = (AST_Literal*)ast_literal_make($3);
            $$ = ast_shift_make(id, number, true);
        }
        ;

comando_entrada_saida:
        TK_PR_INPUT expressao {
            $$ = ast_input_make($2);
        }
        | TK_PR_OUTPUT lista_expressoes      %prec "end_list_expressions" {
            $$ = ast_output_make($2);
        }
        ;

chamada_func:
        TK_IDENTIFICADOR '(' lista_expressoes ')' {
            // Guarantee that identifier is a function
            AST_Identifier *id = (AST_Identifier*)ast_identifier_make($1);
            char *id_key = get_key_from_identifier(id);

            DeclarationHeader *decl = NULL;

            if ((decl = find_declaration_recursive(id))) {
                if (decl->type == DT_VARIABLE) {
                    push_variable_error(id);
                } else if (decl->type == DT_VECTOR) {
                    push_vector_error(id);
                } else if (decl->type == DT_FUNCTION) {
                    FunctionDeclaration *func_decl = (FunctionDeclaration*)decl;
                    int num_params = function_declaration_num_params(func_decl);

                    int num_expressions = 0;
                    AST_Header *search = $3;
                    while (search) {
                        num_expressions++;
                        search = search->next;
                    }

                    if (num_expressions < num_params) {
                        push_missing_args_error(id);
                    } else if (num_expressions > num_params) {
                        push_excess_args_error(id);
                    } else {
                        // @Todo(leo)
                        // Correct number of parameters, now check the types
                        /* DeclarationHeader *param = func_decl->first_param; */
                        /* AST_Header *expr = $3; */
                        /* while (param) { // we know that param and expr are the same length */
                        /*     Assert(param->type == DT_VARIABLE); */

                        /*     VariableDeclaration *var_decl = (VariableDeclaration*)param; */
                        /*     e */

                        /*     if (var_decl->type != expr_type) { */
                        /*         // error */
                        /*     } */

                        /*     param = param->next; */
                        /*     expr = expr->ext; */
                        /* } */
                    }
                } else {
                    Assert(false);
                }
            } else {
                push_undeclared_error(id);
            }

            $$ = ast_function_call_make($1, $3);
        }
        | TK_IDENTIFICADOR '(' ')' {
            // Guarantee that identifier is a function
            AST_Identifier *id = (AST_Identifier*)ast_identifier_make($1);
            char *id_key = get_key_from_identifier(id);

            DeclarationHeader *decl = NULL;

            if ((decl = find_declaration_recursive(id))) {
                if (decl->type == DT_VARIABLE) {
                    push_variable_error(id);
                } else if (decl->type == DT_VECTOR) {
                    push_vector_error(id);
                } else if (decl->type == DT_FUNCTION) {
                    // @Todo(leo): check function parameters
                    FunctionDeclaration *func_decl = (FunctionDeclaration*)decl;
                    if (func_decl->first_param) {
                        push_missing_args_error(id);
                    }
                } else {
                    Assert(false);
                }
            } else {
                push_undeclared_error(id);
            }

            $$ = ast_function_call_make($1, NULL);
        }
        ;

lista_expressoes:
        expressao
        | lista_expressoes ',' expressao {
            $$ = $1;
            // Jump to the end of the $1 expression list and append $3.
            AST_Header *header = $$;
            while (header->next) header = header->next;
            header->next = $3;
        }
        ;

comando_atribuicao:
        TK_IDENTIFICADOR '=' expressao {
            AST_Identifier *id = (AST_Identifier*)ast_identifier_make($1);
            $$ = ast_assignment_make(&id->header, $3);

            DeclarationHeader *decl = find_declaration_recursive(id);
            if (!decl) push_undeclared_error((AST_Identifier*)id);
        }
        | TK_IDENTIFICADOR '[' expressao ']' '=' expressao {
            AST_Header *vec = ast_indexed_vector_make($1, $3);
            $$ = ast_assignment_make(vec, $6);
            AST_Identifier *id = (AST_Identifier*)ast_identifier_make($1);

            DeclarationHeader *decl = find_declaration_recursive(id);
            if (!decl) push_undeclared_error((AST_Identifier*)id);
        }
        | TK_IDENTIFICADOR '$' TK_IDENTIFICADOR '=' expressao {
            AST_Identifier *user_type_id = (AST_Identifier*)ast_identifier_make($1);
            AST_Header *id = ast_identifier_make($3);
            $$ = ast_assignment_user_type_make(user_type_id, id, $5);

            DeclarationHeader *decl = find_declaration_recursive((AST_Identifier*)id);
            if (!decl) push_undeclared_error((AST_Identifier*)id);
        }
        ;

expressao:
        expressao_arit
        | expressao_logica
        | TK_LIT_CHAR {$$ = ast_literal_make($1);}
        | TK_LIT_STRING {$$ = ast_literal_make($1);}
        ;

expressao_arit:
        expressao_arit '+' expressao_arit_term1 {
            $$ = ast_arit_expr_make(AST_ARIM_SOMA, $1, $3);
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
        TK_IDENTIFICADOR {$$ = ast_identifier_make($1);}
        | TK_IDENTIFICADOR '[' expressao ']' {$$ = ast_indexed_vector_make($1, $3);}
        | lit_numerico
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
        TK_LIT_FALSE {$$ = ast_literal_make($1);}
        | TK_LIT_TRUE {$$ = ast_literal_make($1);}
        | '(' expressao_logica ')' {$$ = $2;}
        ;

operator_relacional:
        TK_OC_LE {$$ = AST_LOGICO_COMP_LE;}
        | TK_OC_GE {$$ = AST_LOGICO_COMP_GE;}
        | TK_OC_EQ {$$ = AST_LOGICO_COMP_IGUAL;}
        | TK_OC_NE {$$ = AST_LOGICO_COMP_DIF;}
        ;
