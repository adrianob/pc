#include "cc_misc.h"
#include "cc_dict.h"
#include "main.h"
#include "table_symbol.h"
#include "cc_ast.h"
#include "macros.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

extern int g_line_number;
extern AST_Program *g_program;

int comp_get_line_number(void) { return g_line_number; }

void yyerror(char const *mensagem) {
    fprintf(stderr, "%d: %s\n", g_line_number, mensagem);
}

void main_init(int argc, char **argv) { dict = dict_new(); }

static void remove_dict_items(comp_dict_t *dict) {
    for (int hash = 0; hash < dict->size; ++hash) {
        while (dict->data[hash]) {
            TableSymbol *value = dict_remove(dict, dict->data[hash]->key);
            table_symbol_free(value);
        }
    }
    assert(dict->occupation == 0);
}

void print_expression_to_graph(void *parent, AST_Header *expr) {
    switch (expr->type) {
    case AST_IDENTIFICADOR: {
        AST_Identifier *ident = (AST_Identifier *)expr;
	
        TableSymbol *symbol = ident->entry->value;
        gv_declare(expr->type, expr, symbol->value_string_or_ident);
        gv_connect(parent, expr);
    } break;
    case AST_LITERAL: {
        AST_Literal *literal = (AST_Literal *)expr;
        TableSymbol *symbol = literal->entry->value;
        switch (symbol->token_type) {
        case POA_LIT_INT: {
            char str[100];
            sprintf(str, "%d", symbol->value_int);
            gv_declare(expr->type, expr, str);
        } break;
        case POA_LIT_FLOAT: {
            char str[100];
            snprintf(str, 100, "%f", symbol->value_float);
            gv_declare(expr->type, expr, str);
        } break;
        case POA_LIT_CHAR: {
            char str[2];
            str[0] = symbol->value_char;
            str[1] = '\0';
            gv_declare(expr->type, expr, str);
        } break;
        case POA_LIT_STRING: {
            gv_declare(expr->type, expr, (char *)symbol->value_string_or_ident);
        } break;
        case POA_LIT_BOOL: {
            if (symbol->value_bool) {
                gv_declare(expr->type, expr, "TRUE");
            } else {
                gv_declare(expr->type, expr, "FALSE");
            }
        } break;
        }
        gv_connect(parent, expr);
    } break;
    case AST_ARIM_SUBTRACAO:
    case AST_ARIM_MULTIPLICACAO:
    case AST_ARIM_DIVISAO:
    case AST_ARIM_INVERSAO:
    case AST_ARIM_SOMA: {
        gv_declare(expr->type, expr, NULL);
        gv_connect(parent, expr);
        AST_AritExpr *express = (AST_AritExpr *)expr;
        print_expression_to_graph(expr, express->first);
        print_expression_to_graph(expr, express->second);
    } break;
    case AST_LOGICO_COMP_DIF:
    case AST_LOGICO_COMP_IGUAL:
    case AST_LOGICO_COMP_LE:
    case AST_LOGICO_COMP_GE:
    case AST_LOGICO_E:
    case AST_LOGICO_COMP_L:
    case AST_LOGICO_COMP_G:
    case AST_LOGICO_COMP_NEGACAO:
    case AST_LOGICO_OU: {
        gv_declare(expr->type, expr, NULL);
        gv_connect(parent, expr);
        AST_LogicExpr *express = (AST_LogicExpr *)expr;
        print_expression_to_graph(expr, express->first);
        print_expression_to_graph(expr, express->second);
    } break;
    default:
        Assert(false);
    }
}

void print_command_to_graph(void *parent, AST_Header *cmd) {
    if(cmd->type != AST_NOCODE){
      gv_declare(cmd->type, cmd, NULL);
      gv_connect(parent, cmd);
    } else{
      gv_connect(parent, cmd->next);
    }

    switch (cmd->type) {
    case AST_IF_ELSE: {
        AST_IfElse *if_else = (AST_IfElse *)cmd;

        print_expression_to_graph(cmd, if_else->condition);

	if (if_else->then_command) {
	    print_command_to_graph(cmd, if_else->then_command);
	}

        if (if_else->else_command) {
            print_command_to_graph(cmd, if_else->else_command);
        }
    } break;
    case AST_SHIFT_LEFT:
    case AST_SHIFT_RIGHT: {
        AST_Shift *shift = (AST_Shift *)cmd;

        print_expression_to_graph(cmd, &shift->identifier->header);
        print_expression_to_graph(cmd, &shift->number->header);
    } break;
    case AST_WHILE_DO:
    case AST_DO_WHILE: {
        AST_While *w = (AST_While *)cmd;
        print_expression_to_graph(cmd, w->condition);
        print_command_to_graph(cmd, w->first_command);
    } break;
    case AST_ATRIBUICAO: {
        AST_Assignment *assign = (AST_Assignment *)cmd;

        if (assign->is_user_type_assignment) {
            print_expression_to_graph(cmd,
                                      &assign->user_type_identifier->header);
        }
        print_expression_to_graph(cmd, assign->identifier);
        print_expression_to_graph(cmd, assign->expr);
    } break;
    case AST_RETURN: {
        AST_Return *ret = (AST_Return *)cmd;
        print_expression_to_graph(cmd, ret->expr);
    } break;
    case AST_BLOCO: {
        AST_Block *block = (AST_Block *)cmd;
	AST_Header *block_cmd = block->first_command;
	void *block_cmd_parent = block;
	while (block_cmd) {
	    print_command_to_graph(block_cmd_parent, block_cmd);
	    block_cmd_parent = block_cmd;
	    block_cmd = block_cmd->next;
	}
    } break;
    case AST_INPUT: {
        AST_Input *in = (AST_Input *)cmd;
        print_expression_to_graph(cmd, in->expr);
    } break;
    case AST_OUTPUT: {
        AST_Output *out = (AST_Output *)cmd;

	void *expr_parent = out;
	AST_Header *expr = out->expr;
	while (expr) {
	    print_expression_to_graph(expr_parent, expr);
	    expr_parent = expr;
	    expr = expr->next;
	}
    } break;
    case AST_CASE:
        break;
    case AST_FOR:
        break;
    case AST_FOREACH:
        break;
    case AST_NOCODE:
        break;
    case AST_SWITCH: {
        AST_Switch *s = (AST_Switch *)cmd;
        print_expression_to_graph(cmd, s->condition);
        print_command_to_graph(cmd, s->first_command);
    } break;
    case AST_CHAMADA_DE_FUNCAO: {
        AST_FunctionCall *funcall = (AST_FunctionCall *)cmd;
	print_expression_to_graph(cmd, &funcall->identifier->header);

	void *param_parent = funcall;
	AST_Header *param = funcall->first_param;
	while (param) {
	    print_expression_to_graph(param_parent, param);
	    param_parent = param;
	    param = param->next;
	}
    } break;
    case AST_CONTINUE:
    case AST_BREAK: {
        // No need to do anything, since this command does not have any
        // children.
    } break;
    default:
        Assert(false);
    }
}

void print_ast_to_graph(AST_Program *program) {
    assert(program->type == AST_PROGRAMA);
    gv_declare(program->type, program, NULL);

    void *func_parent = program;
    AST_Function *func = program->first_func;
    while (func) {
        Assert(func->type == AST_FUNCAO);

        // Start printing list of commands from the function
        TableSymbol *symbol = func->identifier->entry->value;
        gv_declare(func->type, func, symbol->value_string_or_ident);
        gv_connect(func_parent, func);
        AST_Header *cmd = func->first_command;
        void *cmd_parent = func;
        while (cmd) {
            print_command_to_graph(cmd_parent, cmd);

            cmd_parent = cmd;
            cmd = cmd->next;
        }

        func_parent = func;
        func = func->next;
    }
}

void main_finalize(void) {
    gv_init(NULL);
    print_ast_to_graph(g_program);
    gv_close();
    remove_dict_items(dict);
    dict_free(dict);
}

void comp_print_table(void) {
    printf("================ Printing symbols table ==============\n");
    for (int hash = 0; hash < dict->size; ++hash) {
        comp_dict_item_t *search_item = dict->data[hash];
        while (search_item) {
#ifdef AVALIACAO_ETAPA_1
            int *line_number = (int *)search_item->value;
            cc_dict_etapa_1_print_entrada(search_item->key, *line_number);
#elif AVALIACAO_ETAPA_2
            TableSymbol *symbol = (TableSymbol *)search_item->value;
            cc_dict_etapa_2_print_entrada(search_item->key, symbol->line_number, symbol->token_type);
/* #else */
/* #error "Not implemented yet" */
#endif
            search_item = search_item->next;
        }
    }
    printf("======================= Done =========================\n");
}
