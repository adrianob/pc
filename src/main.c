#include "main.h"
#include "cc_ast.h"
#include "macros.h"

extern AST_Program *g_program;
extern FILE *yyin;
extern char *yytext;
extern int getLineNumber();
#define print_nome(TOKEN)                                                      \
    printf("%d " #TOKEN " [%s]\n", comp_get_line_number(), yytext);
#define print_nome2(TOKEN)                                                     \
    printf("%d TK_ESPECIAL [%c]\n", comp_get_line_number(), TOKEN);
#define USER_INIT main_init(argc, argv);
#define USER_FINALIZE main_finalize();

void main_avaliacao_etapa_1_tabela(void);
int main_avaliacao_etapa_1(int argc, char **argv) {
    int token = 0;
    while ((token = yylex())) {
        if (getenv("INF47_TABLE")) {
            continue;
        }
        switch (token) {
        case ',':
        case ';':
        case ':':
        case '(':
        case ')':
        case '[':
        case ']':
        case '{':
        case '}':
        case '+':
        case '-':
        case '*':
        case '/':
        case '<':
        case '>':
        case '=':
        case '!':
        case '&':
        case '$':
        case '%':
        case '#':
        case '^':
            print_nome2(token);
            break;
        case TK_PR_INT:
            print_nome(TK_PR_INT);
            break;
        case TK_PR_FLOAT:
            print_nome(TK_PR_FLOAT);
            break;
        case TK_PR_BOOL:
            print_nome(TK_PR_BOOL);
            break;
        case TK_PR_CHAR:
            print_nome(TK_PR_CHAR);
            break;
        case TK_PR_STRING:
            print_nome(TK_PR_STRING);
            break;
        case TK_PR_IF:
            print_nome(TK_PR_IF);
            break;
        case TK_PR_THEN:
            print_nome(TK_PR_THEN);
            break;
        case TK_PR_ELSE:
            print_nome(TK_PR_ELSE);
            break;
        case TK_PR_WHILE:
            print_nome(TK_PR_WHILE);
            break;
        case TK_PR_DO:
            print_nome(TK_PR_DO);
            break;
        case TK_PR_INPUT:
            print_nome(TK_PR_INPUT);
            break;
        case TK_PR_OUTPUT:
            print_nome(TK_PR_OUTPUT);
            break;
        case TK_PR_RETURN:
            print_nome(TK_PR_RETURN);
            break;
        case TK_PR_CONST:
            print_nome(TK_PR_CONST);
            break;
        case TK_PR_STATIC:
            print_nome(TK_PR_STATIC);
            break;
        case TK_PR_FOREACH:
            print_nome(TK_PR_FOREACH);
            break;
        case TK_PR_FOR:
            print_nome(TK_PR_FOR);
            break;
        case TK_PR_SWITCH:
            print_nome(TK_PR_SWITCH);
            break;
        case TK_PR_CASE:
            print_nome(TK_PR_CASE);
            break;
        case TK_PR_BREAK:
            print_nome(TK_PR_BREAK);
            break;
        case TK_PR_CONTINUE:
            print_nome(TK_PR_CONTINUE);
            break;
        case TK_PR_CLASS:
            print_nome(TK_PR_CLASS);
            break;
        case TK_PR_PRIVATE:
            print_nome(TK_PR_PRIVATE);
            break;
        case TK_PR_PUBLIC:
            print_nome(TK_PR_PUBLIC);
            break;
        case TK_PR_PROTECTED:
            print_nome(TK_PR_PROTECTED);
            break;
        case TK_OC_LE:
            print_nome(TK_OC_LE);
            break;
        case TK_OC_GE:
            print_nome(TK_OC_GE);
            break;
        case TK_OC_EQ:
            print_nome(TK_OC_EQ);
            break;
        case TK_OC_NE:
            print_nome(TK_OC_NE);
            break;
        case TK_OC_AND:
            print_nome(TK_OC_AND);
            break;
        case TK_OC_OR:
            print_nome(TK_OC_OR);
            break;
        case TK_OC_SL:
            print_nome(TK_OC_SL);
            break;
        case TK_OC_SR:
            print_nome(TK_OC_SR);
            break;
        case TK_LIT_INT:
            print_nome(TK_LIT_INT);
            break;
        case TK_LIT_FLOAT:
            print_nome(TK_LIT_FLOAT);
            break;
        case TK_LIT_FALSE:
            print_nome(TK_LIT_FALSE);
            break;
        case TK_LIT_TRUE:
            print_nome(TK_LIT_TRUE);
            break;
        case TK_LIT_CHAR:
            print_nome(TK_LIT_CHAR);
            break;
        case TK_LIT_STRING:
            print_nome(TK_LIT_STRING);
            break;
        case TK_IDENTIFICADOR:
            print_nome(TK_IDENTIFICADOR);
            break;
        case TOKEN_ERRO:
            print_nome(TOKEN_ERRO);
            break;
        default:
            printf("<Invalid Token with code %d>\n", token);
            return 1;
        }
    }
    if (getenv("INF47_TABLE")) {
        main_avaliacao_etapa_1_tabela();
    }
    return 0;
}

void cc_dict_etapa_1_print_entrada(char *token, int line) {
    printf("%d [%s]\n", line, token);
}

void cc_dict_etapa_2_print_entrada(char *token, int line, int tipo) {
    printf("%d [%s] %d\n", line, token, tipo);
}

void main_avaliacao_etapa_1_tabela(void) { comp_print_table(); }

int main_avaliacao_etapa_2(int argc, char **argv) {
    int ret = yyparse();
    /*comp_print_table();*/
    return ret;
}

void print_expression_to_graph(void *parent, AST_Header *expr) {
    switch (expr->type) {
    case AST_IDENTIFICADOR: {
        AST_Identifier *ident = (AST_Identifier *)expr;
        gv_declare(expr->type, expr, (const char *)ident->entry->key);
        gv_connect(parent, expr);
    } break;
    case AST_LITERAL: {
        AST_Literal *literal = (AST_Literal *)expr;
        gv_declare(expr->type, expr, (const char *)literal->entry->key);
        gv_connect(parent, expr);
    } break;
    case AST_ARIM_SUBTRACAO:
    case AST_ARIM_MULTIPLICACAO:
    case AST_ARIM_DIVISAO:
    case AST_ARIM_INVERSAO:
    case AST_ARIM_SOMA: {
        gv_declare(expr->type, expr, NULL);
        gv_connect(parent, expr);
        AST_AritExpr * express = (AST_AritExpr *)expr;
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
        AST_LogicExpr * express = (AST_LogicExpr *)expr;
        print_expression_to_graph(expr, express->first);
        print_expression_to_graph(expr, express->second);
    } break;
    default:
        Assert(false);
    }
}

void print_command_to_graph(void *parent, AST_Header *cmd) {
    gv_declare(cmd->type, cmd, NULL);
    gv_connect(parent, cmd);
    switch (cmd->type) {
    case AST_IF_ELSE: {
        AST_IfElse *if_else = (AST_IfElse *)cmd;

        print_expression_to_graph(cmd, if_else->condition);
        print_command_to_graph(cmd, if_else->then_command);
        if(if_else->else_command){
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
        print_command_to_graph(cmd, block->first_command);
    } break;
    case AST_INPUT: {
        AST_Input *in = (AST_Input *)cmd;
        print_expression_to_graph(cmd, in->expr);
    } break;
    case AST_OUTPUT: {
        /*@TODO fix multiple expressions*/
        AST_Input *in = (AST_Input *)cmd;
        print_expression_to_graph(cmd, in->expr);
    } break;
    case AST_CASE:
        break;
    case AST_FOR:
        break;
    case AST_FOREACH:
        break;
    case AST_SWITCH: {
        AST_Switch *s = (AST_Switch *)cmd;
        print_expression_to_graph(cmd, s->condition);
        print_command_to_graph(cmd, s->first_command);
    } break;
    case AST_CHAMADA_DE_FUNCAO: {
        AST_FunctionCall *funcall = (AST_FunctionCall *)cmd;
        print_expression_to_graph(cmd, &funcall->identifier->header);
        print_expression_to_graph(cmd, funcall->first_param);
    } break;
    // @Todo: Other AST types
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
        assert(func->type == AST_FUNCAO);

        // Start printing list of commands from the function
        gv_declare(func->type, func, func->identifier->entry->key);
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

int main_avaliacao_etapa_3(int argc, char **argv) {
    gv_init(NULL);
    int ret = yyparse();
    print_ast_to_graph(g_program);
    gv_close();

    /* int num_funcs = 0; */
    /* AST_Function *func = g_program->first_func; */
    /* while (func) { */
    /*     ++num_funcs; */
    /*     func = func->next; */
    /* } */
    /* printf("Number of functions declared: %d\n", num_funcs); */

    return ret;
}

int main_avaliacao_etapa_4(int argc, char **argv) {
    return main_avaliacao_etapa_2(argc, argv);
}

int main_avaliacao_etapa_5(int argc, char **argv) {
    return main_avaliacao_etapa_2(argc, argv);
}

int main_avaliacao_etapa_6(int argc, char **argv) {
    return main_avaliacao_etapa_2(argc, argv);
}

int main_avaliacao_etapa_7(int argc, char **argv) {
    return main_avaliacao_etapa_2(argc, argv);
}

int main(int argc, char **argv) {
    // if some argument is provided, treat it as input
    if (argc != 1) {
        yyin = fopen(argv[1], "r");
        // if fopen fails, yyin continues to be stdin
        if (yyin == NULL) {
            yyin = stdin;
        }
    }
    USER_INIT;
    int r;
#ifdef AVALIACAO_ETAPA_1
    r = main_avaliacao_etapa_1(argc, argv);
#elif AVALIACAO_ETAPA_2
    r = main_avaliacao_etapa_2(argc, argv);
#elif AVALIACAO_ETAPA_3
    r = main_avaliacao_etapa_3(argc, argv);
#elif AVALIACAO_ETAPA_4
    r = main_avaliacao_etapa_4(argc, argv);
#elif AVALIACAO_ETAPA_5
    r = main_avaliacao_etapa_5(argc, argv);
#elif AVALIACAO_ETAPA_6
    r = main_avaliacao_etapa_6(argc, argv);
#elif AVALIACAO_ETAPA_7
    r = main_avaliacao_etapa_7(argc, argv);
#else
    r = 0;
#endif
    USER_FINALIZE;
    return r;
}
