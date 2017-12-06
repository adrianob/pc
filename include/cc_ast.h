/*
  cc_ast.h

  Este arquivo contém as constantes para os tipos dos nós da AST.
*/
#ifndef __CC_AST_H
#define __CC_AST_H

#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include "macros.h"
#include "cc_dict.h"
#include "enums.h"
#include "table_symbol.h"

#ifndef AST_TYPES
#define AST_TYPES						\
        AST_TYPE(AST_PROGRAMA,            "Program"),		\
	AST_TYPE(AST_FUNCAO,              "Function"),		\
	AST_TYPE(AST_IF_ELSE,             "If Else"),		\
	AST_TYPE(AST_DO_WHILE,            "Do While"),		\
	AST_TYPE(AST_WHILE_DO,            "While Do"),		\
	AST_TYPE(AST_INPUT,               "Input"),	        \
	AST_TYPE(AST_OUTPUT,              "Output"),		\
	AST_TYPE(AST_ATRIBUICAO,          "Assignment"),	\
	AST_TYPE(AST_RETURN,              "Return"),		\
	AST_TYPE(AST_BLOCO,               "Block"),		\
	AST_TYPE(AST_BREAK,               "Break"),		\
	AST_TYPE(AST_CONTINUE,            "Continue"),	        \
	AST_TYPE(AST_CASE,                "Case"),		\
	AST_TYPE(AST_FOR,                 "For"),		\
	AST_TYPE(AST_FOREACH,             "Foreach"),		\
	AST_TYPE(AST_SWITCH,              "Switch"),	        \
	AST_TYPE(AST_IDENTIFICADOR,       "Identifier"),	\
	AST_TYPE(AST_LITERAL,             "Literal"),		\
	AST_TYPE(AST_ARIM_SOMA,           "+"),			\
	AST_TYPE(AST_ARIM_SUBTRACAO,      "-"),			\
	AST_TYPE(AST_ARIM_MULTIPLICACAO,  "*"),			\
	AST_TYPE(AST_ARIM_DIVISAO,        "/"),			\
	AST_TYPE(AST_ARIM_INVERSAO,       "Inversion"),		\
	AST_TYPE(AST_LOGICO_E,            "&&"),		\
	AST_TYPE(AST_LOGICO_OU,           "||"),		\
	AST_TYPE(AST_LOGICO_COMP_DIF,     "!="),		\
	AST_TYPE(AST_LOGICO_COMP_IGUAL,   "=="),		\
	AST_TYPE(AST_LOGICO_COMP_LE,      "<="),		\
	AST_TYPE(AST_LOGICO_COMP_GE,      ">="),		\
	AST_TYPE(AST_LOGICO_COMP_L,       "<"),			\
	AST_TYPE(AST_LOGICO_COMP_G,       ">"),			\
	AST_TYPE(AST_LOGICO_COMP_NEGACAO, "!"),			\
	AST_TYPE(AST_VETOR_INDEXADO,      "Indexed Vector"),	\
	AST_TYPE(AST_CHAMADA_DE_FUNCAO,   "Function Call"),	\
	AST_TYPE(AST_SHIFT_RIGHT,         "<<"),		\
	AST_TYPE(AST_SHIFT_LEFT,          ">>"),
#endif

enum {
#define AST_TYPE(t, s) t
    AST_TYPES
#undef AST_TYPE
};

static char *g_ast_names[] = {
#define AST_TYPE(t, s) s
    AST_TYPES
#undef AST_TYPE
};

typedef struct comp_dict_item comp_dict_item_t;
typedef struct AST_Header AST_Header;
typedef struct AST_Identifier AST_Identifier;
typedef struct AST_Literal AST_Literal;
typedef struct Scope Scope;

typedef struct AST_Header {
    int                type;
    IKS_Type           semantic_type;
    IKS_Type           coertion_to;
    struct AST_Header *next;
} AST_Header;

void ast_header_free(AST_Header *header);

typedef struct AST_Identifier {
    AST_Header        header;
    comp_dict_item_t *entry;
} AST_Identifier;

typedef struct AST_Function {
    AST_Header           header;
    IKS_Type             return_type;
    AST_Identifier      *return_identifier;
    AST_Identifier      *identifier;
    AST_Header          *first_command;
    Scope               *scope;
    struct AST_Function *next;
} AST_Function;

AST_Function *ast_function_make(AST_Identifier *id, AST_Header *first_command,
                                IKS_Type return_type, AST_Identifier *return_identifier);
void ast_function_free(AST_Function *f);

typedef struct AST_Program {
    int type;
    AST_Function *first_func;
    Scope        *scope;
} AST_Program;

AST_Program *ast_program_make();
void ast_program_free(AST_Program *p);

void ast_generate_code(AST_Program *program);

/* ------------------------------------------------
 * Commands
 * ------------------------------------------------ */
typedef struct AST_IfElse {
    AST_Header     header;
    AST_Header    *condition;
    AST_Header    *then_command;
    Scope         *then_scope;
    AST_Header    *else_command;
    Scope         *else_scope;
} AST_IfElse;

AST_Header *ast_if_make(AST_Header *cond, AST_Header *then_command, AST_Header *else_command);
void ast_if_free(AST_IfElse *i);

typedef struct AST_Shift {
    AST_Header           header;
    AST_Identifier      *identifier;
    AST_Literal         *number;
    bool                 shift_right;
} AST_Shift;

AST_Header *ast_shift_make(AST_Identifier *id, AST_Literal *number, bool shift_right);
void ast_shift_free(AST_Shift *s);

typedef struct AST_Case {
    AST_Header     header;
    AST_Literal   *literal;
} AST_Case;

AST_Header *ast_case_make(AST_Literal *lit);
void ast_case_free(AST_Case *c);

typedef struct AST_While {
    AST_Header         header;
    AST_Header        *condition;
    AST_Header        *first_command;
    Scope             *scope;
    bool               is_do_while;
} AST_While;

AST_Header *ast_while_make(AST_Header *cond, AST_Header *first_command, bool is_do_while);
void ast_while_free(AST_While *w);

typedef struct AST_Switch {
    AST_Header     header;
    AST_Header    *condition;
    AST_Header    *first_command;
} AST_Switch;

AST_Header *ast_switch_make(AST_Header *cond, AST_Header *first_command);
void ast_switch_free(AST_Switch *s);

typedef struct AST_Input {
    AST_Header     header;
    AST_Header    *expr;
} AST_Input;

AST_Header *ast_input_make(AST_Header *expr);
void ast_input_free(AST_Input *i);

typedef struct AST_For {
    AST_Header         header;
    AST_Header        *first_command;
    AST_Header        *list_first_command;
    AST_Header        *second_list_first_command;
    AST_Header        *expr;
    Scope             *scope;
} AST_For;

AST_Header *ast_for_make(AST_Header *expr, AST_Header *first_command, AST_Header *list_first_command,
			 AST_Header *second_list_first_command);
void ast_for_free(AST_For *f);

typedef struct AST_Foreach {
    AST_Header         header;
    AST_Identifier    *identifier;
    AST_Header        *first_command;
    AST_Header        *expr;
    Scope             *scope;
} AST_Foreach;

AST_Header *ast_foreach_make(AST_Identifier *identifier, AST_Header *expr, AST_Header *first_command);
void ast_foreach_free(AST_Foreach *f);

typedef struct AST_Output {
    AST_Header   header;
    AST_Header  *expr;
} AST_Output;

AST_Header *ast_output_make(AST_Header *expr);
void ast_output_free(AST_Output *o);

typedef struct AST_Assignment {
    AST_Header         header;
    // This field is NULL when is_user_type_assignment is false
    AST_Identifier    *user_type_identifier;
    AST_Header        *identifier;
    AST_Header        *expr;
    bool               is_user_type_assignment;
} AST_Assignment;

AST_Header *ast_assignment_make(AST_Header *id, AST_Header *expr);
AST_Header *ast_assignment_user_type_make(AST_Identifier *user_type_id, AST_Header *id, AST_Header *expr);
void ast_assignment_free(AST_Assignment *a);

typedef struct AST_Return {
    AST_Header    header;
    AST_Header   *expr;
} AST_Return;

AST_Header *ast_return_make(AST_Header *expr);
void ast_return_free(AST_Return *r);

typedef struct AST_Block {
    AST_Header   header;
    AST_Header  *first_command;
    Scope       *scope;
} AST_Block;

AST_Header *ast_block_make(AST_Header *cmd);
void ast_block_free(AST_Block *b);

typedef struct AST_Break {
    AST_Header header;
} AST_Break;

AST_Header *ast_break_make();
void ast_break_free(AST_Break *b);

typedef struct AST_Continue {
    AST_Header header;
} AST_Continue;

AST_Header *ast_continue_make();
void ast_continue_free(AST_Continue *c);

/* ------------------------------------------------
 * Expressions
 * ------------------------------------------------ */

AST_Header *ast_identifier_make(comp_dict_item_t *entry);
void ast_identifier_free(AST_Identifier *i);

typedef struct AST_Literal {
    AST_Header        header;
    comp_dict_item_t *entry;
} AST_Literal;

AST_Header *ast_literal_make(comp_dict_item_t *entry, IKS_Type semantic_type);
void ast_literal_free(AST_Literal *l);

typedef struct AST_AritExpr {
    AST_Header   header;
    AST_Header  *first;
    AST_Header  *second;
} AST_AritExpr;

AST_Header *ast_arit_expr_make(int op, AST_Header *lhs, AST_Header *rhs);
void ast_arit_expr_free(AST_AritExpr *e);

typedef struct AST_LogicExpr {
    AST_Header   header;
    AST_Header  *first;
    AST_Header  *second;
} AST_LogicExpr;

AST_Header *ast_logic_expr_make(int op, AST_Header *lhs, AST_Header *rhs);
void ast_logic_expr_free(AST_LogicExpr *e);

typedef struct AST_IndexedVector {
    AST_Header      header;
    AST_Identifier *identifier;
    AST_Header     *expr;
} AST_IndexedVector;

AST_Header *ast_indexed_vector_make(AST_Identifier *id, AST_Header *expr);
void ast_indexed_vector_free(AST_IndexedVector *iv);

void ast_expr_free(AST_Header *expr);

typedef struct AST_FunctionCall {
    AST_Header      header;
    AST_Identifier *identifier;
    AST_Header *first_param;
} AST_FunctionCall;

AST_Header *ast_function_call_make(comp_dict_item_t *entry, AST_Header *param);
void ast_function_call_free(AST_FunctionCall *fc);

int find_line_number_from_ast_header(AST_Header *header);

static inline char *get_key_from_identifier(AST_Identifier *id) {
    return ((TableSymbol*)id->entry->value)->value_string_or_ident;
}

static inline int get_line_from_identifier(AST_Identifier *id) {
    return ((TableSymbol*)id->entry->value)->line_number;
}

#endif
