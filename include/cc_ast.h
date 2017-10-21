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
typedef struct AST_CommandHeader AST_CommandHeader;
typedef struct AST_ExprHeader AST_ExprHeader;
typedef struct AST_Identifier AST_Identifier;
typedef struct AST_Literal AST_Literal;

typedef struct AST_ExprHeader {
    int type;
    struct AST_ExprHeader *next;
} AST_ExprHeader;

typedef struct AST_Identifier {
    AST_ExprHeader    header;
    comp_dict_item_t *entry;
} AST_Identifier;

typedef struct AST_Function {
    int type;
    AST_CommandHeader *first_command;
    AST_Identifier      *identifier;
    struct AST_Function *next;
} AST_Function;

static AST_Function *ast_function_make(AST_Identifier *id) {
    AST_Function *f = calloc(1, sizeof(*f));
    f->type = AST_FUNCAO;
    f->identifier = id;
    return f;
}

static void ast_function_free(AST_Function *f) {
    free(f);
}

typedef struct AST_Program {
    int type;
    AST_Function *first_func;
} AST_Program;

static AST_Program *ast_program_make() {
    AST_Program *p = calloc(1, sizeof(*p));
    p->type = AST_PROGRAMA;
    return p;
}

static void ast_program_free(AST_Program *p) {
    free(p);
}

/* ------------------------------------------------
 * Commands
 * ------------------------------------------------ */
typedef struct AST_CommandHeader {
    int type;
    struct AST_CommandHeader *next;
} AST_CommandHeader ;

typedef struct AST_IfElse {
    AST_CommandHeader     header;
    AST_ExprHeader       *condition;
    AST_CommandHeader    *then_command;
    AST_CommandHeader    *else_command;
} AST_IfElse;

static AST_CommandHeader *ast_if_make(AST_ExprHeader *cond, AST_CommandHeader *then_command,
				      AST_CommandHeader *else_command) {
    AST_IfElse *i = calloc(1, sizeof(*i));
    i->header.type = AST_IF_ELSE;
    i->condition = cond;
    i->then_command = then_command;
    i->else_command = else_command;
    return &i->header;
}

typedef struct AST_Shift {
    AST_CommandHeader    header;
    AST_Identifier      *identifier;
    AST_Literal         *number;
    bool shift_right;
} AST_Shift;

static AST_CommandHeader *ast_shift_make(AST_Identifier *id, AST_Literal *number, bool shift_right) {
    AST_Shift *s = calloc(1, sizeof(*s));
    s->header.type = (shift_right) ? AST_SHIFT_RIGHT : AST_SHIFT_LEFT;
    s->identifier = id;
    s->number = number;
    return &s->header;
}

typedef struct AST_While {
    AST_CommandHeader  header;
    AST_ExprHeader    *condition;
    AST_CommandHeader *first_command;
    bool               is_do_while;
} AST_While;

static AST_CommandHeader *ast_while_make(AST_ExprHeader *cond, AST_CommandHeader *first_command) {
    AST_While *w = calloc(1, sizeof(*w));
    w->header.type = AST_WHILE_DO;
    w->condition = cond;
    w->first_command = first_command;
    return &w->header;
}

static void ast_while_free(AST_While *w) {
    free(w);
}

typedef struct AST_Assignment {
    AST_CommandHeader  header;
    // This field is NULL when is_user_type_assignment is false
    AST_Identifier    *user_type_identifier;
    AST_ExprHeader    *identifier;
    AST_ExprHeader    *expr;

    bool is_user_type_assignment;
} AST_Assignment;

static AST_CommandHeader *ast_assignment_make(AST_ExprHeader *id, AST_ExprHeader *expr) {
    AST_Assignment *a = calloc(1, sizeof(*a));
    a->header.type = AST_ATRIBUICAO;
    a->identifier = id;
    a->expr = expr;
    return &a->header;
}

static AST_CommandHeader *ast_assignment_user_type_make(AST_Identifier *user_type_id,
							AST_ExprHeader *id, AST_ExprHeader *expr) {
    AST_Assignment *a = calloc(1, sizeof(*a));
    a->header.type = AST_ATRIBUICAO;
    a->is_user_type_assignment = true;
    a->user_type_identifier = user_type_id;
    a->identifier = id;
    a->expr = expr;
    return &a->header;
}

typedef struct AST_Return {
    AST_CommandHeader header;
    AST_ExprHeader   *expr;
} AST_Return;

static AST_CommandHeader *ast_return_make(AST_ExprHeader *expr) {
    AST_Return *r = calloc(1, sizeof(*r));
    r->header.type = AST_RETURN;
    r->expr = expr;
    return &r->header;
}

typedef struct AST_Block {
    AST_CommandHeader header;
    AST_CommandHeader *first_command;
} AST_Block;

static AST_CommandHeader *ast_block_make(AST_CommandHeader *cmd) {
    AST_Block *b = calloc(1, sizeof(*b));
    b->header.type = AST_BLOCO;
    b->first_command = cmd;
    return &b->header;
}

static void ast_block_free(AST_Block *b) {
    free(b);
}

typedef struct AST_Break {
    AST_CommandHeader header;
} AST_Break;

static AST_CommandHeader *ast_break_make() {
    AST_Break *b = calloc(1, sizeof(*b));
    b->header.type = AST_BREAK;
    return &b->header;
}

static void ast_break_free(AST_CommandHeader *b) {
    free(b);
}

typedef struct AST_Continue {
    AST_CommandHeader header;
} AST_Continue;

static AST_CommandHeader *ast_continue_make() {
    AST_Continue *c = calloc(1, sizeof(*c));
    c->header.type = AST_CONTINUE;
    return &c->header;
}

static void ast_continue_free(AST_CommandHeader *c) {
    free(c);
}


/* ------------------------------------------------
 * Expressions
 * ------------------------------------------------ */


static AST_ExprHeader *ast_identifier_make(comp_dict_item_t *entry) {
    AST_Identifier *i = calloc(1, sizeof(*i));
    i->header.type = AST_IDENTIFICADOR;
    i->entry = entry;
    return &i->header;
}

static void ast_identifier_free(AST_Identifier *i) {
    free(i);
}

typedef struct AST_Literal {
    AST_ExprHeader    header;
    comp_dict_item_t *entry;
} AST_Literal;

static AST_ExprHeader *ast_literal_make(comp_dict_item_t *entry) {
    AST_Literal *l = calloc(1, sizeof(*l));
    l->header.type = AST_LITERAL;
    l->entry = entry;
    return &l->header;
}

typedef struct AST_AritExpr {
    AST_ExprHeader   header;
    AST_ExprHeader  *first;
    AST_ExprHeader  *second;
} AST_AritExpr;

static AST_ExprHeader *ast_arit_expr_make(int op, AST_ExprHeader *lhs, AST_ExprHeader *rhs) {
    AST_AritExpr *a = calloc(1, sizeof(*a));
    a->header.type = op;
    a->first = lhs;
    a->second = rhs;
    return &a->header;
}

typedef struct AST_LogicExpr {
    AST_ExprHeader  header;
    AST_ExprHeader  *first;
    AST_ExprHeader  *second;
} AST_LogicExpr;

static AST_ExprHeader *ast_logic_expr_make(int op, AST_ExprHeader *lhs, AST_ExprHeader *rhs) {
    AST_LogicExpr *a = calloc(1, sizeof(*a));
    a->header.type = op;
    a->first = lhs;
    a->second = rhs;
    return &a->header;
}

typedef struct AST_IndexedVector {
    AST_ExprHeader  header;
    AST_Identifier *identifier;
    AST_ExprHeader *expr;
} AST_IndexedVector;

static AST_ExprHeader *ast_indexed_vector_make(comp_dict_item_t *entry, AST_ExprHeader *expr) {
    AST_IndexedVector *iv = calloc(1, sizeof(*iv));
    iv->header.type = AST_VETOR_INDEXADO;
    iv->identifier = (AST_Identifier*)ast_identifier_make(entry);
    iv->expr = expr;
    return &iv->header;
}

static void ast_expr_free(AST_ExprHeader *expr) {
    switch (expr->type) {
    case AST_IDENTIFICADOR: ast_identifier_free((AST_Identifier*)expr); break;
    default: Assert(false);
    }
}

static void ast_indexed_vector_free(AST_IndexedVector *iv) {
    ast_identifier_free(iv->identifier);
    ast_expr_free(iv->expr);
    free(iv);
}

typedef struct AST_FunctionCall {
    AST_ExprHeader  header;
    AST_Identifier *identifier;
    AST_ExprHeader *first_param;
} AST_FunctionCall;

static AST_ExprHeader *ast_function_call_make(comp_dict_item_t *entry, AST_ExprHeader *param) {
    AST_FunctionCall *f = calloc(1, sizeof(*f));
    f->header.type = AST_CHAMADA_DE_FUNCAO;
    f->identifier = (AST_Identifier*)ast_identifier_make(entry);
    f->first_param = param;
    return &f->header;
}

#endif
