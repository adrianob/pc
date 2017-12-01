#include "cc_ast.h"
#include "macros.h"

AST_Header *ast_shift_make(AST_Identifier *id, AST_Literal *number, bool shift_right) {
    AST_Shift *s = calloc(1, sizeof(*s));
    s->header.type = (shift_right) ? AST_SHIFT_RIGHT : AST_SHIFT_LEFT;
    // @Assumption(Leo): Shift type should always be int.
    s->header.semantic_type = IKS_INT;
    s->identifier = id;
    s->number = number;
    return &s->header;
}

void ast_shift_free(AST_Shift *s) {
    if (!s) return;
    ast_identifier_free(s->identifier);
    ast_literal_free(s->number);
    free(s);
}

AST_Program *ast_program_make() {
    AST_Program *p = calloc(1, sizeof(*p));
    p->type = AST_PROGRAMA;
    return p;
}

void ast_header_free(AST_Header *header) {
    AST_Header *search_header = header;

    while (search_header) {
        AST_Header *header_to_delete = search_header;
        search_header = search_header->next;
	    
        switch (header_to_delete->type) {
        case AST_IF_ELSE:              ast_if_free((AST_IfElse*)header_to_delete); break;
        case AST_WHILE_DO:
        case AST_DO_WHILE:             ast_while_free((AST_While*)header_to_delete); break;
        case AST_INPUT:                ast_input_free((AST_Input*)header_to_delete); break;
        case AST_OUTPUT:               ast_output_free((AST_Output*)header_to_delete); break;
        case AST_ATRIBUICAO:           ast_assignment_free((AST_Assignment*)header_to_delete); break;
        case AST_RETURN:               ast_return_free((AST_Return*)header_to_delete); break;
        case AST_BLOCO:                ast_block_free((AST_Block*)header_to_delete); break;
        case AST_BREAK:                ast_break_free((AST_Break*)header_to_delete); break;
        case AST_CONTINUE:             ast_continue_free((AST_Continue*)header_to_delete); break;
        case AST_CASE:                 ast_case_free((AST_Case*)header_to_delete); break;
        case AST_FOR:                  ast_for_free((AST_For*)header_to_delete); break;
        case AST_FOREACH:              ast_foreach_free((AST_Foreach*)header_to_delete); break;
        case AST_SWITCH:               ast_switch_free((AST_Switch*)header_to_delete); break;
        case AST_IDENTIFICADOR:        ast_identifier_free((AST_Identifier*)header_to_delete); break;
        case AST_LITERAL:              ast_literal_free((AST_Literal*)header_to_delete); break;
        case AST_ARIM_SOMA:
        case AST_ARIM_SUBTRACAO:
        case AST_ARIM_MULTIPLICACAO:
        case AST_ARIM_DIVISAO:
        case AST_ARIM_INVERSAO:        ast_arit_expr_free((AST_AritExpr*)header_to_delete); break;
        case AST_LOGICO_E:
        case AST_LOGICO_OU:
        case AST_LOGICO_COMP_DIF:
        case AST_LOGICO_COMP_IGUAL:
        case AST_LOGICO_COMP_LE:
        case AST_LOGICO_COMP_GE:
        case AST_LOGICO_COMP_L:
        case AST_LOGICO_COMP_G:
        case AST_LOGICO_COMP_NEGACAO:  ast_logic_expr_free((AST_LogicExpr*)header_to_delete); break;
        case AST_VETOR_INDEXADO:       ast_indexed_vector_free((AST_IndexedVector*)header_to_delete); break;
        case AST_CHAMADA_DE_FUNCAO:    ast_function_call_free((AST_FunctionCall*)header_to_delete); break;
        case AST_SHIFT_RIGHT:
        case AST_SHIFT_LEFT:           ast_shift_free((AST_Shift*)header_to_delete); break;
        default: Assert(false);
        }
    }
}

void ast_program_free(AST_Program *program) {
    if (!program) return;

    AST_Function *func = program->first_func;
    while (func) {
        AST_Function *func_to_delete = func;
        func = func->next;

        ast_function_free(func_to_delete);
    }
}

AST_Function *ast_function_make(AST_Identifier *id, AST_Header *first_command,
                                IKS_Type return_type, AST_Identifier *return_identifier) {
    AST_Function *f = calloc(1, sizeof(*f));
    f->header.type = AST_FUNCAO;
    f->return_type = return_type;
    f->return_identifier = return_identifier;
    f->identifier = id;
    f->first_command = first_command;
    return f;
}

void ast_function_free(AST_Function *f) {
    if (!f) return;
    ast_identifier_free(f->identifier);
    ast_header_free(f->first_command);
    free(f);
}

AST_Header *ast_if_make(AST_Header *cond, AST_Header *then_command, AST_Header *else_command) {
    AST_IfElse *i = calloc(1, sizeof(*i));
    i->header.type = AST_IF_ELSE;
    i->condition = cond;
    i->then_command = then_command;
    i->else_command = else_command;
    return &i->header;
}

void ast_if_free(AST_IfElse *i) {
    if (!i) return;
    ast_header_free(i->condition);
    ast_header_free(i->then_command);
    ast_header_free(i->else_command);
    free(i);
}

AST_Header *ast_while_make(AST_Header *cond, AST_Header *first_command, bool is_do_while) {
    AST_While *w = calloc(1, sizeof(*w));
    w->header.type = (is_do_while) ? AST_DO_WHILE : AST_WHILE_DO;
    w->condition = cond;
    w->first_command = first_command;
    return &w->header;
}

void ast_while_free(AST_While *w) {
    if (!w) return;
    ast_header_free(w->condition);
    ast_header_free(w->first_command);
    free(w);
}

AST_Header *ast_switch_make(AST_Header *cond, AST_Header *first_command) {
    AST_Switch *s = calloc(1, sizeof(*s));
    s->header.type = AST_SWITCH;
    s->condition = cond;
    s->first_command = first_command;
    return &s->header;
}

void ast_switch_free(AST_Switch *s) {
    if (!s) return;
    ast_header_free(s->condition);
    ast_header_free(s->first_command);
    free(s);
}

AST_Header *ast_input_make(AST_Header *expr) {
    AST_Input *i = calloc(1, sizeof(*i));
    i->header.type = AST_INPUT;
    i->expr = expr;
    return &i->header;
}

void ast_input_free(AST_Input *i) {
    if (!i) return;
    ast_header_free(i->expr);
    free(i);
}

AST_Header *ast_for_make(AST_Header *expr, AST_Header *first_command, AST_Header *list_first_command,
			 AST_Header *second_list_first_command) {
    AST_For *o = calloc(1, sizeof(*o));
    o->header.type = AST_FOR;
    o->expr = expr;
    o->first_command = first_command;
    o->list_first_command = list_first_command;
    o->second_list_first_command = second_list_first_command;
    return &o->header;
}

void ast_for_free(AST_For *f) {
    if (!f) return;
    ast_header_free(f->first_command);
    ast_header_free(f->list_first_command);
    ast_header_free(f->second_list_first_command);
    ast_header_free(f->expr);
}

AST_Header *ast_foreach_make(AST_Identifier *identifier, AST_Header *expr, AST_Header *first_command) {
    AST_Foreach *o = calloc(1, sizeof(*o));
    o->header.type = AST_FOREACH;
    o->expr = expr;
    o->identifier = identifier;
    o->first_command = first_command;
    return &o->header;
}

void ast_foreach_free(AST_Foreach *f) {
    if (!f) return;
    ast_identifier_free(f->identifier);
    ast_header_free(f->first_command);
    ast_header_free(f->expr);
    free(f);
}

AST_Header *ast_case_make(AST_Literal *lit) {
    AST_Case *c = calloc(1, sizeof(*c));
    c->header.type = AST_CASE;
    c->literal = lit;
    return &c->header;
}

void ast_case_free(AST_Case *c) {
    if (!c) return;
    ast_literal_free(c->literal);
    free(c);
}

AST_Header *ast_output_make(AST_Header *expr) {
    AST_Output *o = calloc(1, sizeof(*o));
    o->header.type = AST_OUTPUT;
    o->expr = expr;
    return &o->header;
}

void ast_output_free(AST_Output *o) {
    if (!o) return;
    ast_header_free(o->expr);
    free(o);
}

AST_Header *ast_assignment_make(AST_Header *id, AST_Header *expr) {
    AST_Assignment *a = calloc(1, sizeof(*a));
    a->header.type = AST_ATRIBUICAO;
    a->identifier = id;
    a->expr = expr;
    return &a->header;
}

void ast_assignment_free(AST_Assignment *a) {
    if (!a) return;
    ast_identifier_free(a->user_type_identifier);
    ast_header_free(a->identifier);
    ast_header_free(a->expr);
    free(a);
}

AST_Header *ast_assignment_user_type_make(AST_Identifier *user_type_id, AST_Header *id, AST_Header *expr) {
    AST_Assignment *a = calloc(1, sizeof(*a));
    a->header.type = AST_ATRIBUICAO;
    a->is_user_type_assignment = true;
    a->user_type_identifier = user_type_id;
    a->identifier = id;
    a->expr = expr;
    return &a->header;
}

AST_Header *ast_return_make(AST_Header *expr) {
    AST_Return *r = calloc(1, sizeof(*r));
    r->header.type = AST_RETURN;
    r->expr = expr;
    return &r->header;
}

void ast_return_free(AST_Return *r) {
    if (!r) return;
    ast_header_free(r->expr);
    free(r);
}

AST_Header *ast_block_make(AST_Header *cmd) {
    AST_Block *b = calloc(1, sizeof(*b));
    b->header.type = AST_BLOCO;
    b->first_command = cmd;
    return &b->header;
}

void ast_block_free(AST_Block *b) {
    if (!b) return;
    ast_header_free(b->first_command);
    free(b);
}

AST_Header *ast_break_make() {
    AST_Break *b = calloc(1, sizeof(*b));
    b->header.type = AST_BREAK;
    return &b->header;
}

void ast_break_free(AST_Break *b) {
    free(b);
}

AST_Header *ast_continue_make() {
    AST_Continue *c = calloc(1, sizeof(*c));
    c->header.type = AST_CONTINUE;
    return &c->header;
}

void ast_continue_free(AST_Continue *c) {
    free(c);
}

AST_Header *ast_identifier_make(comp_dict_item_t *entry) {
    AST_Identifier *i = calloc(1, sizeof(*i));
    i->header.type = AST_IDENTIFICADOR;
    i->entry = entry;
    return &i->header;
}

void ast_identifier_free(AST_Identifier *i) {
    free(i);
}

AST_Header *ast_literal_make(comp_dict_item_t *entry, IKS_Type semantic_type) {
    AST_Literal *l = calloc(1, sizeof(*l));
    l->header.type = AST_LITERAL;
    l->header.semantic_type = semantic_type;
    l->entry = entry;
    return &l->header;
}

void ast_literal_free(AST_Literal *l) {
    free(l);
}

AST_Header *ast_arit_expr_make(int op, AST_Header *lhs, AST_Header *rhs) {
    AST_AritExpr *a = calloc(1, sizeof(*a));
    a->header.type = op;
    a->first = lhs;
    a->second = rhs;
    return &a->header;
}

void ast_arit_expr_free(AST_AritExpr *e) {
    if (!e) return;
    ast_header_free(e->first);
    ast_header_free(e->second);
    free(e);
}

AST_Header *ast_logic_expr_make(int op, AST_Header *lhs, AST_Header *rhs) {
    AST_LogicExpr *a = calloc(1, sizeof(*a));
    a->header.type = op;
    a->first = lhs;
    a->second = rhs;
    return &a->header;
}

void ast_logic_expr_free(AST_LogicExpr *e) {
    if (!e) return;
    ast_header_free(e->first);
    ast_header_free(e->second);
    free(e);
}

AST_Header *ast_indexed_vector_make(AST_Identifier *id, AST_Header *expr) {
    AST_IndexedVector *iv = calloc(1, sizeof(*iv));
    iv->header.type = AST_VETOR_INDEXADO;
    iv->identifier = id;
    iv->expr = expr;
    return &iv->header;
}

void ast_indexed_vector_free(AST_IndexedVector *iv) {
    ast_identifier_free(iv->identifier);
    ast_header_free(iv->expr);
    free(iv);
}

AST_Header *ast_function_call_make(comp_dict_item_t *entry, AST_Header *param) {
    AST_FunctionCall *f = calloc(1, sizeof(*f));
    f->header.type = AST_CHAMADA_DE_FUNCAO;
    f->identifier = (AST_Identifier*)ast_identifier_make(entry);
    f->first_param = param;
    return &f->header;
}

void ast_function_call_free(AST_FunctionCall *f) {
    if (!f) return;
    ast_identifier_free(f->identifier);
    ast_header_free(f->first_param);
    free(f);
}
