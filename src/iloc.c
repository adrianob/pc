#include "iloc.h"

ILOC_Instruction *ast_assignment_generate_code(AST_Assignment *assignment, STACK_T *scope_stack);
ILOC_Instruction *ast_expr_generate_code(AST_Header *expr, STACK_T *scope_stack);
ILOC_Instruction *ast_cmd_generate_code(AST_Header *cmd, STACK_T *scope_stack);
ILOC_Instruction *iloc_instruction_concat(ILOC_Instruction *inst, ILOC_Instruction *new_inst);

void iloc_operand_free(const ILOC_Operand *operand) {
    if (operand->type == ILOC_LABEL_REF) {
        sdsfree(operand->label);
    }
}

ILOC_Instruction *iloc_instruction_make(void) {
    ILOC_Instruction *i = calloc(1, sizeof(*i));
    array_init(i->sources);
    array_init(i->targets);
    return i;
}

ILOC_Operand iloc_register_make(ILOC_RegisterType type) {
    ILOC_Operand op = {0};
    op.type = ILOC_REGISTER;
    op.register_type = type;
    op.register_number = (type == ILOC_RT_GENERIC) ? get_next_register_number() : -1;
    return op;
}

ILOC_Operand iloc_number_make(int value) {
    ILOC_Operand op = {0};
    op.type = ILOC_NUMBER;
    op.number = value;
    return op;
}

ILOC_Operand iloc_label_ref_make(sds label_name) {
    ILOC_Operand op = {0};
    op.type = ILOC_LABEL_REF;
    op.label = sdsdup(label_name);
    return op;
}

sds label_make() {
    static int number = 0;
    sds label = sdscatprintf(sdsempty(), "L%d", number++);
    return label;
}

void iloc_instruction_free(ILOC_Instruction *inst) {
    if (inst->label) sdsfree(inst->label);

    for (int i = 0; i < array_len(inst->sources); ++i)
        iloc_operand_free(&inst->sources[i]);
    array_free(inst->sources);

    for (int i = 0; i < array_len(inst->targets); ++i)
        iloc_operand_free(&inst->targets[i]);
    array_free(inst->targets);
}

// Function that creates a register if the last intruction points to a literal.
// If the instruction does not point to a literal, it does nothing.
ILOC_Instruction *load_literal_to_register(ILOC_Instruction *code) {
    if (code->opcode == ILOC_NOP) {
        Assert(array_len(code->targets) == 1);

        ILOC_Instruction *reg = iloc_instruction_make();
        reg->opcode = ILOC_LOADI;
        array_push(reg->sources, iloc_number_make(code->targets[0].number));
        array_push(reg->targets, iloc_register_make(ILOC_RT_GENERIC));
        return reg;

        ILOC_Instruction *code_and_reg = NULL;
        code_and_reg = iloc_instruction_concat(code, reg);
        return code_and_reg;
    } else {
        return code;
    }
}

sds iloc_operand_string(const ILOC_Operand *operand) {
    Assert(operand);
    sds name;
    switch (operand->type) {
    case ILOC_NUMBER: 
        name = sdscatprintf(sdsempty(), "%d", operand->number);
        break;
    case ILOC_LABEL_REF:
        name = sdsdup(operand->label);
        break;
    case ILOC_REGISTER:
        if (operand->register_type == ILOC_RT_RARP) {
            name = sdsnew("rarp");
        } else if (operand->register_type == ILOC_RT_RBSS) {
            name = sdsnew("rbss");
        } else if (operand->register_type == ILOC_RT_GENERIC) {
            name = sdscatprintf(sdsempty(), "r%d", operand->register_number);
        } else Assert(false);
        break;
    default: Assert(false);
    }
    return name;
}

ILOC_Instruction *iloc_instruction_concat(ILOC_Instruction *inst, ILOC_Instruction *new_inst) {
    if (inst && new_inst) {
        ILOC_Instruction *last_inst = inst;
        while (last_inst->next) last_inst = last_inst->next;
        
        ILOC_Instruction *last_new_inst = new_inst;
        while (last_new_inst->next) last_new_inst = last_new_inst->next;

        ILOC_Instruction *first_new_inst = new_inst;
        while (first_new_inst->prev) first_new_inst = first_new_inst->prev;

        last_inst->next = first_new_inst;
        first_new_inst->prev = last_inst;

        return last_new_inst;
    } else if (!inst) {
        ILOC_Instruction *last_new_inst = new_inst;
        while (last_new_inst->next) last_new_inst = last_new_inst->next;
        
        return last_new_inst;
    } else if (!new_inst) {
        ILOC_Instruction *last_inst = inst;
        while (last_inst->next) last_inst = last_inst->next;

        return last_inst;
    } else {
        Assert(false);
    }
}

ILOC_OpCode get_non_immediate_logic_expr_opcode(int ast_type) {
    switch (ast_type) {
    case AST_LOGICO_E:          return ILOC_AND;
    case AST_LOGICO_OU:         return ILOC_OR;
    case AST_LOGICO_COMP_DIF:   return ILOC_CMP_NE;
    case AST_LOGICO_COMP_IGUAL: return ILOC_CMP_EQ;
    case AST_LOGICO_COMP_LE:    return ILOC_CMP_LE;
    case AST_LOGICO_COMP_GE:    return ILOC_CMP_GE;
    case AST_LOGICO_COMP_L:     return ILOC_CMP_LT;
    case AST_LOGICO_COMP_G:     return ILOC_CMP_GT;
    // @TODO negação
    default: Assert(false);
    }
}

ILOC_OpCode get_immediate_logic_expr_opcode(int ast_type) {
    switch (ast_type) {
    // @TODO implement short circuit
    case AST_LOGICO_E:          return ILOC_ANDI;
    case AST_LOGICO_OU:         return ILOC_ORI;
    default: Assert(false);
    }
}

ILOC_OpCode get_non_immediate_arit_expr_opcode(int ast_type) {
    switch (ast_type) {
    case AST_ARIM_DIVISAO:       return ILOC_DIV;
    case AST_ARIM_MULTIPLICACAO: return ILOC_MULT;
    case AST_ARIM_SOMA:          return ILOC_ADD;
    case AST_ARIM_SUBTRACAO:     return ILOC_SUB;
    default: Assert(false);
    }
}

ILOC_OpCode get_immediate_arit_expr_opcode(int ast_type) {
    switch (ast_type) {
    case AST_ARIM_DIVISAO:       return ILOC_DIVI;
    case AST_ARIM_MULTIPLICACAO: return ILOC_MULTI;
    case AST_ARIM_SOMA:          return ILOC_ADDI;
    case AST_ARIM_SUBTRACAO:     return ILOC_SUBI;
    default: Assert(false);
    }
}

ILOC_Instruction *logic_expr_generate_code(AST_LogicExpr *expr, STACK_T *scope_stack) {
    ILOC_Instruction *code = NULL;

    ILOC_Instruction *code_expr1 = ast_expr_generate_code(expr->first, scope_stack);
    ILOC_Instruction *code_expr2 = ast_expr_generate_code(expr->second, scope_stack);

    Assert(array_len(code_expr1->targets) > 0);
    Assert(array_len(code_expr2->targets) > 0);

    code = iloc_instruction_concat(code, code_expr1);
    code = iloc_instruction_concat(code, code_expr2);

    if (code_expr1->opcode == ILOC_NOP && code_expr2->opcode == ILOC_NOP) {
        // Two literals on the expression

        // Load one literal in one register
        ILOC_Instruction *load = iloc_instruction_make();
        load->opcode = ILOC_LOADI;
        array_push(load->sources, code_expr1->targets[0]);
        array_push(load->targets, iloc_register_make(ILOC_RT_GENERIC));

        code = iloc_instruction_concat(code, load);

        // Apply the immediate instruction with the other literal
        ILOC_Instruction *inst = iloc_instruction_make();
        inst->opcode = get_immediate_logic_expr_opcode(expr->header.type);
        array_push(inst->sources, load->targets[0]);
        array_push(inst->sources, code_expr2->targets[0]);
        array_push(inst->targets, iloc_register_make(ILOC_RT_GENERIC));

        code = iloc_instruction_concat(code, inst);
        return code;
    } else if (code_expr1->opcode != ILOC_NOP && code_expr2->opcode == ILOC_NOP) {
        // First expression is on a register but second is a literal
        // Apply the immediate instruction with the other literal
        ILOC_Instruction *inst = iloc_instruction_make();
        inst->opcode = get_immediate_logic_expr_opcode(expr->header.type);
        array_push(inst->sources, code_expr1->targets[0]);
        array_push(inst->sources, code_expr2->targets[0]);
        array_push(inst->targets, iloc_register_make(ILOC_RT_GENERIC));

        code = iloc_instruction_concat(code, inst);
        return code;
    } else if (code_expr1->opcode == ILOC_NOP && code_expr2->opcode != ILOC_NOP) {
        // Second expression is on a register but first is a literal
        // Apply the immediate instruction with the other literal
        ILOC_Instruction *inst = iloc_instruction_make();
        inst->opcode = get_immediate_logic_expr_opcode(expr->header.type);
        array_push(inst->sources, code_expr2->targets[0]);
        array_push(inst->sources, code_expr1->targets[0]);
        array_push(inst->targets, iloc_register_make(ILOC_RT_GENERIC));

        code = iloc_instruction_concat(code, inst);
        return code;
    } else {
        // The two expressions are on registers
        // Apply the register instruction with the other literal
        ILOC_Instruction *inst = iloc_instruction_make();
        inst->opcode = get_non_immediate_logic_expr_opcode(expr->header.type);
        array_push(inst->sources, code_expr1->targets[0]);
        array_push(inst->sources, code_expr2->targets[0]);
        array_push(inst->targets, iloc_register_make(ILOC_RT_GENERIC));

        code = iloc_instruction_concat(code, inst);
        return code;
    }
}

ILOC_Instruction *arit_expr_generate_code(AST_AritExpr *expr, STACK_T *scope_stack) {
    ILOC_Instruction *code = NULL;

    ILOC_Instruction *code_expr1 = ast_expr_generate_code(expr->first, scope_stack);
    ILOC_Instruction *code_expr2 = ast_expr_generate_code(expr->second, scope_stack);

    Assert(array_len(code_expr1->targets) > 0);
    Assert(array_len(code_expr2->targets) > 0);

    code = iloc_instruction_concat(code, code_expr1);
    code = iloc_instruction_concat(code, code_expr2);

    if (code_expr1->opcode == ILOC_NOP && code_expr2->opcode == ILOC_NOP) {
        // Two literals on the expression

        // Load one literal in one register
        ILOC_Instruction *load = iloc_instruction_make();
        load->opcode = ILOC_LOADI;
        array_push(load->sources, code_expr1->targets[0]);
        array_push(load->targets, iloc_register_make(ILOC_RT_GENERIC));

        code = iloc_instruction_concat(code, load);

        // Apply the immediate instruction with the other literal
        ILOC_Instruction *inst = iloc_instruction_make();
        inst->opcode = get_immediate_arit_expr_opcode(expr->header.type);
        array_push(inst->sources, load->targets[0]);
        array_push(inst->sources, code_expr2->targets[0]);
        array_push(inst->targets, iloc_register_make(ILOC_RT_GENERIC));

        code = iloc_instruction_concat(code, inst);
        return code;
    } else if (code_expr1->opcode != ILOC_NOP && code_expr2->opcode == ILOC_NOP) {
        // First expression is on a register but second is a literal
        // Apply the immediate instruction with the other literal
        ILOC_Instruction *inst = iloc_instruction_make();
        inst->opcode = get_immediate_arit_expr_opcode(expr->header.type);
        array_push(inst->sources, code_expr1->targets[0]);
        array_push(inst->sources, code_expr2->targets[0]);
        array_push(inst->targets, iloc_register_make(ILOC_RT_GENERIC));

        code = iloc_instruction_concat(code, inst);
        return code;
    } else if (code_expr1->opcode == ILOC_NOP && code_expr2->opcode != ILOC_NOP) {
        // Second expression is on a register but first is a literal
        // Apply the immediate instruction with the other literal
        ILOC_Instruction *inst = iloc_instruction_make();
        inst->opcode = get_immediate_arit_expr_opcode(expr->header.type);
        array_push(inst->sources, code_expr2->targets[0]);
        array_push(inst->sources, code_expr1->targets[0]);
        array_push(inst->targets, iloc_register_make(ILOC_RT_GENERIC));

        code = iloc_instruction_concat(code, inst);
        return code;
    } else {
        // The two expressions are on registers
        // Apply the register instruction with the other literal
        ILOC_Instruction *inst = iloc_instruction_make();
        inst->opcode = get_non_immediate_arit_expr_opcode(expr->header.type);
        array_push(inst->sources, code_expr1->targets[0]);
        array_push(inst->sources, code_expr2->targets[0]);
        array_push(inst->targets, iloc_register_make(ILOC_RT_GENERIC));

        code = iloc_instruction_concat(code, inst);
        return code;
    }
}

ILOC_Instruction *ast_assignment_generate_code(AST_Assignment *assignment, STACK_T *scope_stack) {
    /* printf("Generating code for assignment\n"); */
    ILOC_Instruction *code = NULL;

    if (assignment->is_user_type_assignment) {
        // Not implemented yet.
        Assert(false);
    } else {
        Assert(assignment->identifier->type == AST_IDENTIFICADOR  ||
               assignment->identifier->type == AST_VETOR_INDEXADO);

        ILOC_Instruction *expr_code = ast_expr_generate_code(assignment->expr, scope_stack);
        ILOC_Instruction *vec_expr_code = NULL;
        if (assignment->identifier->type == AST_VETOR_INDEXADO){
            vec_expr_code = ast_expr_generate_code(((AST_IndexedVector *)assignment->identifier)->expr,
                                                   scope_stack);
        }
        code = iloc_instruction_concat(code, expr_code);

        bool is_global_scope;
        DeclarationHeader *decl = NULL;
        if(assignment->identifier->type == AST_IDENTIFICADOR){
            decl = scope_find_declaration_recursive(
                (AST_Identifier*)assignment->identifier, scope_stack, &is_global_scope
            );
        } else { //vector assignment
            decl = scope_find_declaration_recursive(
                (AST_Identifier*)((AST_IndexedVector *)assignment->identifier)->identifier, scope_stack, &is_global_scope
            );
        }
        ILOC_Instruction *inst = iloc_instruction_make();
        inst->opcode = ILOC_STOREAI;
        // Sources
        int array_offset = 0;
        if (expr_code->opcode == ILOC_NOP) {
            ILOC_Instruction *load_into_reg = iloc_instruction_make();
            load_into_reg->opcode = ILOC_LOADI;

            array_push(load_into_reg->sources, expr_code->targets[0]);
            array_push(load_into_reg->targets, iloc_register_make(ILOC_RT_GENERIC));

            Assert(load_into_reg->targets[0].register_type == ILOC_RT_GENERIC);
            Assert(load_into_reg->targets[0].register_number != -1);

            code = iloc_instruction_concat(code, load_into_reg);

            array_push(inst->sources, load_into_reg->targets[0]);

            if(assignment->identifier->type == AST_VETOR_INDEXADO) {
                array_offset += vec_expr_code->targets[0].number * ((VectorDeclaration *)decl)->elem_size_in_bytes;
            }

            /* printf("Target number is: %d\n", load_into_reg->targets[0].number); */
        } else {
            array_push(inst->sources, expr_code->targets[0]);
        }
        // Targets
        if (is_global_scope)
            array_push(inst->targets, iloc_register_make(ILOC_RT_RBSS));
        else
            array_push(inst->targets, iloc_register_make(ILOC_RT_RARP));
        int address_offset = declaration_header_get_address_offset(decl) + array_offset;
        array_push(inst->targets, iloc_number_make(address_offset));

        code = iloc_instruction_concat(code, inst);
    }

    return code;
}

ILOC_Instruction *ast_literal_generate_code(AST_Literal *lit) {
    ILOC_Instruction *code = iloc_instruction_make();
    code->opcode = ILOC_NOP;

    TableSymbol *symbol = (TableSymbol*)lit->entry->value;
    switch (symbol->token_type) {
    case POA_LIT_BOOL: array_push(code->targets, iloc_number_make((int)symbol->value_bool)); break;
    case POA_LIT_INT: array_push(code->targets, iloc_number_make(symbol->value_int)); break;
    default: Assert(false);
    }
    
    return code;
}

ILOC_Instruction *ast_identifier_generate_code(AST_Identifier *id, STACK_T *scope_stack) {
    bool is_global_scope;
    DeclarationHeader *decl = scope_find_declaration_recursive(id, scope_stack, &is_global_scope);
    Assert(decl);

    ILOC_Instruction *code = iloc_instruction_make();
    code->opcode = ILOC_LOADAI;
    if (is_global_scope) {
        array_push(code->sources, iloc_register_make(ILOC_RT_RBSS));
    } else {
        array_push(code->sources, iloc_register_make(ILOC_RT_RARP));
    }
    int offset_size = declaration_header_get_address_offset(decl);
    array_push(code->sources, iloc_number_make(offset_size));
    array_push(code->targets, iloc_register_make(ILOC_RT_GENERIC));
    return code;
}

ILOC_Instruction *ast_expr_generate_code(AST_Header *expr, STACK_T *scope_stack) {
    ILOC_Instruction *code = NULL;

    switch (expr->type) {
    case AST_ARIM_INVERSAO:
        Assert(false);
        break;
    case AST_ARIM_DIVISAO:
    case AST_ARIM_MULTIPLICACAO:
    case AST_ARIM_SOMA:
    case AST_ARIM_SUBTRACAO:
        code = arit_expr_generate_code((AST_AritExpr*)expr, scope_stack);
        break;
    /* case AST_ATRIBUICAO: */
    /*     code = ast_assignment_generate_code((AST_Assignment*)expr, scope_stack); */
    /*     break; */
    case AST_LITERAL:
        code = ast_literal_generate_code((AST_Literal*)expr);
        break;
    case AST_IDENTIFICADOR:
        code = ast_identifier_generate_code((AST_Identifier*)expr, scope_stack);
        break;
    case AST_LOGICO_OU:
    case AST_LOGICO_E:
    case AST_LOGICO_COMP_DIF:
    case AST_LOGICO_COMP_IGUAL:
    case AST_LOGICO_COMP_LE:
    case AST_LOGICO_COMP_GE:
    case AST_LOGICO_COMP_L:
    case AST_LOGICO_COMP_G:
        code = logic_expr_generate_code((AST_LogicExpr*)expr, scope_stack);
        break;
    default:
        Assert(false);
    }

    return code;
}

/* ILOC_Instruction *logic_expr_generate_code_labels(AST_LogicExpr *hdr, sds true_label, */
/*                                                   sds false_label, STACK_T *scope_stack) { */
/*     ILOC_Instruction *code = NULL; */
    
/*     return code; */
/* } */

ILOC_Instruction *ast_expr_generate_code_labels(AST_Header *hdr, sds true_label,
                                                sds false_label, STACK_T *scope_stack) {
    ILOC_Instruction *code = NULL;
    switch (hdr->type) {
    case AST_LOGICO_OU: {
        AST_LogicExpr *expr = (AST_LogicExpr*)hdr;

        // new false label
        ILOC_Instruction *false_label_code = iloc_instruction_make();
        false_label_code->opcode = ILOC_NOP;
        false_label_code->label = label_make();
        
        ILOC_Instruction *first_code = ast_expr_generate_code_labels(expr->first, true_label,
                                                                     false_label_code->label, scope_stack);
        ILOC_Instruction *second_code = ast_expr_generate_code_labels(expr->second, true_label,
                                                                      false_label, scope_stack);
        // Do not allow, e.g, 3 || 4
        Assert(first_code->opcode != ILOC_NOP);
        Assert(second_code->opcode != ILOC_NOP);

        code = iloc_instruction_concat(code, first_code);
        code = iloc_instruction_concat(code, false_label_code);
        code = iloc_instruction_concat(code, second_code);
    } break;
    case AST_LOGICO_E: {
        Assert(false);
    } break;
    case AST_LOGICO_COMP_DIF:
    case AST_LOGICO_COMP_GE:
    case AST_LOGICO_COMP_LE:
    case AST_LOGICO_COMP_G:
    case AST_LOGICO_COMP_L:
    case AST_LOGICO_COMP_IGUAL: {
        AST_LogicExpr *expr = (AST_LogicExpr*)hdr;

        ILOC_Instruction *first_code = ast_expr_generate_code_labels(expr->first, true_label,
                                                                     false_label, scope_stack);
        ILOC_Instruction *second_code = ast_expr_generate_code_labels(expr->second, true_label,
                                                                      false_label, scope_stack);

        ILOC_Instruction *first_code_with_target_register = load_literal_to_register(first_code);
        ILOC_Instruction *second_code_with_target_register = load_literal_to_register(second_code);

        // Comparison
        ILOC_Instruction *comp = iloc_instruction_make();
        comp->opcode = get_non_immediate_logic_expr_opcode(hdr->type);
        array_push(comp->sources, first_code_with_target_register->targets[0]);
        array_push(comp->sources, second_code_with_target_register->targets[0]);
        array_push(comp->targets, iloc_register_make(ILOC_RT_GENERIC));

        // Conditional branch
        ILOC_Instruction *cbr = iloc_instruction_make();
        cbr->opcode = ILOC_CBR;
        array_push(cbr->sources, comp->targets[0]);
        array_push(cbr->targets, iloc_label_ref_make(true_label));
        array_push(cbr->targets, iloc_label_ref_make(false_label));

        code = iloc_instruction_concat(code, first_code_with_target_register);
        code = iloc_instruction_concat(code, second_code_with_target_register);
        code = iloc_instruction_concat(code, comp);
        code = iloc_instruction_concat(code, cbr);
    } break;
    case AST_LOGICO_COMP_NEGACAO:
        Assert(false);
        /* code = logic_expr_generate_code_labels((AST_LogicExpr*)hdr, true_label, false_label, scope_stack); */
        break;
    // TODO(leo): finish other ast nodes.
    case AST_IDENTIFICADOR: {
        ILOC_Instruction *id = ast_identifier_generate_code((AST_Identifier*)hdr, scope_stack);
        code = iloc_instruction_concat(code, id);
    } break;
    case AST_LITERAL: {
        ILOC_Instruction *nop = ast_literal_generate_code((AST_Literal*)hdr);
        code = iloc_instruction_concat(code, nop);
    } break;
    default:
        printf("%s not handled\n", g_ast_names[hdr->type]);
        Assert(false);
    }
    return code;
}

ILOC_Instruction *ast_do_generate_code(AST_While *while_cmd, STACK_T *scope_stack) {
    ILOC_Instruction *code = NULL;

    if (while_cmd->header.type == AST_DO_WHILE) {
        // do while case
        // Begin command label
        ILOC_Instruction *begin_label_inst = iloc_instruction_make();
        begin_label_inst->opcode = ILOC_NOP;
        begin_label_inst->label = label_make();
        // Next label
        ILOC_Instruction *next_label_inst = iloc_instruction_make();
        next_label_inst->opcode = ILOC_NOP;
        next_label_inst->label = label_make();

        ILOC_Instruction *condition_code = ast_expr_generate_code_labels(while_cmd->condition, begin_label_inst->label,
                                                                         next_label_inst->label, scope_stack);
        // Make the then branch code
        ILOC_Instruction *branch_code = NULL;
        if (while_cmd->first_command) {
            if (while_cmd->scope)
                stack_push(&scope_stack, while_cmd->scope);

            AST_Header *cmd = while_cmd->first_command;
            while (cmd) {
                ILOC_Instruction *cmd_code = ast_cmd_generate_code(cmd, scope_stack);
                branch_code = iloc_instruction_concat(branch_code, cmd_code);
                cmd = cmd->next;
            }

            if (while_cmd->scope)
                stack_pop(&scope_stack);
        }

        code = iloc_instruction_concat(code, begin_label_inst);
        code = iloc_instruction_concat(code, branch_code);
        code = iloc_instruction_concat(code, condition_code);
        code = iloc_instruction_concat(code, next_label_inst);

        return code;
    } else {
        // while case
        // Begin command label
        ILOC_Instruction *begin_label_inst = iloc_instruction_make();
        begin_label_inst->opcode = ILOC_NOP;
        begin_label_inst->label = label_make();
        // True condition label
        ILOC_Instruction *true_label_inst = iloc_instruction_make();
        true_label_inst->opcode = ILOC_NOP;
        true_label_inst->label = label_make();
        // Next label
        ILOC_Instruction *next_label_inst = iloc_instruction_make();
        next_label_inst->opcode = ILOC_NOP;
        next_label_inst->label = label_make();
        // Goto begin
        ILOC_Instruction *goto_begin = iloc_instruction_make();
        goto_begin->opcode = ILOC_JUMPI;
        array_push(goto_begin->targets, iloc_label_ref_make(begin_label_inst->label));

        ILOC_Instruction *condition_code = ast_expr_generate_code_labels(while_cmd->condition, true_label_inst->label,
                                                                         next_label_inst->label, scope_stack);
        // Make the then branch code
        ILOC_Instruction *branch_code = NULL;
        if (while_cmd->first_command) {
            if (while_cmd->scope)
                stack_push(&scope_stack, while_cmd->scope);

            AST_Header *cmd = while_cmd->first_command;
            while (cmd) {
                ILOC_Instruction *cmd_code = ast_cmd_generate_code(cmd, scope_stack);
                branch_code = iloc_instruction_concat(branch_code, cmd_code);
                cmd = cmd->next;
            }

            if (while_cmd->scope)
                stack_pop(&scope_stack);
        }

        code = iloc_instruction_concat(code, begin_label_inst);
        code = iloc_instruction_concat(code, condition_code);
        code = iloc_instruction_concat(code, true_label_inst);
        code = iloc_instruction_concat(code, branch_code);
        code = iloc_instruction_concat(code, goto_begin);
        code = iloc_instruction_concat(code, next_label_inst);

        return code;
    }
}

ILOC_Instruction *ast_if_generate_code(AST_IfElse *if_else, STACK_T *scope_stack) {
    ILOC_Instruction *code = NULL;
    // The true label for the if command.
    ILOC_Instruction *true_label = iloc_instruction_make();
    true_label->opcode = ILOC_NOP;
    true_label->label = label_make();
    // The false label for the if command.
    ILOC_Instruction *false_label = iloc_instruction_make();
    false_label->opcode = ILOC_NOP;
    false_label->label = label_make();
    // The next label for the if command.
    ILOC_Instruction *next_label = iloc_instruction_make();
    next_label->opcode = ILOC_NOP;
    next_label->label = label_make();
    // Goto next
    ILOC_Instruction *goto_next = iloc_instruction_make();
    goto_next->opcode = ILOC_JUMPI;
    array_push(goto_next->targets, iloc_label_ref_make(next_label->label));

    ILOC_Instruction *condition_code = ast_expr_generate_code_labels(if_else->condition, true_label->label,
                                                                     false_label->label, scope_stack);

    // Make the then branch code
    ILOC_Instruction *then_branch_code = NULL;
    if (if_else->then_command) {
        if (if_else->then_scope)
            stack_push(&scope_stack, if_else->then_scope);

        AST_Header *cmd = if_else->then_command;
        while (cmd) {
            ILOC_Instruction *cmd_code = ast_cmd_generate_code(cmd, scope_stack);
            then_branch_code = iloc_instruction_concat(code, cmd_code);
            cmd = cmd->next;
        }

        if (if_else->then_scope)
            stack_pop(&scope_stack);
    }
    // Make the else branch code
    ILOC_Instruction *else_branch_code = NULL;
    if (if_else->else_command) {
        if (if_else->else_scope)
            stack_push(&scope_stack, if_else->else_scope);

        AST_Header *cmd = if_else->else_command;
        while (cmd) {
            ILOC_Instruction *cmd_code = ast_cmd_generate_code(cmd, scope_stack);
            else_branch_code = iloc_instruction_concat(code, cmd_code);
            cmd = cmd->next;
        }

        if (if_else->else_scope)
            stack_pop(&scope_stack);
    }

    code = iloc_instruction_concat(code, condition_code);
    code = iloc_instruction_concat(code, true_label);
    if (then_branch_code) {
        code = iloc_instruction_concat(code, then_branch_code);
    }
    code = iloc_instruction_concat(code, goto_next);
    code = iloc_instruction_concat(code, false_label);
    if (else_branch_code) {
        code = iloc_instruction_concat(code, else_branch_code);
    }
    code = iloc_instruction_concat(code, next_label);

    return code;
}

ILOC_Instruction *ast_cmd_generate_code(AST_Header *cmd, STACK_T *scope_stack) {
    ILOC_Instruction *code = NULL;

    switch (cmd->type) {
    case AST_ATRIBUICAO: {
        ILOC_Instruction *assignment_code = ast_assignment_generate_code((AST_Assignment*)cmd, scope_stack);
        Assert(assignment_code);
        code = iloc_instruction_concat(code, assignment_code);
    } break;
    case AST_IF_ELSE: {
        ILOC_Instruction *if_code = ast_if_generate_code((AST_IfElse*)cmd, scope_stack);
        Assert(if_code);
        code = iloc_instruction_concat(code, if_code);
    } break;
    case AST_WHILE_DO:
    case AST_DO_WHILE: {
        ILOC_Instruction *do_code = ast_do_generate_code((AST_While*)cmd, scope_stack);
        code = iloc_instruction_concat(code, do_code);
    } break;
    // TODO(leo): the rest of the ast nodes.

    default:
        printf("node: %s\n", g_ast_names[cmd->type]);
        Assert(false);
    }

    return code;
}

ILOC_Instruction *ast_function_generate_code(AST_Function *func, STACK_T *scope_stack) {
    /* printf("Generating code for function %s\n", get_key_from_identifier(func->identifier)); */
    ILOC_Instruction *code = NULL;

    stack_push(&scope_stack, func->scope);

    AST_Header *cmd = func->first_command;
    while (cmd) {
        ILOC_Instruction *cmd_code = ast_cmd_generate_code(cmd, scope_stack);
        code = iloc_instruction_concat(code, cmd_code);
        cmd = cmd->next;
    }

    stack_pop(&scope_stack);

    return code;
}

ILOC_Instruction *iloc_generate_code(AST_Program *program) {
    Assert(program); Assert(program->scope); Assert(program->scope->type == ST_GLOBAL);

    STACK_T *scope_stack = stack_initialize();
    stack_push(&scope_stack, program->scope);

    ILOC_Instruction *code = NULL;
    // Loop over all functions. NOTE(leo): currently there is only main declared.
    AST_Function *func = program->first_func;

    while (func) {
        ILOC_Instruction *func_code = ast_function_generate_code(func, scope_stack);
        code = iloc_instruction_concat(code, func_code);
        func = func->next;
    }

    stack_pop(&scope_stack);

    return code;
}

ILOC_Instruction *iloc_instruction_from_declaration(char *symbol_name, DeclarationHeader *decl_hdr) {
    ILOC_Instruction *inst = iloc_instruction_make();
    inst->label = sdsnew(symbol_name);
    return inst;
}

sds iloc_stringify(ILOC_Instruction *code) {
    // Go to the beginning of the list.
    ILOC_Instruction *inst = code;
    while (inst->prev) inst = inst->prev;

    sds code_str = sdsempty();
    // Loop one instruction by one
    while (inst) {
        if (inst->label) {
            code_str = sdscat(code_str, inst->label);
            code_str = sdscat(code_str, ":\n");
        }

        code_str = sdscatprintf(code_str, "%s ", iloc_opcode_names[inst->opcode]);

        if (inst->opcode == ILOC_NOP) {
            code_str = sdscat(code_str, "\n");
            inst = inst->next;
            continue;
        }

        for (int i = 0; i < array_len(inst->sources); ++i) {
            sds source_name = iloc_operand_string(&inst->sources[i]);

            if (i == 0) {
                code_str = sdscatprintf(code_str, "%s", source_name);
            } else {
                code_str = sdscatprintf(code_str, ", %s", source_name);
            }

            sdsfree(source_name);
        }

        code_str = sdscat(code_str, " => ");

        for (int i = 0; i < array_len(inst->targets); ++i) {
            sds target_name = iloc_operand_string(&inst->targets[i]);

            if (i == 0) {
                code_str = sdscatprintf(code_str, "%s", target_name);
            } else {
                code_str = sdscatprintf(code_str, ", %s", target_name);
            }
            
            sdsfree(target_name);
        }
        code_str = sdscat(code_str, "\n");
        inst = inst->next;
    }

    return code_str;
}

void iloc_free_code(ILOC_Instruction *code) {
    ILOC_Instruction *it = code;
    // Get it to the beginning of the list
    while (it->prev) it = it->prev; 

    while (it) {
        ILOC_Instruction *next = it->next;
        iloc_instruction_free(it);
        it = next;
    }
}
