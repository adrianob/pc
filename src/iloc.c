#include "iloc.h"

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
    ILOC_Operand op;
    op.type = ILOC_REGISTER;
    op.register_type = type;
    op.register_number = (type == ILOC_RT_GENERIC) ? get_next_register_number() : -1;
    return op;
}

ILOC_Operand iloc_number_make(int value) {
    ILOC_Operand op;
    op.type = ILOC_NUMBER;
    op.number = value;
    return op;
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
    ILOC_Instruction *last = new_inst;

    if (!inst) {
        while (last->next) last = last->next;
        return last;
    }

    if (inst->next) {
        Assert(false);
    } else {
        ILOC_Instruction *first = new_inst;
        while (first->prev)  first = first->prev;
        inst->next = first;
        first->prev = inst;
        while (last->next) last = last->next;
        return last;
    }
}

ILOC_Instruction *ast_assignment_generate_code(AST_Assignment *assignment, STACK_T *scope_stack);
ILOC_Instruction *ast_expr_generate_code(AST_Header *expr, STACK_T *scope_stack);

ILOC_OpCode get_non_immediate_logic_expr_opcode(int ast_type) {
    switch (ast_type) {
    case AST_LOGICO_E:          return ILOC_AND;
    case AST_LOGICO_OU:         return ILOC_OR;
    case AST_LOGICO_COMP_DIF:   return ILOC_NE;
    case AST_LOGICO_COMP_IGUAL: return ILOC_EQ;
    case AST_LOGICO_COMP_LE:    return ILOC_LE;
    case AST_LOGICO_COMP_GE:    return ILOC_GE;
    case AST_LOGICO_COMP_L:     return ILOC_LT;
    case AST_LOGICO_COMP_G:     return ILOC_GT;
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
        Assert(assignment->identifier->type == AST_IDENTIFICADOR);

        ILOC_Instruction *expr_code = ast_expr_generate_code(assignment->expr, scope_stack);
        code = iloc_instruction_concat(code, expr_code);

        bool is_global_scope;
        DeclarationHeader *decl = scope_find_declaration_recursive(
            (AST_Identifier*)assignment->identifier, scope_stack, &is_global_scope
        );
        ILOC_Instruction *inst = iloc_instruction_make();
        inst->opcode = ILOC_STOREAI;
        // Sources
        if (expr_code->opcode == ILOC_NOP) {
            ILOC_Instruction *load_into_reg = iloc_instruction_make();
            load_into_reg->opcode = ILOC_LOADI;
            array_push(load_into_reg->sources, expr_code->targets[0]);
            array_push(load_into_reg->targets, iloc_register_make(ILOC_RT_GENERIC));

            Assert(load_into_reg->targets[0].register_type == ILOC_RT_GENERIC);
            Assert(load_into_reg->targets[0].register_number != -1);

            code = iloc_instruction_concat(code, load_into_reg);

            array_push(inst->sources, load_into_reg->targets[0]);
            /* printf("Target number is: %d\n", load_into_reg->targets[0].number); */
        } else {
            array_push(inst->sources, expr_code->targets[0]);
        }
        // Targets
        if (is_global_scope)
            array_push(inst->targets, iloc_register_make(ILOC_RT_RBSS));
        else
            array_push(inst->targets, iloc_register_make(ILOC_RT_RARP));
        int address_offset = declaration_header_get_address_offset(decl);
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

ILOC_Instruction *ast_cmd_generate_code(AST_Header *cmd, STACK_T *scope_stack) {
    ILOC_Instruction *code = NULL;

    switch (cmd->type) {
    case AST_ATRIBUICAO: {
        ILOC_Instruction *assignment_code = ast_assignment_generate_code((AST_Assignment*)cmd, scope_stack);
        Assert(assignment_code);

        code = iloc_instruction_concat(code, assignment_code);
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
