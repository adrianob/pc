#include "iloc.h"

ILOC_OperandHeader *iloc_register_make(ILOC_RegisterType register_type) {
    ILOC_Register *r = calloc(1, sizeof(*r));
    r->header.type = ILOC_REGISTER;
    r->register_type = register_type;
    r->number = (register_type == ILOC_RT_GENERIC) ? get_next_register_number() : -1;
    return &r->header;
}

ILOC_OperandHeader *iloc_number_make(int value) {
    ILOC_Number *n = calloc(1, sizeof(*n));
    n->header.type = ILOC_NUMBER;
    n->value = value;
    return &n->header;
}

ILOC_Instruction *iloc_instruction_make(void) {
    ILOC_Instruction *i = calloc(1, sizeof(*i));
    array_init(i->sources);
    array_init(i->targets);
    return i;
}

sds iloc_operand_string(ILOC_OperandHeader *hdr) {
    sds name;
    switch (hdr->type) {
    case ILOC_NUMBER: {
        ILOC_Number *num = (ILOC_Number*)hdr;
        name = sdscatprintf(sdsempty(), "%d", num->value);
    } break;
    case ILOC_LABEL_REF: {
        ILOC_LabelRef *label = (ILOC_LabelRef*)hdr;
        name = sdsdup(label->ref);
    } break;
    case ILOC_REGISTER: {
        ILOC_Register *reg = (ILOC_Register*)hdr;
        if (reg->register_type == ILOC_RT_FP) {
            name = sdsnew("fp");
        } else if (reg->register_type == ILOC_RT_RBSS) {
            name = sdsnew("rbss");
        } else {
            name = sdscatprintf(sdsempty(), "r%d", reg->number);
        }
    } break;
    default: Assert(false);
    }
    return name;
}

ILOC_Instruction *iloc_instruction_append_after(ILOC_Instruction *inst, ILOC_Instruction *new_inst) {
    if (!inst) return new_inst;

    if (inst->next) {
        ILOC_Instruction *tmp = inst->next;
        inst->next = new_inst;
        new_inst->next = tmp;
        new_inst->next->prev = new_inst;
        new_inst->prev = inst;
        return new_inst;
    } else {
        inst->next = new_inst;
        new_inst->prev = inst;
        return new_inst;
    }
}

ILOC_Instruction *iloc_instruction_append_before(ILOC_Instruction *inst, ILOC_Instruction *new_inst) {
    if (!inst) return new_inst;

    if (inst->prev) {
        ILOC_Instruction *tmp = inst->prev;
        tmp->next = new_inst;
        new_inst->prev = tmp;
        new_inst->next = inst;
        return new_inst;
    } else {
        inst->prev = new_inst;
        new_inst->next = inst;
        return new_inst;
    }
}

ILOC_Instruction *ast_assignment_generate_code(AST_Assignment *assignment, STACK_T *scope_stack) {
    printf("Generating code for assignment\n");
    ILOC_Instruction *code = iloc_instruction_make();

    if (assignment->is_user_type_assignment) {
        // Not implemented yet.
        Assert(false);
    } else {
        Assert(assignment->identifier->type == AST_IDENTIFICADOR);

        switch (assignment->expr->type) {
        case AST_LITERAL: {
            code->opcode = ILOC_STOREAI;
            DeclarationHeader *decl = scope_find_declaration_recursive(
                (AST_Identifier*)assignment->identifier, scope_stack
            );
            AST_Literal *lit = (AST_Literal*)assignment->expr;
            TableSymbol *symbol = (TableSymbol*)lit->entry->value;
            switch (symbol->token_type) {
            case POA_LIT_BOOL: {
                int address_offset = declaration_header_get_address_offset(decl);
                array_push(code->sources, iloc_number_make((int)symbol->value_bool));
                array_push(code->targets, iloc_register_make(ILOC_RT_FP));
                array_push(code->targets, iloc_number_make(address_offset));
            } break;
            case POA_LIT_CHAR: {
                printf("Not implemented for chars yet\n");
                Assert(false);
            } break;
            case POA_LIT_INT: {
                code->opcode = ILOC_STOREAI;
                int address_offset = declaration_header_get_address_offset(decl);
                array_push(code->sources, iloc_number_make(symbol->value_int));
                array_push(code->targets, iloc_register_make(ILOC_RT_FP));
                array_push(code->targets, iloc_number_make(address_offset));
            } break;
            case POA_LIT_FLOAT: case POA_LIT_STRING: default:
                printf("Not implemented\n");
                Assert(false);
            }
        } break;
        case AST_IDENTIFICADOR: {
            /* code->opcode = ILOC_STOREA0; */
            /* DeclarationHeader *decl = scope_find_declaration_recursive( */
            /*     (AST_Identifier*)assignment->identifier, scope_stack */
            /* ); */
            /* AST_Literal *lit = (AST_Literal*)assignment->expr; */
            /* TableSymbol *symbol = (TableSymbol*)lit->entry->value; */
            /* switch (symbol->token_type) { */
            /* case POA_LIT_BOOL: { */
            /*     int address_offset = declaration_header_get_address_offset(decl); */
            /*     array_push(code->sources, iloc_number_make((int)symbol->value_bool)); */
            /*     array_push(code->targets, iloc_register_make(ILOC_RT_FP)); */
            /*     array_push(code->targets, iloc_number_make(address_offset)); */
            /* } break; */
            /* case POA_LIT_CHAR: { */
            /*     printf("Not implemented for chars yet\n"); */
            /*     Assert(false); */
            /* } break; */
            /* case POA_LIT_INT: { */
            /*     int address_offset = declaration_header_get_address_offset(decl); */
            /*     array_push(code->sources, iloc_number_make(symbol->value_int)); */
            /*     array_push(code->targets, iloc_register_make(ILOC_RT_FP)); */
            /*     array_push(code->targets, iloc_number_make(address_offset)); */
            /* } break; */
            /* case POA_LIT_FLOAT: case POA_LIT_STRING: default: */
            /*     printf("Not implemented\n"); */
            /*     Assert(false); */
            /* } */

        } break;
        /* case AST_VETOR_INDEXADO: */
        /* case AST_CHAMADA_DE_FUNCAO: */
        default:
            printf("Not implemented yet\n");
            Assert(false);
        }
    }

    return code;
}


ILOC_Instruction *ast_function_generate_code(AST_Function *func, STACK_T *scope_stack) {
    printf("Generating code for function %s\n", get_key_from_identifier(func->identifier));
    ILOC_Instruction *cmd_inst = NULL;

    stack_push(&scope_stack, func->scope);

    AST_Header *cmd = func->first_command;
    while (cmd) {
        switch (cmd->type) {
        case AST_ATRIBUICAO: {
            ILOC_Instruction *code = ast_assignment_generate_code((AST_Assignment*)cmd, scope_stack);
            cmd_inst = iloc_instruction_append_after(cmd_inst, code);
        } break;
        // TODO(leo): the rest of the ast nodes.
        default:
            printf("node: %s\n", g_ast_names[cmd->type]);
            Assert(false);
        }

        cmd = cmd->next;
    }

    return cmd_inst;
}

ILOC_Instruction *iloc_generate_code(AST_Program *program) {
    Assert(program); Assert(program->scope); Assert(program->scope->type == ST_GLOBAL);

    STACK_T *scope_stack = stack_initialize();
    stack_push(&scope_stack, program->scope);

    ILOC_Instruction *inst = NULL;
    // Loop over all functions. NOTE(leo): currently there is only main declared.
    AST_Function *func = program->first_func;

    while (func) {
        ILOC_Instruction *func_code = ast_function_generate_code(func, scope_stack);
        inst = iloc_instruction_append_after(inst, func_code);
        func = func->next;
    }

    return inst;
}

ILOC_Instruction *iloc_instruction_from_declaration(char *symbol_name, DeclarationHeader *decl_hdr) {
    ILOC_Instruction *inst = iloc_instruction_make();
    inst->label = sdsnew(symbol_name);
    return inst;
}

sds iloc_stringify(ILOC_Instruction *code) {
    printf("Converting code to text...\n");
    // Go to the beginning of the list.
    ILOC_Instruction *inst = code;
    while (inst->prev) inst = inst->prev;

    sds code_str = sdsempty();
    // Loop one instruction by one
    while (inst) {
        code_str = sdscatprintf(code_str, "%s ", iloc_opcode_names[inst->opcode]);
        for (int i = 0; i < array_len(inst->sources); ++i) {
            sds source_name = iloc_operand_string(inst->sources[i]);

            if (i == 0) {
                code_str = sdscatprintf(code_str, "%s", source_name);
            } else {
                code_str = sdscatprintf(code_str, ", %s", source_name);
                sdsfree(source_name);
            }
        }
        code_str = sdscat(code_str, " => ");
        for (int i = 0; i < array_len(inst->targets); ++i) {
            sds target_name = iloc_operand_string(inst->targets[i]);

            if (i == 0) {
                code_str = sdscatprintf(code_str, "%s", target_name);
            } else {
                code_str = sdscatprintf(code_str, ", %s", target_name);
                sdsfree(target_name);
            }
        }
        inst = inst->next;
    }

    return code_str;
}
