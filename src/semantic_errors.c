#include "semantic_errors.h"
#include "macros.h"

Array(SemanticError) g_semantic_errors = NULL;

void push_declared_error(AST_Identifier *id) {
    SemanticError err;
    err.type = IKS_ERROR_DECLARED;
    err.description = sdscatprintf(sdsempty(),
                                   "%d: Identifier %s already declared.",
                                   get_line_from_identifier(id),
                                   get_key_from_identifier(id));
    array_push(g_semantic_errors, err);
}

void push_non_existent_field_error(AST_Identifier *id) {
    SemanticError err;
    err.type = IKS_ERROR_NON_EXISTENT_FIELD;
    err.description = sdscatprintf(sdsempty(),
                                   "%d: Field %s does not exist.",
                                   get_line_from_identifier(id),
                                   get_key_from_identifier(id));
    array_push(g_semantic_errors, err);
}

void push_undeclared_error(AST_Identifier *id) {
    SemanticError err;
    err.type = IKS_ERROR_UNDECLARED;
    err.description = sdscatprintf(sdsempty(),
                                   "%d: Identifier %s was not declared.",
                                   get_line_from_identifier(id),
                                   get_key_from_identifier(id));
    array_push(g_semantic_errors, err);
}

void push_user_type_definition_error(AST_Identifier *id) {
    SemanticError err;
    err.type = IKS_ERROR_USER_TYPE_DEFINITION;
    err.description = sdscatprintf(sdsempty(),
                                   "%d: Identifier %s has to be used as a user type definition.",
                                   get_line_from_identifier(id),
                                   get_key_from_identifier(id));
    array_push(g_semantic_errors, err);
}

void push_user_type_error(AST_Identifier *id) {
    SemanticError err;
    err.type = IKS_ERROR_USER_TYPE;
    err.description = sdscatprintf(sdsempty(),
                                   "%d: Identifier %s has to be used as a user type identifier.",
                                   get_line_from_identifier(id),
                                   get_key_from_identifier(id));
    array_push(g_semantic_errors, err);
}

void push_variable_error(AST_Identifier *id) {
    SemanticError err;
    err.type = IKS_ERROR_VARIABLE;
    err.description = sdscatprintf(sdsempty(),
                                   "%d: Identifier %s has to be used as a variable.",
                                   get_line_from_identifier(id),
                                   get_key_from_identifier(id));
    array_push(g_semantic_errors, err);
}

void push_wrong_type_args(AST_Identifier *id, int param_index) {
    SemanticError err;
    err.type = IKS_ERROR_WRONG_TYPE_ARGS;
    err.description = sdscatprintf(sdsempty(),
                                   "%d: Type mismatch in %s for parameter %d.",
                                   get_line_from_identifier(id),
                                   get_key_from_identifier(id),
                                   param_index);
    array_push(g_semantic_errors, err);
}

void push_invalid_coertion_error(AST_Header *header) {
    SemanticError err;
    err.type = (header->semantic_type == IKS_CHAR)
        ? IKS_ERROR_CHAR_TO_X
        : IKS_ERROR_STRING_TO_X;
    err.description = sdscatprintf(sdsempty(),
                                   "%d: Invalid coertion from %s.",
                                   find_line_number_from_ast_header(header),
                                   iks_type_names[header->semantic_type]);
    array_push(g_semantic_errors, err);
}

void push_wrong_type_error(AST_Header *header) {
    SemanticError err;
    err.type = IKS_ERROR_WRONG_TYPE;
    err.description = sdscatprintf(sdsempty(),
                                   "%d: Wrong type %s.",
                                   find_line_number_from_ast_header(header),
                                   iks_type_names[header->semantic_type]);
    array_push(g_semantic_errors, err);
}

void push_wrong_par_return(AST_Header *header, IKS_Type expected_type) {
    SemanticError err;
    err.type = IKS_ERROR_WRONG_PAR_RETURN;
    err.description = sdscatprintf(sdsempty(),
                                   "%d: return has to match the return type of the function (%s).",
                                   find_line_number_from_ast_header(header),
                                   iks_type_names[expected_type]);
    array_push(g_semantic_errors, err);
}

void push_function_error(AST_Identifier *id) {
    SemanticError err;
    err.type = IKS_ERROR_FUNCTION;
    err.description = sdscatprintf(sdsempty(),
                                   "%d: Identifier %s has to be used as a function.",
                                   get_line_from_identifier(id),
                                   get_key_from_identifier(id));
    array_push(g_semantic_errors, err);
}

void push_vector_error(AST_Identifier *id) {
    SemanticError err;
    err.type = IKS_ERROR_VECTOR;
    err.description = sdscatprintf(sdsempty(),
                                   "%d: Identifier %s has to be used as a vector.",
                                   get_line_from_identifier(id),
                                   get_key_from_identifier(id));
    array_push(g_semantic_errors, err);
}

void push_missing_args_error(AST_Identifier *id) {
    SemanticError err;
    err.type = IKS_ERROR_MISSING_ARGS;
    err.description = sdscatprintf(sdsempty(),
                                   "%d: %s called with missing arguments.",
                                   get_line_from_identifier(id),
                                   get_key_from_identifier(id));
    array_push(g_semantic_errors, err);
}

void push_wrong_par_output(AST_Header *header) {
    SemanticError err;
    err.type = IKS_ERROR_WRONG_PAR_OUTPUT;
    err.description = sdscatprintf(sdsempty(),
                                   "%d: output only receives string literals and arithmetic expressions.",
                                   find_line_number_from_ast_header(header));
    array_push(g_semantic_errors, err);
}

void push_wrong_par_input(AST_Header *header) {
    SemanticError err;
    err.type = IKS_ERROR_WRONG_PAR_INPUT;
    err.description = sdscatprintf(sdsempty(),
                                   "%d: input only receives identifiers.",
                                   find_line_number_from_ast_header(header));
    array_push(g_semantic_errors, err);
}

void push_excess_args_error(AST_Identifier *id) {
    SemanticError err;
    err.type = IKS_ERROR_EXCESS_ARGS;
    err.description = sdscatprintf(sdsempty(),
                                   "%d: %s called with excessive arguments.",
                                   get_line_from_identifier(id),
                                   get_key_from_identifier(id));
    array_push(g_semantic_errors, err);
}

void push_wrong_type_usage_error(AST_Identifier *id, DeclarationType type) {
    SemanticError err;
    switch (type) {
    case DT_FUNCTION: err.type = IKS_ERROR_FUNCTION; break;
    case DT_USER_TYPE: err.type = IKS_ERROR_USER_TYPE; break;
    case DT_USER_TYPE_DEFINITION: err.type = IKS_ERROR_USER_TYPE_DEFINITION; break;
    case DT_VECTOR: err.type = IKS_ERROR_VECTOR; break;
    case DT_VARIABLE: err.type = IKS_ERROR_VARIABLE; break;
    default: Assert(false);
    }
    err.description = sdscatprintf(sdsempty(),
                                   "%d: Identifier %s has to be used as a %s.",
                                   get_line_from_identifier(id),
                                   get_key_from_identifier(id),
                                   iks_type_names[type]);
    array_push(g_semantic_errors, err);
}
