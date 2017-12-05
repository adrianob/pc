#ifndef __SEMANTIC_ERRORS_H__
#define __SEMANTIC_ERRORS_H__

#include "cc_ast.h"
#include "sds.h"

typedef struct SemanticError {
    IKS_Error type;
    sds description;
} SemanticError;

void push_declared_error(AST_Identifier *id);
void push_non_existent_field_error(AST_Identifier *id);
void push_undeclared_error(AST_Identifier *id);
void push_variable_error(AST_Identifier *id);
void push_wrong_type_args(AST_Identifier *id, int param_index);
void push_invalid_coertion_error(AST_Header *header);
void push_wrong_type_error(AST_Header *header);
void push_wrong_par_return(AST_Header *header, IKS_Type expected_type);
void push_function_error(AST_Identifier *id);
void push_vector_error(AST_Identifier *id);
void push_missing_args_error(AST_Identifier *id);
void push_wrong_par_output(AST_Header *header);
void push_wrong_par_input(AST_Header *header);
void push_user_type_definition_error(AST_Identifier *id);
void push_user_type_error(AST_Identifier *id);
void push_excess_args_error(AST_Identifier *id);
void push_wrong_type_usage_error(AST_Identifier *id, DeclarationType type);

#endif // __SEMANTIC_ERRORS_H__
