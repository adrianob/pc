#ifndef __SEMANTIC_H__
#define __SEMANTIC_H__

#include "cc_ast.h"
#include "table_symbol.h"
#include "enums.h"
#include "sds.h"

typedef struct SemanticError {
    IKS_Error type;
    sds description;
} SemanticError;

typedef struct DeclarationHeader {
    DeclarationType type;
    struct DeclarationHeader *next;
} DeclarationHeader;

typedef struct UserTypeField {
    IKS_Type           type;
    FieldVisibility    visibility;
    AST_Header        *identifier;
    int                size_in_bytes;
    struct UserTypeField     *next;
} UserTypeField;

static UserTypeField *user_type_field_make(IKS_Type type, AST_Header *id_hdr, FieldVisibility vis) {
    UserTypeField *u = calloc(1, sizeof(*u));
    u->type = type;
    u->visibility = vis;
    u->identifier = id_hdr;
    return u;
}

typedef struct UserTypeDeclaration {
    DeclarationHeader     header;
    AST_Identifier       *identifier;
    UserTypeField        *first_field;
} UserTypeDeclaration;

static DeclarationHeader *user_type_declaration_make(AST_Identifier *id, UserTypeField *first_field) {
    UserTypeDeclaration *u = calloc(1, sizeof(*u));
    u->header.type = DT_USER_TYPE;
    u->identifier = id;
    u->first_field = first_field;
    return &u->header;
}

typedef struct VariableDeclaration {
    DeclarationHeader   header;
    IKS_Type            type;
    AST_Identifier     *type_identifier;
    AST_Identifier     *identifier;
    int            size_in_bytes;
    // @Todo(leo): Consider when variable is const.
    // bool                is_const;
} VariableDeclaration;

static DeclarationHeader *variable_declaration_make(AST_Identifier *id, AST_Identifier *type_id, IKS_Type type) {
    VariableDeclaration *d = calloc(1, sizeof(*d));
    d->header.type = DT_VARIABLE;
    d->identifier = id;
    d->type_identifier = type_id;
    d->type = type;
    d->size_in_bytes = get_primitive_type_size(type);
    return &d->header;
}

typedef struct VectorDeclaration {
    DeclarationHeader   header;
    AST_Identifier     *identifier;
    AST_Literal        *count;
    IKS_Type            type;
    int                 elem_size_in_bytes;
} VectorDeclaration;

static DeclarationHeader *vector_declaration_make(AST_Identifier *id, AST_Literal *count, IKS_Type type) {
    VectorDeclaration *d = calloc(1, sizeof(*d));
    d->header.type = DT_VECTOR;
    d->identifier = id;
    d->count = count;
    d->type = type;
    d->elem_size_in_bytes = get_primitive_type_size(type);
    return &d->header;
}

typedef struct FunctionDeclaration {
    DeclarationHeader   header;
    AST_Identifier     *identifier;
    AST_Identifier     *return_identifier;
    IKS_Type            return_type;
    DeclarationHeader  *first_param;
} FunctionDeclaration;

static DeclarationHeader *function_declaration_make(AST_Identifier *id, AST_Identifier *ret_id,
                                                    IKS_Type return_type, DeclarationHeader *first_param) {
    FunctionDeclaration *d = calloc(1, sizeof(*d));
    d->header.type = DT_FUNCTION;
    d->identifier = id;
    d->return_identifier = ret_id;
    d->return_type = return_type;
    d->first_param = first_param;
    return &d->header;
}

static int function_declaration_num_params(FunctionDeclaration *decl) {
    int num_params = 0;

    DeclarationHeader *search = decl->first_param;
    while (search) {
        num_params++;
        search = search->next;
    }

    return num_params;
}

#endif // __SEMANTIC_H__
