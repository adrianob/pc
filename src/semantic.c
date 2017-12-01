#include "semantic.h"

UserTypeField *user_type_field_make(IKS_Type type, AST_Header *id_hdr, FieldVisibility vis) {
    UserTypeField *u = calloc(1, sizeof(*u));
    u->type = type;
    u->visibility = vis;
    u->identifier = id_hdr;
    return u;
}

void *user_type_field_free(UserTypeField *f) {
    ast_header_free(f->identifier);
    free(f);
}

DeclarationHeader *user_type_declaration_make(AST_Identifier *id, UserTypeField *first_field) {
    UserTypeDeclaration *u = calloc(1, sizeof(*u));
    u->header.type = DT_USER_TYPE;
    u->header.ref_count = 1;
    u->identifier = id;
    u->first_field = first_field;
    return &u->header;
}

void *user_type_declaration_free(UserTypeDeclaration *d) {
    ast_identifier_free(d->identifier);
    UserTypeField *field = d->first_field;
    while (field) {
        UserTypeField *next = field->next;
        user_type_field_free(field);
        field = next;
    }
    free(d);
}

DeclarationHeader *variable_declaration_make(AST_Identifier *id, AST_Identifier *type_id, IKS_Type type) {
    VariableDeclaration *d = calloc(1, sizeof(*d));
    d->header.type = DT_VARIABLE;
    d->header.ref_count = 1;
    d->identifier = id;
    d->type_identifier = type_id;
    d->type = type;
    d->size_in_bytes = get_primitive_type_size(type);
    return &d->header;
}

void *variable_declaration_free(VariableDeclaration *d) {
    ast_identifier_free(d->type_identifier);
    ast_identifier_free(d->identifier);
    free(d);
}

DeclarationHeader *vector_declaration_make(AST_Identifier *id, AST_Literal *count, IKS_Type type) {
    VectorDeclaration *d = calloc(1, sizeof(*d));
    d->header.type = DT_VECTOR;
    d->header.ref_count = 1;
    d->identifier = id;
    d->count = count;
    d->type = type;
    d->elem_size_in_bytes = get_primitive_type_size(type);
    return &d->header;
}

void *vector_declaration_free(VectorDeclaration *d) {
    ast_identifier_free(d->identifier);
    ast_literal_free(d->count);
    free(d);
}

DeclarationHeader *function_declaration_make(AST_Identifier *id, AST_Identifier *ret_id,
                                             IKS_Type return_type, DeclarationHeader *first_param) {
    FunctionDeclaration *d = calloc(1, sizeof(*d));
    d->header.type = DT_FUNCTION;
    d->header.ref_count = 1;
    d->identifier = id;
    d->return_identifier = ret_id;
    d->return_type = return_type;
    d->first_param = first_param;
    return &d->header;
}

void *function_declaration_free(FunctionDeclaration *d) {
    ast_identifier_free(d->identifier);
    ast_identifier_free(d->return_identifier);

    DeclarationHeader *param = d->first_param;
    while (param) {
        DeclarationHeader *next = param->next;
        declaration_header_free(param);
        param = next;
    }
    free(d);
}

int function_declaration_num_params(FunctionDeclaration *decl) {
    int num_params = 0;

    DeclarationHeader *search = decl->first_param;
    while (search) {
        num_params++;
        search = search->next;
    }

    return num_params;
}

void declaration_header_free(void *data) {
    if (!data) return;
    DeclarationHeader *decl_hdr = (DeclarationHeader*)data;

    Assert(decl_hdr->ref_count >= 1);

    if (decl_hdr->ref_count == 1) {
        switch (decl_hdr->type) {
        case DT_FUNCTION: function_declaration_free((FunctionDeclaration*)data); break;
        case DT_USER_TYPE: user_type_declaration_free((UserTypeDeclaration*)data); break;
        case DT_VECTOR: vector_declaration_free((VectorDeclaration*)data); break;
        case DT_VARIABLE: variable_declaration_free((VariableDeclaration*)data); break;
        default: Assert(false);
        }
    } else {
        --decl_hdr->ref_count;
    }
}
