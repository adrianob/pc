#include "semantic.h"

int get_user_type_definition_size(UserTypeField *first_field) {
    int size_in_bytes = 0;

    UserTypeField *field = first_field;
    while (field) {
        size_in_bytes += field->size_in_bytes;
        field = field->next;
    }

    return size_in_bytes;
}

int get_vector_declaration_size(VectorDeclaration *v) {
    TableSymbol *ts = (TableSymbol*)v->count->entry->value;
    Assert(ts->token_type == POA_LIT_INT);

    int num_elements = ts->value_int;
    int size = v->elem_size_in_bytes * num_elements;
    return size;
}

UserTypeField *user_type_field_make(IKS_Type type, AST_Header *id_hdr, FieldVisibility vis) {
    UserTypeField *u = calloc(1, sizeof(*u));
    u->type = type;
    u->visibility = vis;
    u->identifier = id_hdr;
    u->size_in_bytes = get_primitive_type_size(type);
    return u;
}

void user_type_field_free(UserTypeField *f) {
    ast_header_free(f->identifier);
    free(f);
}

DeclarationHeader *user_type_definition_make(AST_Identifier *id, UserTypeField *first_field) {
    UserTypeDefinition *u = calloc(1, sizeof(*u));
    u->header.type = DT_USER_TYPE_DEFINITION;
    u->identifier = id;
    u->first_field = first_field;
    u->size_in_bytes = get_user_type_definition_size(first_field);
    return &u->header;
}

void user_type_definition_free(UserTypeDefinition *def) {
    ast_identifier_free(def->identifier);
    UserTypeField *field = def->first_field;
    while (field) {
        UserTypeField *next = field->next;
        user_type_field_free(field);
        field = next;
    }
    free(def);
}

DeclarationHeader *user_type_declaration_make(AST_Identifier *id, UserTypeDefinition *type_definition) {
    UserTypeDeclaration *u = calloc(1, sizeof(*u));
    u->header.type = DT_USER_TYPE;
    u->header.ref_count = 1;
    u->identifier = id;
    u->type_definition = type_definition;
    return &u->header;
}

void user_type_declaration_free(UserTypeDeclaration *d) {
    ast_identifier_free(d->identifier);
    free(d);
}

DeclarationHeader *variable_declaration_make(AST_Identifier *id, AST_Identifier *type_id, IKS_Type type) {
    VariableDeclaration *d = calloc(1, sizeof(*d));
    d->header.type = DT_VARIABLE;
    d->header.ref_count = 1;
    d->identifier = id;
    d->address_offset = -1;
    d->type_identifier = type_id;
    d->type = type;
    d->size_in_bytes = get_primitive_type_size(type);
    return &d->header;
}

void variable_declaration_free(VariableDeclaration *d) {
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

void vector_declaration_free(VectorDeclaration *d) {
    ast_identifier_free(d->identifier);
    ast_literal_free(d->count);
    free(d);
}

DeclarationHeader *function_declaration_make(AST_Identifier *id, AST_Identifier *ret_id,
                                             IKS_Type return_type, UserTypeDefinition *return_type_definition,
                                             DeclarationHeader *first_param) {
    FunctionDeclaration *d = calloc(1, sizeof(*d));
    d->header.type = DT_FUNCTION;
    d->header.ref_count = 1;
    d->identifier = id;
    d->return_identifier = ret_id;
    d->return_type = return_type;
    d->return_type_definition = return_type_definition;
    d->first_param = first_param;
    return &d->header;
}

void function_declaration_free(FunctionDeclaration *d) {
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
        case DT_USER_TYPE_DEFINITION: user_type_definition_free((UserTypeDefinition*)data); break;
        case DT_VECTOR: vector_declaration_free((VectorDeclaration*)data); break;
        case DT_VARIABLE: variable_declaration_free((VariableDeclaration*)data); break;
        default: Assert(false);
        }
    } else {
        --decl_hdr->ref_count;
    }
}
