#ifndef __SEMANTIC_H__
#define __SEMANTIC_H__

#include <stdint.h>

#include "cc_ast.h"
#include "table_symbol.h"
#include "enums.h"
#include "sds.h"

typedef struct DeclarationHeader {
    DeclarationType type;
    int             ref_count;
    struct DeclarationHeader *next;
} DeclarationHeader;

typedef struct UserTypeField {
    IKS_Type               type;
    FieldVisibility        visibility;
    AST_Header            *identifier;
    int                    size_in_bytes;
    struct UserTypeField  *next;
} UserTypeField;

UserTypeField *user_type_field_make(IKS_Type type, AST_Header *id_hdr, FieldVisibility vis);

typedef struct UserTypeDeclaration {
    DeclarationHeader     header;
    AST_Identifier       *identifier;
    UserTypeField        *first_field;
} UserTypeDeclaration;

DeclarationHeader *user_type_declaration_make(AST_Identifier *id, UserTypeField *first_field);

typedef struct VariableDeclaration {
    DeclarationHeader   header;
    IKS_Type            type;
    AST_Identifier     *type_identifier;
    AST_Identifier     *identifier;
    int                 size_in_bytes;
    // @Todo(leo): Consider when variable is const.
    // bool                is_const;
} VariableDeclaration;

DeclarationHeader *variable_declaration_make(AST_Identifier *id, AST_Identifier *type_id, IKS_Type type);

typedef struct VectorDeclaration {
    DeclarationHeader   header;
    AST_Identifier     *identifier;
    AST_Literal        *count;
    IKS_Type            type;
    int                 elem_size_in_bytes;
} VectorDeclaration;

DeclarationHeader *vector_declaration_make(AST_Identifier *id, AST_Literal *count, IKS_Type type);

typedef struct FunctionDeclaration {
    DeclarationHeader   header;
    AST_Identifier     *identifier;
    AST_Identifier     *return_identifier;
    IKS_Type            return_type;
    DeclarationHeader  *first_param;
} FunctionDeclaration;

DeclarationHeader *function_declaration_make(AST_Identifier *id, AST_Identifier *ret_id,
                                             IKS_Type return_type, DeclarationHeader *first_param);

int function_declaration_num_params(FunctionDeclaration *decl);

void declaration_header_free(void *data);

static inline void scope_free(void *d) {
    if (!d) return;
    comp_dict_t *dict = (comp_dict_t*)d;
    dict_free_items(dict, declaration_header_free);
    dict_free(dict);
}

static inline DeclarationHeader *declaration_header_implicit_share(DeclarationHeader *hdr) {
    hdr->ref_count++;
    return hdr;
}

#endif // __SEMANTIC_H__
