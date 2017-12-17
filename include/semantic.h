#ifndef __SEMANTIC_H__
#define __SEMANTIC_H__

#include <stdint.h>

#include "cc_ast.h"
#include "table_symbol.h"
#include "enums.h"
#include "sds.h"

typedef struct ILOC_Operand ILOC_Operand;

static int g_current_global_address_offset = 0;
static int g_local_address_offset = 0;

void declaration_header_free(void *data);

typedef struct DeclarationHeader {
    DeclarationType type;
    int             ref_count;
    bool            is_parameter;
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

typedef struct UserTypeDefinition {
    DeclarationHeader     header;
    AST_Identifier       *identifier;
    UserTypeField        *first_field;
    int                   size_in_bytes;
} UserTypeDefinition;

DeclarationHeader *user_type_definition_make(AST_Identifier *id, UserTypeField *first_field);

typedef struct UserTypeDeclaration {
    DeclarationHeader     header;
    UserTypeDefinition   *type_definition;
    AST_Identifier       *identifier;
    unsigned int         address_offset;
} UserTypeDeclaration;

DeclarationHeader *user_type_declaration_make(AST_Identifier *id, UserTypeDefinition *type_definition);

typedef struct VariableDeclaration {
    DeclarationHeader   header;
    IKS_Type            type;
    AST_Identifier     *type_identifier;
    AST_Identifier     *identifier;
    int                 size_in_bytes;
    unsigned int        address_offset;
    /* @Todo(leo): Consider when variable is const. */
    // bool                is_const;
} VariableDeclaration;

DeclarationHeader *variable_declaration_make(AST_Identifier *id,
                                             AST_Identifier *type_id,
                                             IKS_Type type);

typedef struct VectorDeclaration {
    DeclarationHeader   header;
    AST_Identifier     *identifier;
    Array(AST_Literal*) dimensions;
    IKS_Type            type;
    int                 elem_size_in_bytes;
    unsigned int        address_offset;
} VectorDeclaration;

DeclarationHeader *vector_declaration_make(AST_Identifier *id, IKS_Type type);
unsigned long get_vector_declaration_size(VectorDeclaration *v);

typedef struct FunctionDeclaration {
    DeclarationHeader   header;
    AST_Identifier     *identifier;
    AST_Identifier     *return_identifier;
    UserTypeDefinition *return_type_definition;
    IKS_Type            return_type;
    DeclarationHeader  *first_param;
} FunctionDeclaration;

DeclarationHeader *function_declaration_make(AST_Identifier *id, AST_Identifier *ret_id,
                                             IKS_Type return_type, UserTypeDefinition *return_type_definition,
                                             DeclarationHeader *first_param);

int function_declaration_num_params(FunctionDeclaration *decl);

static inline DeclarationHeader *declaration_header_implicit_share(DeclarationHeader *hdr) {
    hdr->ref_count++;
    return hdr;
}

static inline unsigned long declaration_header_get_address_offset(DeclarationHeader *hdr) {
    switch (hdr->type) {
    case DT_VECTOR: return ((VectorDeclaration*)hdr)->address_offset;
    case DT_VARIABLE: return ((VariableDeclaration*)hdr)->address_offset;
    case DT_USER_TYPE: return ((UserTypeDeclaration*)hdr)->address_offset;
    // These types have no offset.
    case DT_USER_TYPE_DEFINITION:
    case DT_FUNCTION:
    default: Assert(false);
    }
}

#endif // __SEMANTIC_H__
