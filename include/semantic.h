#ifndef __SEMANTIC_H__
#define __SEMANTIC_H__

#include <stdint.h>

#include "cc_ast.h"
#include "table_symbol.h"
#include "enums.h"
#include "sds.h"

static int g_current_global_address_offset = 0;
static int g_local_address_offset = 0;

void declaration_header_free(void *data);

typedef struct DeclarationHeader {
    DeclarationType type;
    int             ref_count;
    struct DeclarationHeader *next;
} DeclarationHeader;

typedef enum ScopeType {
    ST_GLOBAL,
    ST_LOCAL,
} ScopeType;

typedef struct Scope {
    comp_dict_t *symbols;
    // The next symbol added will have this address offset.
    int current_address_offset;
    ScopeType type;
} Scope;

Scope *scope_make(ScopeType type);

static inline void scope_free(void *d) {
    if (!d) return;
    Scope *s = (Scope*)d;
    dict_free_items(s->symbols, declaration_header_free);
    dict_free(s->symbols);
    free(s);
}

static inline DeclarationHeader *scope_get(Scope *scope, char *name) {
    comp_dict_item_t *entry = dict_get_entry(scope->symbols, name);
    return (entry) ? entry->value : NULL;
}

void scope_add(Scope *scope, char *name, DeclarationHeader *decl_hdr);

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
    int                   address_offset;
} UserTypeDeclaration;

DeclarationHeader *user_type_declaration_make(AST_Identifier *id, UserTypeDefinition *type_definition);

typedef struct VariableDeclaration {
    DeclarationHeader   header;
    IKS_Type            type;
    AST_Identifier     *type_identifier;
    AST_Identifier     *identifier;
    int                 size_in_bytes;
    int                 address_offset;
    /* @Todo(leo): Consider when variable is const. */
    // bool                is_const;
} VariableDeclaration;

DeclarationHeader *variable_declaration_make(AST_Identifier *id, AST_Identifier *type_id, IKS_Type type);

typedef struct VectorDeclaration {
    DeclarationHeader   header;
    AST_Identifier     *identifier;
    AST_Literal        *count;
    IKS_Type            type;
    int                 elem_size_in_bytes;
    int                 address_offset;
} VectorDeclaration;

DeclarationHeader *vector_declaration_make(AST_Identifier *id, AST_Literal *count, IKS_Type type);

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

#endif // __SEMANTIC_H__
