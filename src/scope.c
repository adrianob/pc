#include "scope.h"

Scope *scope_make(ScopeType type) {
    Scope *s = calloc(1, sizeof(*s));
    s->symbols = dict_new();
    s->current_address_offset = 0;
    s->type = type;
    return s;
}

void scope_add(Scope *scope, char *name, DeclarationHeader *decl_hdr) {
    Assert(dict_get_entry(scope->symbols, name) == NULL);

    switch (decl_hdr->type) {
    case DT_USER_TYPE: {
        UserTypeDeclaration *ut = (UserTypeDeclaration*)decl_hdr;
        ut->address_offset = scope->current_address_offset;
        scope->current_address_offset += ut->type_definition->size_in_bytes;
    } break;
    case DT_VARIABLE: {
        VariableDeclaration *v = (VariableDeclaration*)decl_hdr;
        v->address_offset = scope->current_address_offset;
        scope->current_address_offset += v->size_in_bytes;
    } break;
    case DT_VECTOR: {
        VectorDeclaration *v = (VectorDeclaration*)decl_hdr;
        v->address_offset = scope->current_address_offset;
        scope->current_address_offset += get_vector_declaration_size(v);
    } break;
    case DT_FUNCTION: case DT_USER_TYPE_DEFINITION: break; // Do nothing
    }

    dict_put(scope->symbols, name, decl_hdr);
    // TODO(leo): maybe return something useful here.
}

DeclarationHeader *scope_find_declaration_recursive(AST_Identifier *id, STACK_T *scopes, bool *is_global_scope) {
    Scope *scope = stack_top(scopes);
    char *id_key = get_key_from_identifier(id);

    DeclarationHeader *decl_hdr = scope_get(scope, id_key);

    STACK_T *head = scopes;

    while(head) {
        Scope *scope = stack_top(head);
        decl_hdr = scope_get(scope, id_key);
        if (decl_hdr) {
            if (is_global_scope) {
                *is_global_scope = (head->next == NULL) ? true : false;
            }
            return decl_hdr;
        }
        head = head->next;
    }

    return NULL;
}
