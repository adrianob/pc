#ifndef __SCOPE_H__
#define __SCOPE_H__

#include "semantic.h"

/* void declaration_header_free(void *data); */

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


#endif // __SCOPE_H__
