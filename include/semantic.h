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
} DeclarationHeader;

typedef struct UserTypeField {
    IKS_Type           type;
    FieldVisibility    visibility;
    AST_Header        *identifier;
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
    AST_Identifier     *return_identifier;
    AST_Identifier     *identifier;
} VariableDeclaration;

static DeclarationHeader *variable_declaration_make(AST_Identifier *id, AST_Identifier *ret_id, IKS_Type type) {
    VariableDeclaration *d = calloc(1, sizeof(*d));
    d->header.type = DT_VARIABLE;
    d->identifier = id;
    d->return_identifier = ret_id;
    d->type = type;
    return &d->header;
}

typedef struct VectorDeclaration {
    DeclarationHeader   header;
    AST_Identifier     *identifier;
    AST_Literal        *count;
    IKS_Type            type;
} VectorDeclaration;

static DeclarationHeader *vector_declaration_make(AST_Identifier *id, AST_Literal *count, IKS_Type type) {
    VectorDeclaration *d = calloc(1, sizeof(*d));
    d->header.type = DT_VECTOR;
    d->identifier = id;
    d->count = count;
    d->type = type;
    return &d->header;
}


/* static AST_Header *scope_lookup(Array(comp_dict_t) scopes, AST_Identifier *identifier) { */
/*     for (int i = array_len(scopes)-1; i >= 0; --i) { */
/* 	AST_Header *hdr = (AST_Header*)dict_get(scopes[i], identifier); */

/* 	if (!hdr) continue; // Not in this current scope. */

/* 	return hdr; */
/*     } */
/*     return NULL; */
/* } */

/* static Array(SemanticError) semantic_analyse_tree(AST_Program *program) { */
/*     // Initialize stack of scopes. */
/*     Array(comp_dict_t) scopes; */
/*     array_init(scopes); */
/*     // Initialize array of errors to return. */
/*     Array(SemanticError) errors;  */
/*     array_init(errors); */

/*     AST_Function *func = program->first_func; */
/*     while (func) { */
/* 	// Analyse this function */
/* 	if (func->return_type == IKS_UNDECIDED) { */
/* 	    Assert(func->return_identifier); */

/* 	    { */
/* 		func */
/* 	    } */


/* 	    // Analyse the return type of the function */
/* 	    { */
/* 		AST_Header *hdr = scope_lookup(func->return_identifier); */

/* 		if (hdr) { */
/* 		    // Symbol is declared */
/* 		    if (hdr->type == AST_FUNCAO) { */
/* 			SemanticError err; */
/* 			err.type = IKS_WRONG_TYPE; */
/* 			err.description = "Cannot return a function."; */
/* 			array_push(errors, err); */
/* 		    } else if (hdr->type == AST_LITERAL) { */
/* 			func->return_type = ((AST_Literal*)hdr)->semantic_type; */
/* 		    } else { */
/* 			Assert(false); */
/* 		    } */
/* 		} else { */
/* 		    // Undeclared symbol. */
/* 		    TableSymbol *id_ts = (TableSymbol*)func->identifier->entry; */
/* 		    TableSymbol *return_id_ts = (TableSymbol*)func->return_identifier->entry; */

/* 		    char *desc_format = "Return identifier %s is undeclared for function %s."; */
/* 		    int desc_size = strlen(id_ts->name) + strlen(return_id_ts->name) + strlen(desc_format) - 4 + 1; */

/* 		    SemanticError err; */
/* 		    err.type = IKS_ERROR_UNDECLARED; */
/* 		    err.description = calloc(desc_size, 1); */
/* 		    snprintf(err.description, desc_size, desc_format, return_id_ts->name, id_ts->name); */

/* 		    array_push(errors, err); */
/* 		} */
/* 	    } */
/* 	} */
	
/*     } */

/*     tree_free(global_scope); */
/*     return errors; */
/* } */

#endif // __SEMANTIC_H__
