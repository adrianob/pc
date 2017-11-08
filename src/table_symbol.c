#include "table_symbol.h"
#include "main.h"

TableSymbol *table_symbol_make(int line_number, int token_type, char *raw_value) {
    TableSymbol *symbol = malloc(sizeof(*symbol));
    assert(symbol != NULL);
    symbol->line_number = line_number;
    symbol->token_type = token_type;

    switch (token_type) {
    case POA_LIT_INT: {
        symbol->value_int = atoi(raw_value);
    } break;

    case POA_LIT_FLOAT: {
        symbol->value_float = atof(raw_value);
    } break;

    case POA_LIT_CHAR: {
        symbol->value_char = raw_value[0];
    } break;

    case POA_LIT_STRING: {
        char *value = malloc(strlen(raw_value));
        strncpy(value, raw_value, strlen(raw_value));
        symbol->value_string_or_ident = value;
    } break;

    case POA_LIT_BOOL: {
        if (strncmp(raw_value, "true", strlen(raw_value)) == 0) {
            symbol->value_bool = true;
        } else if (strncmp(raw_value, "false", strlen(raw_value)) == 0) {
            symbol->value_bool = false;
        } else {
            assert(false);
        }
    } break;

    case POA_IDENT: {
        // There should be no memory leak here, since the value is freed on the function
        // tyble_symbol_free.
        char *value = calloc(strlen(raw_value)+1, 1);
        strncpy(value, raw_value, strlen(raw_value));
        symbol->value_string_or_ident = value;
    } break;

    default:
        assert(false);
    }
    return symbol;
}

void table_symbol_free(TableSymbol *s) {
    if (s == NULL) return;

    if (s->token_type == POA_LIT_STRING || s->token_type == POA_IDENT) {
        free(s->value_string_or_ident);
    }
    free(s);
}
