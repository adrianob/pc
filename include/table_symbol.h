#ifndef TABLE_SYMBOL_H
#define TABLE_SYMBOL_H

#include <stdbool.h>

typedef struct TableSymbol {
    int line_number;
    int token_type;
    union {
        int value_int;
        float value_float;
        char *value_string_or_ident;
        char value_char;
        bool value_bool;
    };
} TableSymbol;

TableSymbol *table_symbol_make(int line_number, int token_type, char *raw_value);
void table_symbol_free(TableSymbol *s);

#endif // TABLE_SYMBOL_H
