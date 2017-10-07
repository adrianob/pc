#ifndef TABLE_SYMBOL_H
#define TABLE_SYMBOL_H

#include <stdbool.h>

/*
  Constantes a serem utilizadas para diferenciar os lexemas que estão
  registrados na tabela de símbolos.
*/
#define POA_LIT_INT 1
#define POA_LIT_FLOAT 2
#define POA_LIT_CHAR 3
#define POA_LIT_STRING 4
#define POA_LIT_BOOL 5
#define POA_IDENT 6

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
