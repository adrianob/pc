/*
  main.h

  Cabeçalho principal do analisador sintático
*/
#ifndef __MAIN_H
#define __MAIN_H
#include "cc_dict.h"
#include "cc_gv.h"
#include "cc_list.h"
#include "cc_misc.h"
#include "cc_tree.h"
#include "parser.h"
#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
  Protótipos de funções chamadas pelo flex
*/
int yylex();
void yyerror(const char *s);

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

static TableSymbol *table_symbol_make(int line_number, int token_type,
                                      char *raw_value) {
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
        char *value = malloc(strlen(raw_value));
        strncpy(value, raw_value, strlen(raw_value));
        symbol->value_string_or_ident = value;
    } break;

    default:
        assert(false);
    }
    return symbol;
}

static void table_symbol_free(TableSymbol *s) {
    if (s == NULL) return;

    if (s->token_type == POA_LIT_STRING || s->token_type == POA_IDENT) {
        free(s->value_string_or_ident);
    }
    free(s);
}

void cc_dict_etapa_1_print_entrada(char *token, int line);
void cc_dict_etapa_2_print_entrada(char *token, int line, int tipo);
int comp_get_line_number(void);
void comp_print_table(void);

#endif
