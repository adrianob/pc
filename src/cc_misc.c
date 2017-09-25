#include "cc_misc.h"
#include "cc_dict.h"
#include "main.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

extern int g_line_number;

int comp_get_line_number(void) { return g_line_number; }

void yyerror(char const *mensagem) {
    fprintf(stderr, "%d: %s\n", g_line_number, mensagem); //altere para que apareça a linha
}

void main_init(int argc, char **argv) { dict = dict_new(); }

static void remove_dict_items(comp_dict_t *dict) {
    for (int hash = 0; hash < dict->size; ++hash) {
        while (dict->data[hash]) {
            TableSymbol *value = dict_remove(dict, dict->data[hash]->key);
            table_symbol_free(value);
        }
    }
    assert(dict->occupation == 0);
}

void main_finalize(void) {
    remove_dict_items(dict);
    dict_free(dict);
}

void comp_print_table(void) {
    printf("================ Printing symbols table ==============\n");
    for (int hash = 0; hash < dict->size; ++hash) {
        comp_dict_item_t *search_item = dict->data[hash];
        while (search_item) {
#ifdef AVALIACAO_ETAPA_1
            int *line_number = (int *)search_item->value;
            cc_dict_etapa_1_print_entrada(search_item->key, *line_number);
#elif AVALIACAO_ETAPA_2
            TableSymbol *symbol = (TableSymbol *)search_item->value;
            cc_dict_etapa_2_print_entrada(search_item->key, symbol->line_number, symbol->token_type);
#else
#error "Not implemented yet"
#endif
            search_item = search_item->next;
        }
    }
    printf("======================= Done =========================\n");
}
