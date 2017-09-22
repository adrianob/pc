#include "cc_misc.h"
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "cc_dict.h"
#include "main.h"

extern int g_line_number;

int
comp_get_line_number(void)
{
    return g_line_number;
}

void
yyerror(char const *mensagem)
{
    fprintf(stderr, "%d: %s\n", g_line_number, mensagem); //altere para que apareça a linha
}

void
main_init(int argc, char **argv)
{
    dict = dict_new();
}

static void
remove_dict_items(comp_dict_t *dict)
{
    for (int hash = 0; hash < dict->size; ++hash) {
        while (dict->data[hash]) {
            TableSymbol *value = dict_remove(dict, dict->data[hash]->key);
            table_symbol_free(value);
        }
    }
    assert(dict->occupation == 0);
}

void
main_finalize(void)
{
    remove_dict_items(dict);
    dict_free(dict);
}

void
comp_print_table(void)
{
    //para cada entrada na tabela de símbolos
    //Etapa 1: chame a função cc_dict_etapa_1_print_entrada
    //implemente esta função
    printf("Printing table\n");
    int i, l;
    for (i = 0, l = dict->size; i < l; ++i) {
        if (dict->data[i]) {
            // aqui é o mesmo problema, onde vc está printando o hash somente, mas pode ser que existam varias
            // keys com o mesmo hash.
            int *line_number = (int*)dict->data[i]->value;
            cc_dict_etapa_1_print_entrada(dict->data[i]->key, *line_number);
        }
    }
    printf("Done.\n");
}
