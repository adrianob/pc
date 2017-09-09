#include "cc_misc.h"
#include <stdio.h>
#include <stdlib.h>
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
    fprintf(stderr, "%s\n", mensagem); //altere para que apareça a linha
}

void
main_init(int argc, char **argv)
{
    dict = dict_new();
}

void
main_finalize(void)
{
    int i, l;
    for (i = 0, l = dict->size; i < l; ++i) {
        if (dict->data[i]) {
            // o i aqui representa o hash da hash table. Tu na real está removendo só os primeiros items da
            // hash-table. Pode acontecer que duas keys diferentes sejam mapeadas para o mesmo hash.

            // Nota que o occupation nao fica zero no final, como deveria.
            // printf("occupation: %d\n", dict->occupation);
            dict_remove(dict, dict->data[i]->key);
        }
    }
    dict_free(dict);
}

void
comp_print_table(void)
{
    //para cada entrada na tabela de símbolos
    //Etapa 1: chame a função cc_dict_etapa_1_print_entrada
    //implemente esta função
    int i, l;
    for (i = 0, l = dict->size; i < l; ++i) {
        if (dict->data[i]) {
            // aqui é o mesmo problema, onde vc está printando o hash somente, mas pode ser que existam varias
            // keys com o mesmo hash.
            int *line_number = (int*)dict->data[i]->value;
            cc_dict_etapa_1_print_entrada(dict->data[i]->key, *line_number);
        }
    }
}
