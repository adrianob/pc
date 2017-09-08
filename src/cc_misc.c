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
    //implemente esta função com rotinas de inicialização, se necessário
  dict = dict_new();
}

void
main_finalize(void)
{
    //implemente esta função com rotinas de inicialização, se necessário
    int i, l;
    for (i = 0, l = dict->size; i < l; ++i) {
      if (dict->data[i]) {
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
      cc_dict_etapa_1_print_entrada(dict->data[i]->key, dict->data[i]->value);
    }
  }
}
