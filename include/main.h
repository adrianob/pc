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
#include "cc_ast.h"
#include "semantic.h"
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

void cc_dict_etapa_1_print_entrada(char *token, int line);
void cc_dict_etapa_2_print_entrada(char *token, int line, int tipo);
int comp_get_line_number(void);
void comp_print_table(void);

#endif
