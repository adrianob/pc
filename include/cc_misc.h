#ifndef __MISC_H
#define __MISC_H
#include "cc_dict.h"
#include <stdio.h>

int getLineNumber(void);
void yyerror(char const *mensagem);
void main_init(int argc, char **argv);
void main_finalize(int argc, char **argv);

comp_dict_t *dict;
#endif
