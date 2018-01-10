#ifndef __MISC_H
#define __MISC_H
#include <stdio.h>
#include "cc_dict.h"
#include "optimizer.h"

int getLineNumber(void);
void yyerror(char const *mensagem);
void main_init(int argc, char **argv);
void main_finalize(OptimizationLevel opt_lvl);

comp_dict_t *dict;
#endif
