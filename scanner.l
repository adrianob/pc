/*
  Coloque aqui o identificador do grupo e dos seus membros

  - Leonardo Hahn
  - Adriano Benin
*/
%{
#include "parser.h" //arquivo automaticamente gerado pelo bison
#include "cc_dict.h"
int g_line_number = 1;
extern comp_dict_t *dict;
%}

DIGIT               [0-9]
SPECIALS            [,;:()\[\]{}+\-*/<>=!&$%#^]

%%
int         return TK_PR_INT;
float       return TK_PR_FLOAT;
bool        return TK_PR_BOOL;
char        return TK_PR_CHAR;
string      return TK_PR_STRING;
if          return TK_PR_IF;
then        return TK_PR_THEN;
else        return TK_PR_ELSE;
while       return TK_PR_WHILE;
do          return TK_PR_DO;
input       return TK_PR_INPUT;
output      return TK_PR_OUTPUT;
return      return TK_PR_RETURN;
const       return TK_PR_CONST;
static      return TK_PR_STATIC;
foreach     return TK_PR_FOREACH;
for         return TK_PR_FOR;
switch      return TK_PR_SWITCH;
case        return TK_PR_CASE;
break       return TK_PR_BREAK;
continue    return TK_PR_CONTINUE;
class       return TK_PR_CLASS;
private     return TK_PR_PRIVATE;
public      return TK_PR_PUBLIC;
protected   return TK_PR_PROTECTED;
\<=         return TK_OC_LE;
\>=         return TK_OC_GE;
==          return TK_OC_EQ;
!=          return TK_OC_NE;
\&\&        return TK_OC_AND;
\|\|        return TK_OC_OR;
\<\<        return TK_OC_SL;
\>\>        return TK_OC_SR;
false       return TK_LIT_FALSE;
true        return TK_LIT_TRUE;
{DIGIT}+\.{DIGIT}+  {
                        dict_put(dict, yytext, (void *) g_line_number);
                        return TK_LIT_FLOAT;
                    }
(\+|\-)?{DIGIT}+    {
                        dict_put(dict, yytext, (void *) g_line_number);
                        return TK_LIT_INT;
                    }
\'([a-zA-Z0-9]|{SPECIALS})\'    {
                                    dict_put(dict, yytext, (void *) g_line_number);
                                    return TK_LIT_CHAR;
                                }
\"(\\.|[^"])*\"     {
                        dict_put(dict, yytext, (void *) g_line_number);
                        return TK_LIT_STRING;
                    }
[a-zA-Z_][a-zA-Z_0-9]*  {
                            // FIXME(leo)?:
                            // A sintaxe correta aqui seria &g_line_number.
                            //
                            // E também é necessário usar malloc aqui para um inteiro.
                            // Nao da pra passar a um pointeiro direto pra g_line_number,
                            // porque essa valor está sempre mudando durante o programa.
                            dict_put(dict, yytext, (void *) g_line_number);
                            return TK_IDENTIFICADOR;
                        }
[/][*](.|\n)*[*][/] {
                        for (int i = 0; i < yyleng-1; i++)
                            if (yytext[i] == '\n') g_line_number++;
                    }
[/][/].*            /* No action */
\n                  g_line_number++;
[[:blank:]]+        /* No action */
{SPECIALS}      return yytext[0];
.               return TOKEN_ERRO;
%%
