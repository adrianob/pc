/*
  Coloque aqui o identificador do grupo e dos seus membros

  - Leonardo Hahn
  - Adriano Benin
*/
%{
#include "parser.h" //arquivo automaticamente gerado pelo bison
#include "cc_dict.h"
void add_to_dict(char * key, int line_number);
int g_line_number = 1;
extern comp_dict_t *dict;
%}

DIGIT               [0-9]
SPECIALS            [,;:()\[\]{}+\-*/<>=!&$%#^]

%%
{SPECIALS}      return yytext[0];
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
                        add_to_dict(yytext, g_line_number);
                        return TK_LIT_FLOAT;
                    }
{DIGIT}+    {
                        add_to_dict(yytext, g_line_number);
                        return TK_LIT_INT;
                    }
\'([a-zA-Z0-9]|{SPECIALS})\'    {
                        char *str = strdup(yytext+1); //remove first char
                        str[strlen(str) - 1] = '\0'; //remove last char
                        add_to_dict(str, g_line_number);
                        free(str);
                        return TK_LIT_CHAR;
                    }
\"(\\.|[^"])*\"     {
                        char *str = strdup(yytext+1); //remove first char
                        str[strlen(str) - 1] = '\0'; //remove last char
                        add_to_dict(str, g_line_number);
                        free(str);
                        return TK_LIT_STRING;
                    }
[a-zA-Z_][a-zA-Z_0-9]*  {
                        add_to_dict(yytext, g_line_number);
                        return TK_IDENTIFICADOR;
                    }
[/][*](.|\n)*[*][/] {
                        for (int i = 0; i < yyleng-1; i++)
                            if (yytext[i] == '\n') g_line_number++;
                    }
[/][/].*            /* No action */
\n                  g_line_number++;
[[:blank:]]+        /* No action */
.               return TOKEN_ERRO;
%%

void add_to_dict(char * key, int line_number){
  int *line = malloc(sizeof(int));
  if(line == NULL) {
    perror("malloc failed");
    exit(EXIT_FAILURE);
  }
  *line = line_number;
  //keep only last entry
  dict_remove(dict, key);
  dict_put(dict, key, line);
}
