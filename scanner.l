/*
  Coloque aqui o identificador do grupo e dos seus membros

  - Leonardo Hahn
  - Adriano Benin
*/
%{
#include "main.h"
#include "table_symbol.h"
#include "cc_dict.h"
#include "parser.h" //arquivo automaticamente gerado pelo bison
#include "macros.h"

static inline char *dup_without_first_and_last(char *str) {
    char *new_str = strdup(str+1); //remove first char
    new_str[strlen(new_str) - 1] = '\0'; //remove last char
    return new_str;
}

static void add_to_dict(char *lexeme, int line_number, int token_type);

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
\<          return TK_OC_LT;
\>=         return TK_OC_GE;
\>          return TK_OC_GT;
==          return TK_OC_EQ;
!=          return TK_OC_NE;
\&\&        return TK_OC_AND;
\|\|        return TK_OC_OR;
\<\<        return TK_OC_SL;
\>\>        return TK_OC_SR;
false               {
                        add_to_dict(yytext, g_line_number, POA_LIT_BOOL);
                        return TK_LIT_FALSE;
                    }
true                {
                        add_to_dict(yytext, g_line_number, POA_LIT_BOOL);
                        return TK_LIT_TRUE;
                    }
{DIGIT}+            {
                        add_to_dict(yytext, g_line_number, POA_LIT_INT);
                        return TK_LIT_INT;
                    }
{DIGIT}+\.{DIGIT}+  {
                        add_to_dict(yytext, g_line_number, POA_LIT_FLOAT);
                        return TK_LIT_FLOAT;
                    }
\'([a-zA-Z0-9]|{SPECIALS})\'    {
                        char *str = dup_without_first_and_last(yytext);
                        add_to_dict(str, g_line_number, POA_LIT_CHAR);
                        free(str);
                        return TK_LIT_CHAR;
                    }
\"(\\.|[^"])*\"     {
                        char *str = dup_without_first_and_last(yytext);
                        add_to_dict(str, g_line_number, POA_LIT_STRING);
                        free(str);
                        return TK_LIT_STRING;
                    }
[a-zA-Z_][a-zA-Z_0-9]*  {
                        add_to_dict(yytext, g_line_number, POA_IDENT);
                        return TK_IDENTIFICADOR;
                    }
[/][*][^*]*[*]+([^*/][^*]*[*]+)*[/]   {
                        for (int i = 0; i < yyleng-1; i++)
                            if (yytext[i] == '\n') g_line_number++;
                    }
[/][/].*        {}
{SPECIALS}      return yytext[0];
\n              g_line_number++;
[[:blank:]]+    {}
.               return TOKEN_ERRO;
%%

static void add_to_dict(char *lexeme, int line_number, int token_type) {
    TableSymbol *symbol = table_symbol_make(line_number, token_type, lexeme);

    size_t key_size = strlen(lexeme) + 20; // The size of the array has to be enough to contain the key.
    char *key = calloc(key_size, 1);
    snprintf(key, key_size, "%s-%d", lexeme, token_type); // append the lexem and the token type together.

    if (dict_get_entry(dict, key) != NULL) {
        // Entry already exists. Replace it by maintaining the correct pointer value.
        void *old_val = dict_replace(dict, key, symbol);
        table_symbol_free(old_val);
    } else {
        dict_put(dict, key, symbol);
    }

    // Add the pointer to the entry to yylval
    yylval.valor_lexico = dict_get_entry(dict, key);

    free(key);
}
