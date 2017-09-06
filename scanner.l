/*
  Coloque aqui o identificador do grupo e dos seus membros

  - Leonardo Hahn
*/
%{
#include "parser.h" //arquivo automaticamente gerado pelo bison
int g_line_number = 1;
%}

DIGIT               [0-9]
SPECIALS            (\,|\;|\:|\(|\)|\[|\]|\{|\}|\+|\-|\*|\/|\<|\>|\=|\!|\&|\$|\%|\#|\^)

TK_PR_INT           int
TK_PR_FLOAT         float
TK_PR_BOOL          bool
TK_PR_CHAR          char
TK_PR_STRING        string
TK_PR_IF            if
TK_PR_THEN          then
TK_PR_ELSE          else
TK_PR_WHILE         while
TK_PR_DO            do
TK_PR_INPUT         input
TK_PR_OUTPUT        output
TK_PR_RETURN        return
TK_PR_CONST         cost
TK_PR_STATIC        static
TK_PR_FOREACH       foreach
TK_PR_FOR           for
TK_PR_SWITCH        switch
TK_PR_CASE          case
TK_PR_BREAK         break
TK_PR_CONTINUE      continue
TK_PR_CLASS         class
TK_PR_PRIVATE       private
TK_PR_PUBLIC        public
TK_PR_PROTECTED     protected
TK_OC_LE            \<=
TK_OC_GE            \>=
TK_OC_EQ            ==
TK_OC_NE            !=
TK_OC_AND           \&\&
TK_OC_OR            \|\|
TK_OC_SL            \<\<
TK_OC_SR            \>\>
TK_LIT_INT          [+-]?{DIGIT}+
TK_LIT_FLOAT        [+-]?{DIGIT}+\.{DIGIT}+
TK_LIT_FALSE        false
TK_LIT_TRUE         true
TK_LIT_CHAR         \'([a-zA-Z0-9]|{SPECIALS})\'
TK_LIT_STRING       \"(\\.|[^"])*\"
TK_IDENTIFICADOR    [a-zA-Z_][a-zA-Z_0-9]*
%%

{SPECIALS}          return yytext[0];
{TK_PR_INT}         return TK_PR_INT;
{TK_PR_FLOAT}       return TK_PR_FLOAT;
{TK_PR_BOOL}        return TK_PR_BOOL;
{TK_PR_CHAR}        return TK_PR_CHAR;
{TK_PR_STRING}      return TK_PR_STRING;
{TK_PR_IF}          return TK_PR_IF;
{TK_PR_THEN}        return TK_PR_THEN;
{TK_PR_ELSE}        return TK_PR_ELSE;
{TK_PR_WHILE}       return TK_PR_WHILE;
{TK_PR_DO}          return TK_PR_DO;
{TK_PR_INPUT}       return TK_PR_INPUT;
{TK_PR_OUTPUT}      return TK_PR_OUTPUT;
{TK_PR_RETURN}      return TK_PR_RETURN;
{TK_PR_CONST}       return TK_PR_CONST;
{TK_PR_STATIC}      return TK_PR_STATIC;
{TK_PR_FOREACH}     return TK_PR_FOREACH;
{TK_PR_FOR}         return TK_PR_FOR;
{TK_PR_SWITCH}      return TK_PR_SWITCH;
{TK_PR_CASE}        return TK_PR_CASE;
{TK_PR_BREAK}       return TK_PR_BREAK;
{TK_PR_CONTINUE}    return TK_PR_CONTINUE;
{TK_PR_CLASS}       return TK_PR_CLASS;
{TK_PR_PRIVATE}     return TK_PR_PRIVATE;
{TK_PR_PUBLIC}      return TK_PR_PUBLIC;
{TK_PR_PROTECTED}   return TK_PR_PROTECTED;
{TK_OC_LE}          return TK_OC_LE;
{TK_OC_GE}          return TK_OC_GE;
{TK_OC_EQ}          return TK_OC_EQ;
{TK_OC_NE}          return TK_OC_NE;
{TK_OC_AND}         return TK_OC_AND;
{TK_OC_OR}          return TK_OC_OR;
{TK_OC_SL}          return TK_OC_SL;
{TK_OC_SR}          return TK_OC_SR;
{TK_LIT_INT}        return TK_LIT_INT;
{TK_LIT_FLOAT}      return TK_LIT_FLOAT;
{TK_LIT_FALSE}      return TK_LIT_FALSE;
{TK_LIT_TRUE}       return TK_LIT_TRUE;
{TK_LIT_CHAR}       return TK_LIT_CHAR;
{TK_LIT_STRING}     return TK_LIT_STRING;
{TK_IDENTIFICADOR}  return TK_IDENTIFICADOR;
\n                  g_line_number++;
[/][/].*            {}
[/][*](.|\n)*[*][/] {}
[[:blank:]]+        /* No action */
.                   return TOKEN_ERRO;
%%
