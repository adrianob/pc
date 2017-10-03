/*
  cc_ast.h

  Este arquivo contém as constantes para os tipos dos nós da AST.
*/
#ifndef __CC_AST_H
#define __CC_AST_H

#include <stdbool.h>

#define AST_PROGRAMA 0
#define AST_FUNCAO 1
// Comandos
#define AST_IF_ELSE 2
#define AST_DO_WHILE 3
#define AST_WHILE_DO 4
#define AST_INPUT 5
#define AST_OUTPUT 6
#define AST_ATRIBUICAO 7
#define AST_RETURN 8
#define AST_BLOCO 9
// Condição, Saída, Expressão
#define AST_IDENTIFICADOR 10
#define AST_LITERAL 11
#define AST_ARIM_SOMA 12
#define AST_ARIM_SUBTRACAO 13
#define AST_ARIM_MULTIPLICACAO 14
#define AST_ARIM_DIVISAO 15
#define AST_ARIM_INVERSAO 16       // - (operador unário -)
#define AST_LOGICO_E 17            // &&
#define AST_LOGICO_OU 18           // ||
#define AST_LOGICO_COMP_DIF 19     // !=
#define AST_LOGICO_COMP_IGUAL 20   // ==
#define AST_LOGICO_COMP_LE 21      // <=
#define AST_LOGICO_COMP_GE 22      // >=
#define AST_LOGICO_COMP_L 23       // <
#define AST_LOGICO_COMP_G 24       // >
#define AST_LOGICO_COMP_NEGACAO 25 // !
#define AST_VETOR_INDEXADO                                                     \
    26 // para var[exp] quando o índice exp é acessado no vetor var
#define AST_CHAMADA_DE_FUNCAO 27
#define AST_SHIFT_RIGHT 28
#define AST_SHIFT_LEFT 29

typedef struct comp_dict_item comp_dict_item_t;
typedef struct AST_CommandHeader AST_CommandHeader;
typedef struct AST_ExprHeader AST_ExprHeader;
typedef struct AST_Identifier AST_Identifier;

typedef struct AST_Function {
    int type;

    AST_CommandHeader *first_command;

    struct AST_Function *next;
} AST_Function;

typedef struct AST_Program {
    int type;
    AST_Function *func;
} AST_Program;

/* ------------------------------------------------
 * Commands
 * ------------------------------------------------ */
typedef struct AST_CommandHeader {
    int type;
    struct AST_CommandHeader *next;
} AST_CommandHeader ;

typedef struct AST_CommandIfElse {
    AST_CommandHeader     header;
    AST_ExprHeader       *condition;
    AST_CommandHeader    *true_branch;
    AST_CommandHeader    *false_branch;
} AST_CommandIfElse;

typedef struct AST_While {
    AST_CommandHeader  header;
    AST_CommandHeader *command;
    AST_ExprHeader    *condition;
    bool               is_do_while;
} AST_While;

typedef struct AST_Assignment {
    AST_CommandHeader  header;
    // This field is NULL when is_user_type_assignment is false
    AST_ExprHeader    *type_identifier;
    AST_ExprHeader    *identifier;
    AST_ExprHeader    *value;
    bool is_user_type_assignment;
} AST_Assignment;

typedef struct AST_Return {
    AST_CommandHeader header;
    AST_ExprHeader   *expr;
} AST_Return;

typedef struct AST_Block {
    AST_CommandHeader header;
    AST_CommandHeader *first_command;
} AST_Block;

/* ------------------------------------------------
 * Expressions
 * ------------------------------------------------ */
typedef struct AST_ExprHeader {
    int type;
    struct AST_ExprHeader *next;
} AST_ExprHeader;

typedef struct AST_Identifier {
    AST_ExprHeader    header;
    comp_dict_item_t *entry;
} AST_Identifier;

typedef struct AST_Literal {
    AST_ExprHeader    header;
    comp_dict_item_t *entry;
} AST_Literal;

typedef struct AST_AritExpr {
    AST_ExprHeader   header;
    AST_ExprHeader  *first;
    AST_ExprHeader  *second;
} AST_AritExpr;

typedef struct AST_UnaryAritExpr {
    AST_ExprHeader   header;
    AST_ExprHeader  *expr;
} AST_UnaryAritExpr;

typedef struct AST_LogicExpr {
    AST_ExprHeader  header;
    AST_ExprHeader  *first;
    AST_ExprHeader  *second;
} AST_LogicExpr;

typedef struct AST_UnaryLogicExpr {
    AST_ExprHeader   header;
    AST_ExprHeader  *expr;
} AST_UnaryLogicExpr;

typedef struct AST_IndexedVector {
    AST_ExprHeader  header;
    AST_Identifier *identifier;
    AST_ExprHeader *expr;
} AST_IndexedVector;

typedef struct AST_FunctionCall {
    AST_ExprHeader  header;
    AST_Identifier *identifier;
    AST_ExprHeader *first_param;
} AST_FunctionCall;

#endif
