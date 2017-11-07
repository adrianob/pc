#ifndef __SEMANTIC_H__
#define __SEMANTIC_H__

typedef enum IKS_Type {
    IKS_INT = 1,
    IKS_FLOAT,
    IKS_CHAR,
    IKS_STRING,
    IKS_BOOL,
} IKS_Type;

enum IKS_Error {
    IKS_SUCCESS = 0,
    IKS_ERROR_UNDECLARED,
    IKS_ERROR_DECLARED,

    IKS_ERROR_VARIABLE,
    IKS_ERROR_VECTOR,
    IKS_ERROR_FUNCTION,

    IKS_ERROR_WRONG_TYPE,
    IKS_ERROR_STRING_TO_X,
    IKS_ERROR_CHAR_TO_X,

    IKS_ERROR_MISSING_ARGS,
    IKS_ERROR_EXCESS_ARGS,
    IKS_ERROR_WRONG_TYPE_ARGS,

    IKS_ERROR_WRONG_PAR_INPUT,
    IKS_ERROR_WRONG_PAR_OUTPUT,
    IKS_ERROR_WRONG_PAR_RETURN,
} IKS_Error;

#endif // __SEMANTIC_H__
