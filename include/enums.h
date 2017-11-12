#ifndef __ENUMS_H__
#define __ENUMS_H__

#define IKS_TYPES \
    IKS_TYPE(IKS_UNDEFINED, "Undefined"),              \
        IKS_TYPE(IKS_INT, "Integer"),                  \
        IKS_TYPE(IKS_FLOAT, "Float"),                  \
        IKS_TYPE(IKS_CHAR, "Character"),               \
        IKS_TYPE(IKS_STRING, "String"),                \
        IKS_TYPE(IKS_BOOL, "Boolean"),                 \
        IKS_TYPE(IKS_USER_TYPE, "User-Type")

typedef enum IKS_Type {
#define IKS_TYPE(d, s) d
    IKS_TYPES
#undef IKS_TYPE
} IKS_Type;

static char *iks_type_names[] = {
#define IKS_TYPE(d, s) s
    IKS_TYPES
#undef IKS_TYPE
};

static int get_primitive_type_size(IKS_Type type) {
    switch (type) {
    case IKS_INT: return 4;
    case IKS_FLOAT: return 8;
    case IKS_CHAR: return 1;
    case IKS_BOOL: return 1;
    default: return -1;
    }
}

typedef enum IKS_Error {
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

    // Extra defined errors
    IKS_ERROR_RETURN_FUNCTION,  // Cannot return a function
} IKS_Error;

typedef enum DeclarationType {
    DT_FUNCTION,
    DT_USER_TYPE,
    DT_VECTOR,
    DT_VARIABLE,
} DeclarationType;

typedef enum FieldVisibility {
    FV_PUBLIC,
    FV_PROTECTED,
    FV_PRIVATE,
} FieldVisibility;

#endif // __ENUMS_H__
