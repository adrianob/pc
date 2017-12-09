#ifndef __ILOC_H__
#define __ILOC_H__

#include <stdlib.h>
#include <stdio.h>
#include "sds.h"
#include "macros.h"
#include "stack.h"
#include "cc_ast.h"
#include "semantic.h"
#include "scope.h"

typedef enum ILOC_OperandType {
    ILOC_REGISTER,
    ILOC_NUMBER,
    ILOC_LABEL_REF
} ILOC_OperandType;

typedef struct ILOC_OperandHeader {
    ILOC_OperandType type;
} ILOC_OperandHeader;

typedef enum ILOC_RegisterType {
    ILOC_RT_RBSS,
    ILOC_RT_RARP,
    ILOC_RT_GENERIC
} ILOC_RegisterType;

typedef struct ILOC_Register {
    ILOC_OperandHeader header;
    ILOC_RegisterType  register_type;
    int                number;
} ILOC_Register;

typedef struct ILOC_LabelRef {
    ILOC_OperandHeader header;
    sds                ref;
} ILOC_LabelRef;

typedef struct ILOC_Number {
    ILOC_OperandHeader header;
    int                value;
} ILOC_Number;

#define ILOC_OPCODES \
    ILOC_OPCODE(ILOC_NOP, "nop"),               \
    ILOC_OPCODE(ILOC_ADD, "add"),               \
    ILOC_OPCODE(ILOC_SUB, "sub"),           \
    ILOC_OPCODE(ILOC_MULT, "mult"),          \
    ILOC_OPCODE(ILOC_DIV, "div"),           \
    ILOC_OPCODE(ILOC_ADDI, "addI"),          \
    ILOC_OPCODE(ILOC_SUBI, "subI"),          \
    ILOC_OPCODE(ILOC_RSUBI, "rsubI"),         \
    ILOC_OPCODE(ILOC_MULTI, "multI"),         \
    ILOC_OPCODE(ILOC_DIVI, "divI"),          \
    ILOC_OPCODE(ILOC_RDIVI, "rdivI"),         \
    ILOC_OPCODE(ILOC_LSHIFT, "lshift"),        \
    ILOC_OPCODE(ILOC_LSHIFTI, "lshiftI"),       \
    ILOC_OPCODE(ILOC_RSHIFT, "rshift"),        \
    ILOC_OPCODE(ILOC_RSHIFTI, "rshiftI"),       \
    ILOC_OPCODE(ILOC_AND, "and"),           \
    ILOC_OPCODE(ILOC_ANDI, "andI"),          \
    ILOC_OPCODE(ILOC_OR, "or"),            \
    ILOC_OPCODE(ILOC_ORI, "orI"),           \
    ILOC_OPCODE(ILOC_XOR, "xor"),           \
    ILOC_OPCODE(ILOC_XORI, "xorI"),          \
    ILOC_OPCODE(ILOC_LT, "cmpLT"),          \
    ILOC_OPCODE(ILOC_LE, "cmpLE"),          \
    ILOC_OPCODE(ILOC_EQ, "cmpEQ"),          \
    ILOC_OPCODE(ILOC_GE, "cmpGE"),          \
    ILOC_OPCODE(ILOC_GT, "cmpGT"),          \
    ILOC_OPCODE(ILOC_NE, "cmpNE"),          \
    ILOC_OPCODE(ILOC_LOADI, "loadI"),         \
    ILOC_OPCODE(ILOC_LOAD, "load"),          \
    ILOC_OPCODE(ILOC_LOADAI, "loadAI"),        \
    ILOC_OPCODE(ILOC_LOADAO, "loadA0"),        \
    ILOC_OPCODE(ILOC_STORE, "store"),         \
    ILOC_OPCODE(ILOC_STOREAI, "storeAI"),       \
    ILOC_OPCODE(ILOC_STOREAO, "storeA0"),       \
    ILOC_OPCODE(ILOC_CSTORE, "cstore"),        \
    ILOC_OPCODE(ILOC_CSTOREAI, "cstoreAI"),      \
    ILOC_OPCODE(ILOC_CSTOREAO, "cstoreA0"),      \
    ILOC_OPCODE(ILOC_I2I, "i2i"),           \
    ILOC_OPCODE(ILOC_C2C, "c2c"),           \
    ILOC_OPCODE(ILOC_C2I, "c2i"),           \
    ILOC_OPCODE(ILOC_I2C, "i2c"),

typedef enum ILOC_OpCode {
#define ILOC_OPCODE(e, s) e
    ILOC_OPCODES
#undef ILOC_OPCODE
} ILOC_OpCode;

static const char *iloc_opcode_names[] = {
#define ILOC_OPCODE(e, s) s
    ILOC_OPCODES
#undef ILOC_OPCODE
};

typedef struct ILOC_Instruction {
    sds                        label;
    ILOC_OpCode                opcode;
    Array(ILOC_OperandHeader*) sources;
    Array(ILOC_OperandHeader*) targets;

    struct ILOC_Instruction *prev;
    struct ILOC_Instruction *next;
} ILOC_Instruction;

static inline int get_next_register_number() {
    static int next = 0;
    return next++;
}

ILOC_OperandHeader *iloc_number_make(int value);
ILOC_OperandHeader *iloc_register_make(ILOC_RegisterType register_type);
ILOC_Instruction *iloc_instruction_make(void);
ILOC_Instruction *iloc_instruction_append_after(ILOC_Instruction *inst, ILOC_Instruction *new_inst);
ILOC_Instruction *iloc_instruction_append_before(ILOC_Instruction *inst, ILOC_Instruction *new_inst);
ILOC_Instruction *ast_assignment_generate_code(AST_Assignment *assignment, STACK_T *scope_stack);
ILOC_Instruction *ast_function_generate_code(AST_Function *func, STACK_T *scope_stack);
ILOC_Instruction *iloc_generate_code(AST_Program *program);
ILOC_Instruction *iloc_instruction_from_declaration(char *symbol_name, DeclarationHeader *decl_hdr);
sds iloc_stringify(ILOC_Instruction *code);

#endif // __ILOC_H__
