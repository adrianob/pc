#ifndef __OPTIMIZER_H__
#define __OPTIMIZER_H__

#define OPTIMIZER_DEFAULT_WINDOW_SIZE 2

typedef enum OptimizationLevel {
    OPTIMIZATION_LEVEL_NONE,
    OPTIMIZATION_LEVEL_1,
    /* OL_O2, */
} OptimizationLevel;

typedef struct ILOC_Instruction ILOC_Instruction;

ILOC_Instruction *optimize_window(ILOC_Instruction *code, OptimizationLevel opt_lvl, int window_size);

#endif // __OPTIMIZER_H__
