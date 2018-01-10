#ifndef __OPTIMIZER_H__
#define __OPTIMIZER_H__

typedef struct ILOC_Instruction ILOC_Instruction;

ILOC_Instruction *optimize_window(ILOC_Instruction *code, int window_size);

#endif // __OPTIMIZER_H__
