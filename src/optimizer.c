#include "optimizer.h"

#include <stdio.h>
#include "macros.h"
#include "iloc.h"
#include "sds.h"

#define WINDOW_STEP 1

static ILOC_Instruction *remove_instruction(ILOC_Instruction *code) {
    ILOC_Instruction *to_remove = code;
    ILOC_Instruction *to_return = NULL;

    if (code->prev && code->next) {
        to_return = code->next;
        code->next->prev = code->prev;
        code->prev->next = code->next;
    } else if (code->prev) {
        // This is the last instruction
        to_return = code->prev;
        code->prev->next = NULL;
    } else if (code->next) {
        // This is the first instruction
        to_return = code->next;
        code->next->prev = NULL;
    } else {
        // Only one instruction, do nothing
    }
    iloc_instruction_free(to_remove);
    return to_return;
}

static ILOC_Instruction *remove_comments(ILOC_Instruction *code) {
    ILOC_Instruction *search = code;
    while (search) {
        if (search->type == ILOC_IT_COMMENT) {
            search = remove_instruction(search);
        }
        search = search->next;
    }
    return code;
}

static inline void step_window(ILOC_Instruction **begin, ILOC_Instruction **end) {
    *begin = (*begin)->next;
    *end = (*end)->next;
}

static ILOC_Instruction *remove_redundant_instructions(ILOC_Instruction *code, int window_size) {
    // Initialize window
    ILOC_Instruction *begin_window = code;
    ILOC_Instruction *end_window = code;
    {
        int steps = 0;
        while (steps != window_size) {
            end_window = end_window->next;
            steps++;
            Assert(end_window);
        }
    }
    Assert(begin_window != end_window);

    // Start the actual removing of instructions
    while (end_window) {
        ILOC_Instruction *curr = begin_window;
        // Iterate inside the window
        while (curr != end_window->next) {
            // Remove NOPs
            if (curr->opcode == ILOC_NOP && curr != end_window) {
                // If the nop has no label, then just delete it.
                if (!curr->label) {
                    curr = remove_instruction(curr);
                    end_window = end_window->next;
                } else if (!curr->next->label) {
                    curr->next->label = curr->label;
                    curr->label = NULL;
                    // Remove first instruction of the window
                    curr = remove_instruction(curr);
                    end_window = end_window->next;
                }
            }
            // Remove unnecessary jumps
            if (curr->opcode == ILOC_JUMPI && curr != end_window) {
                Assert(curr->targets[0].type == ILOC_LABEL_REF);
                if (sdscmp(curr->targets[0].label, curr->next->label) == 0) {
                    curr = remove_instruction(curr);
                }
            }

            curr = curr->next;
        }

        step_window(&begin_window, &end_window);
        Assert(begin_window);
    }

    return code;
}

ILOC_Instruction *optimize_window(ILOC_Instruction *code, OptimizationLevel opt_lvl, int window_size) {
    if (opt_lvl == OPTIMIZATION_LEVEL_NONE || !code) {
        printf("No optimizations will be run\n");
        return code;
    }
    // Reverse code

    printf("Will optimize\n");

    code = remove_comments(code);
    code = remove_redundant_instructions(code, window_size);

    return code;
}
