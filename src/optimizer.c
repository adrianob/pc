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

static ILOC_Instruction *remove_comments(ILOC_Instruction *begin_window,
                                         ILOC_Instruction **end_window, int window_size) {
    ILOC_Instruction *curr = begin_window;
    // Iterate inside the window
    while (curr != (*end_window)->next) {
        if (curr->type == ILOC_IT_COMMENT) {
            curr = remove_instruction(curr);
            *end_window = (*end_window)->next;
        }
        curr = curr->next;
    }

    return begin_window;
}

static inline void initialize_window(ILOC_Instruction *code, int window_size,
                                     ILOC_Instruction **begin, ILOC_Instruction **end) {
    Assert(window_size >= 2);
    *begin = code;
    *end = code;
    int steps = 0;
    while (steps != window_size) {
        *end = (*end)->next;
        steps++;
        Assert(*end);
    }

    Assert(begin != end);
}

static inline void step_window(ILOC_Instruction **begin, ILOC_Instruction **end) {
    *begin = (*begin)->next;
    *end = (*end)->next;
}

static ILOC_Instruction *redundant_instructions_optimization(ILOC_Instruction *begin_window,
                                                             ILOC_Instruction **end_window,
                                                             int window_size) {
    ILOC_Instruction *curr = begin_window;
    // Iterate inside the window
    while (curr != (*end_window)->next) {
        // Remove NOPs, and transfer any label that it contains to the next instruction
        if (curr->opcode == ILOC_NOP && curr != *end_window) {
            // If the nop has no label, then just delete it.
            if (!curr->label) {
                curr = remove_instruction(curr);
                *end_window = (*end_window)->next;
            } else if (!curr->next->label) {
                curr->next->label = curr->label;
                curr->label = NULL;
                // Remove first instruction of the window
                curr = remove_instruction(curr);
                *end_window = (*end_window)->next;
            }
        }

        curr = curr->next;
    }

    return begin_window;
}

static ILOC_Instruction *control_flow_optimization(ILOC_Instruction *begin_window,
                                                   ILOC_Instruction **end_window,
                                                   int window_size) {
    ILOC_Instruction *curr = begin_window;
    // Iterate inside the window
    while (curr != (*end_window)->next) {
        // Remove unnecessary jumps
        // jump => La
        // La: ...
        if (curr->opcode == ILOC_JUMPI && curr != *end_window) {
            Assert(curr->targets[0].type == ILOC_LABEL_REF);
            if (sdscmp(curr->targets[0].label, curr->next->label) == 0) {
                curr = remove_instruction(curr);
                *end_window = (*end_window)->next;
            }
        }

        curr = curr->next;
    }

    return begin_window;
}

ILOC_Instruction *optimize_window(ILOC_Instruction *code, OptimizationLevel opt_lvl, int window_size) {
    if (opt_lvl == OPTIMIZATION_LEVEL_NONE || !code) {
        printf("No optimizations will be run\n");
        return code;
    }

    // slide window
    ILOC_Instruction *begin_window, *end_window;
    initialize_window(code, window_size, &begin_window, &end_window);

    while (end_window) {
        // this is not really an optimization
        begin_window = remove_comments(begin_window, &end_window, window_size);
        begin_window = redundant_instructions_optimization(begin_window, &end_window, window_size);
        begin_window = control_flow_optimization(begin_window, &end_window, window_size);

        step_window(&begin_window, &end_window);
        Assert(begin_window);
    }

    return code;
}
