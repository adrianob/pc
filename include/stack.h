#ifndef __STACK_H__
#define __STACK_H__
#include <stdbool.h>

typedef bool (*iterator)(void *);

typedef struct _stack {
  void *data;
  struct _stack *next;
} STACK_T;

STACK_T * stack_initialize(void);
bool stack_is_empty(STACK_T *head);
void stack_push(STACK_T **head_ref, void *element);
void * stack_pop(STACK_T **head_ref);
void * stack_top(STACK_T *head);
void stack_for_each(STACK_T **head_ref, iterator iterator);
void stack_destroy(STACK_T **head_ref, void (*free_func)(void*));
int stack_list_size(STACK_T *head);

#endif // __STACK_H__
