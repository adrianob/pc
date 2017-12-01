#include <stdlib.h>
#include <stdio.h>
#include "../include/stack.h"

STACK_T * stack_initialize(void){
  return NULL;
}

bool stack_is_empty(STACK_T *head){
  return head == NULL;
}

void * stack_top(STACK_T *head){
  return (head) ? head->data : NULL;
}

void * stack_pop(STACK_T **head_ref){
  STACK_T *node = *head_ref;
  if (!stack_is_empty(node)) {
    *head_ref = node->next;
  }

  if (node) {
      void *data = node->data;
      free(node);
      return data;
  }

  return NULL;
}

void stack_push(STACK_T **head_ref, void *element){
  STACK_T *node = (STACK_T *) malloc(sizeof(STACK_T));
  node->data = element;
  node->next = *head_ref;

  *head_ref = node;
}

void stack_for_each(STACK_T **head_ref, iterator iterator){
  STACK_T **node = head_ref;
  while(!stack_is_empty(*node)){
    iterator((*node)->data);
    stack_pop(node);
  }
}

void stack_destroy(STACK_T **head_ref, void (*free_func)(void*)){
  STACK_T *current = *head_ref;
  STACK_T *next;

  while(current){
    next = current->next;
    if (free_func) free_func(current->data);
    free(current);
    current = next;
  }
  *head_ref = NULL;
}

int stack_list_size(STACK_T *head){
  unsigned int i = 0;
  STACK_T *node = head;
  while(node){
    i++;
    node = node->next;
  }
  return i;
}
