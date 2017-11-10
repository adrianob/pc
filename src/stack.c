#include <stdlib.h>
#include <stdio.h>
#include "../include/stack.h"

STACK_T * initialize_stack(void){
  return NULL;
}

bool is_empty(STACK_T *head){
  return head == NULL;
}

void * top(STACK_T *head){
  return (head) ? head->data : NULL;
}

void * pop(STACK_T **head_ref){
  STACK_T *node = *head_ref;
  if (!is_empty(node)) {
    *head_ref = node->next;
  }
  return (node) ? node->data : NULL;
}

void push(STACK_T **head_ref, void *element){
  STACK_T *node = (STACK_T *) malloc(sizeof(STACK_T));
  node->data = element;
  node->next = *head_ref;

  *head_ref = node;
}

void for_each(STACK_T **head_ref, iterator iterator){
  STACK_T **node = head_ref;
  while(!is_empty(*node)){
    iterator((*node)->data);
    pop(node);
  }
}

void destroy(STACK_T **head_ref){
  STACK_T *current = *head_ref;
  STACK_T *next;

  while(current){
    next = current->next;
    free(current->data);
    free(current);
    current = next;
  }
  *head_ref = NULL;
}

int list_size(STACK_T *head){
  unsigned int i = 0;
  STACK_T *node = head;
  while(node){
    i++;
    node = node->next;
  }
  return i;
}
