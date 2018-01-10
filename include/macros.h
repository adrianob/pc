#ifndef MACROS_H
#define MACROS_H

#include <stdlib.h>

#ifndef Assert
#  ifndef COMPILADORES_PROD
#    define Assert2(cond, file, number) do {                         \
        if (!(cond)) {                                                  \
            fprintf(stderr, "******************************\n");        \
            fprintf(stderr, "*********** ASSERT ***********\n");        \
            fprintf(stderr, "**                            \n");        \
            fprintf(stderr, "**  Condition: %s\n", #cond);              \
            fprintf(stderr, "**  %s (line %d)\n", file, number);        \
            fprintf(stderr, "**                            \n");        \
            fprintf(stderr, "******************************\n");        \
            fprintf(stderr, "******************************\n");        \
            fflush(stderr);                                             \
            __builtin_trap();                                           \
        }                                                               \
    } while(0)
#    define Assert(cond) Assert2(cond, __FILE__, __LINE__)
#  else
#    define Assert(cond)
#  endif // LT_DEBUG
#endif // LT_Assert

typedef struct ArrayHeader {
    int len;
    int cap;
} ArrayHeader;

// Dynamic array based on gbArray (https://github.com/gingerBill/gb/blob/master/gb.h)
#define Array(T) T *

#define ARRAY_GROW_FORMULA(x) (x*1.5f)
#define ARRAY_HEADER(ptr) ((ArrayHeader*)ptr - 1)
#define ARRAY_INIT_CAP 8
#define array_len(ptr) (ARRAY_HEADER(ptr)->len)
#define array_cap(ptr) (ARRAY_HEADER(ptr)->cap)

static void *array__grow(void *ptr, int new_cap, int type_size) {
    Assert(new_cap > array_cap(ptr));

    int old_cap = array_cap(ptr);
    array_cap(ptr) = new_cap;

    ArrayHeader *new_area = realloc(ARRAY_HEADER(ptr), sizeof(ArrayHeader)+new_cap*type_size);
    Assert(new_area); // Assert that there is enough memory

    return new_area+1;
}

static void array_free(void *ptr) {
    free(ARRAY_HEADER(ptr));
}

#define array_init(ptr) do {						\
	void **arr = (void**)&(ptr);					\
	ArrayHeader *ah = malloc(sizeof(ArrayHeader)+ARRAY_INIT_CAP*sizeof(*(ptr))); \
	ah->cap = ARRAY_INIT_CAP;					\
	ah->len = 0;							\
	*arr = (void*)(ah+1);						\
    } while(0)

#define array_push(ptr, val) do {				\
	if (array_len((ptr)) == array_cap((ptr))) {		\
	    int new_cap = ARRAY_GROW_FORMULA(array_cap((ptr)));	\
	    ptr = array__grow(ptr, new_cap, sizeof(*(ptr)));	\
	}							\
	Assert(array_cap((ptr)) > array_len((ptr)));		\
								\
	ptr[array_len((ptr))++] = val;				\
    } while(0)

#endif // MACROS_H
