#ifndef MACROS_H
#define MACROS_H

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

#endif // MACROS_H
