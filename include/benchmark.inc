#include <concat.inc>
#define func(f) f, name = MACRO_STRINGIFY(f)
#define benchmark(x, f) \
x%name = MACRO_STRINGIFY(f);\
call x%f