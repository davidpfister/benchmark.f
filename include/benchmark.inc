#include <concat.inc>
#define benchmark(x, f) \
x%name = MACRO_STRINGIFY(f);\
call x%f