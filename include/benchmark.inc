#include <concat.inc>
#define benchmark(x, f) \
x%name = MACRO_STRINGIFY(f);\
call x%f
#define cast(var, T) \
select type (var) ;\
type is (T)
#define endcast end select