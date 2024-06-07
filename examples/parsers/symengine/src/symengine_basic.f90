module symengine_basic

use iso_c_binding, only: c_int, c_long, c_double, c_ptr, c_null_ptr, c_null_char, c_f_pointer, c_associated
use iso_fortran_env, only: int32, int64, real32, real64
use symengine_interface
use exceptions
use conversion


type Basic
    type(c_ptr) :: ptr = c_null_ptr
contains
    procedure :: str, subs, evalf, dbl
    procedure :: basic_assign, basic_assign_i, basic_assign_i64, basic_assign_f, basic_assign_d
    procedure :: basic_assign_c, basic_assign_c64
    procedure :: basic_add, basic_sub, basic_neg, basic_mul
    procedure :: basic_div, basic_pow, basic_eq, basic_neq
    procedure, pass(this) :: basic_eq_i_left, basic_eq_i_right, basic_eq_i64_left, basic_eq_i64_right
    procedure, pass(this) :: basic_eq_f_left, basic_eq_f_right, basic_eq_d_left, basic_eq_d_right
    procedure, pass(this) :: basic_eq_c_left, basic_eq_c_right, basic_eq_c64_left, basic_eq_c64_right
    procedure, pass(this) :: basic_neq_i_left, basic_neq_i_right, basic_neq_i64_left, basic_neq_i64_right
    procedure, pass(this) :: basic_neq_f_left, basic_neq_f_right, basic_neq_d_left, basic_neq_d_right
    procedure, pass(this) :: basic_neq_c_left, basic_neq_c_right, basic_neq_c64_left, basic_neq_c64_right
    procedure, pass(this) :: free_symbols => basic_free_symbols
    procedure, pass(this) :: basic_add_i_left, basic_add_i_right, basic_add_i64_left, basic_add_i64_right
    procedure, pass(this) :: basic_add_f_left, basic_add_f_right, basic_add_d_left, basic_add_d_right
    procedure, pass(this) :: basic_add_c_left, basic_add_c_right, basic_add_c64_left, basic_add_c64_right
    procedure, pass(this) :: basic_sub_i_left, basic_sub_i_right, basic_sub_i64_left, basic_sub_i64_right
    procedure, pass(this) :: basic_sub_f_left, basic_sub_f_right, basic_sub_d_left, basic_sub_d_right
    procedure, pass(this) :: basic_sub_c_left, basic_sub_c_right, basic_sub_c64_left, basic_sub_c64_right
    procedure, pass(this) :: basic_mul_i_left, basic_mul_i_right, basic_mul_i64_left, basic_mul_i64_right
    procedure, pass(this) :: basic_mul_f_left, basic_mul_f_right, basic_mul_d_left, basic_mul_d_right
    procedure, pass(this) :: basic_mul_c_left, basic_mul_c_right, basic_mul_c64_left, basic_mul_c64_right
    procedure, pass(this) :: basic_div_i_left, basic_div_i_right, basic_div_i64_left, basic_div_i64_right
    procedure, pass(this) :: basic_div_f_left, basic_div_f_right, basic_div_d_left, basic_div_d_right
    procedure, pass(this) :: basic_div_c_left, basic_div_c_right, basic_div_c64_left, basic_div_c64_right
    procedure, pass(this) :: basic_pow_i_left, basic_pow_i_right, basic_pow_i64_left, basic_pow_i64_right
    procedure, pass(this) :: basic_pow_f_left, basic_pow_f_right, basic_pow_d_left, basic_pow_d_right
    procedure, pass(this) :: basic_pow_c_left, basic_pow_c_right, basic_pow_c64_left, basic_pow_c64_right
    generic :: assignment(=) => basic_assign, basic_assign_i, basic_assign_i64, basic_assign_f, basic_assign_d
    generic :: assignment(=) => basic_assign_c, basic_assign_c64
    generic :: operator(+) => basic_add
    generic :: operator(+) => basic_add_i_left, basic_add_i_right, basic_add_i64_left, basic_add_i64_right
    generic :: operator(+) => basic_add_f_left, basic_add_f_right, basic_add_d_left, basic_add_d_right
    generic :: operator(+) => basic_add_c_left, basic_add_c_right, basic_add_c64_left, basic_add_c64_right
    generic :: operator(-) => basic_sub
    generic :: operator(-) => basic_sub_i_left, basic_sub_i_right, basic_sub_i64_left, basic_sub_i64_right
    generic :: operator(-) => basic_sub_f_left, basic_sub_f_right, basic_sub_d_left, basic_sub_d_right
    generic :: operator(-) => basic_sub_c_left, basic_sub_c_right, basic_sub_c64_left, basic_sub_c64_right
    generic :: operator(-) => basic_neg
    generic :: operator(*) => basic_mul
    generic :: operator(*) => basic_mul_i_left, basic_mul_i_right, basic_mul_i64_left, basic_mul_i64_right
    generic :: operator(*) => basic_mul_f_left, basic_mul_f_right, basic_mul_d_left, basic_mul_d_right
    generic :: operator(*) => basic_mul_c_left, basic_mul_c_right, basic_mul_c64_left, basic_mul_c64_right
    generic :: operator(/) => basic_div
    generic :: operator(/) => basic_div_i_left, basic_div_i_right, basic_div_i64_left, basic_div_i64_right
    generic :: operator(/) => basic_div_f_left, basic_div_f_right, basic_div_d_left, basic_div_d_right
    generic :: operator(/) => basic_div_c_left, basic_div_c_right, basic_div_c64_left, basic_div_c64_right
    generic :: operator(**) => basic_pow
    generic :: operator(**) => basic_pow_i_left, basic_pow_i_right, basic_pow_i64_left, basic_pow_i64_right
    generic :: operator(**) => basic_pow_f_left, basic_pow_f_right, basic_pow_d_left, basic_pow_d_right
    generic :: operator(**) => basic_pow_c_left, basic_pow_c_right, basic_pow_c64_left, basic_pow_c64_right
    generic :: operator(==) => basic_eq, basic_eq_i_left, basic_eq_i_right, basic_eq_i64_left, basic_eq_i64_right
    generic :: operator(==) => basic_eq_f_left, basic_eq_f_right, basic_eq_d_left, basic_eq_d_right
    generic :: operator(==) => basic_eq_c_left, basic_eq_c_right, basic_eq_c64_left, basic_eq_c64_right
    generic :: operator(/=) => basic_neq, basic_neq_i_left, basic_neq_i_right, basic_neq_i64_left, basic_neq_i64_right
    generic :: operator(/=) => basic_neq_f_left, basic_neq_f_right, basic_neq_d_left, basic_neq_d_right
    generic :: operator(/=) => basic_neq_c_left, basic_neq_c_right, basic_neq_c64_left, basic_neq_c64_right
    final :: basic_free
end type

interface Basic
   module procedure basic_new
end interface

type SetBasic
    type(c_ptr) :: ptr = c_null_ptr
contains
    procedure :: setbasic_assign
    procedure, pass(this) :: size => setbasic_size
    procedure, pass(this) :: get => setbasic_get
    generic :: assignment(=) => setbasic_assign
    final :: setbasic_free
end type

interface SetBasic
   module procedure setbasic_new
end interface

type, extends(Basic) :: SymInteger
contains
    procedure :: get
end type SymInteger

interface SymInteger
    module procedure integer_new
    module procedure integer_new_i64
end interface

type, extends(Basic) :: RealDouble
end type RealDouble

interface RealDouble
    module procedure real_new_d
    module procedure real_new_f
    module procedure real_new_i
    module procedure real_new_i64
end interface

type, extends(Basic) :: SymComplex
end type SymComplex

interface SymComplex
    module procedure complex_new
    module procedure complex_new_i_i
    module procedure complex_new_i64_i
    module procedure complex_new_i_i64
    module procedure complex_new_i64_i64
end interface

type, extends(Basic) :: ComplexDouble
end type ComplexDouble

interface ComplexDouble
    module procedure complex_new_f_f
    module procedure complex_new_f_d
    module procedure complex_new_d_f
    module procedure complex_new_d_d
    module procedure complex_new_cmplx
    module procedure complex_new_cmplx_d
end interface


private
public :: ptr, Basic, SetBasic, SymInteger, RealDouble, SymComplex, ComplexDouble

contains

function integer_new(i) result(res)
    integer :: i
    integer(c_long) :: j
    integer(c_long) :: exception
    type(SymInteger) :: res
    j = int(i)
    res%ptr = c_basic_new_heap()
    exception = c_integer_set_si(res%ptr, j)
    call handle_exception(exception)
end function

function integer_new_i64(i) result(res)
    integer(kind=int64) :: i
    integer(c_long) :: j
    integer(c_long) :: exception
    type(SymInteger) :: res
    j = int(i)
    res%ptr = c_basic_new_heap()
    exception = c_integer_set_si(res%ptr, j)
    call handle_exception(exception)
end function

function get(this) result(res)
    class(SymInteger) :: this
    integer :: res
    res = int(c_integer_get_si(this%ptr))
end function

function real_new_d(d) result(res)
    real(c_double) :: d
    integer(c_long) :: exception
    type(RealDouble) :: res

    res%ptr = c_basic_new_heap()
    exception = c_real_double_set_d(res%ptr, d)
    call handle_exception(exception)
end function

function real_new_f(f) result(res)
    real :: f
    integer(c_long) :: exception
    type(RealDouble) :: res
    res%ptr = c_basic_new_heap()
    exception = c_real_double_set_d(res%ptr, real(f, 8))
    call handle_exception(exception)
end function

function real_new_i(x) result(res)
    integer(kind=int32) :: x
    type(RealDouble) :: res
    res = real_new_d(real(x, 8))
end function

function real_new_i64(x) result(res)
    integer(kind=int64) :: x
    type(RealDouble) :: res
    res = real_new_d(real(x, 8))
end function

function complex_new(re, im) result(res)
    class(basic) :: re, im
    type(basic) :: res 
    integer(c_long) :: exception
    res%ptr = c_basic_new_heap()
    exception = c_complex_set(res%ptr, re%ptr, im%ptr)
    call handle_exception(exception)
end function

function complex_new_i_i(re, im) result(res)
    integer(kind=int32) :: re, im
    type(basic) :: res
    res = complex_new(SymInteger(re), SymInteger(im))
end function

function complex_new_i_i64(re, im) result(res)
    integer(kind=int32) :: re
    integer(kind=int64) :: im
    type(basic) :: res
    res = complex_new(SymInteger(re), SymInteger(im))
end function

function complex_new_i64_i(re, im) result(res)
    integer(kind=int64) :: re
    integer(kind=int32) :: im
    type(basic) :: res
    res = complex_new(SymInteger(re), SymInteger(im))
end function

function complex_new_i64_i64(re, im) result(res)
    integer(kind=int64) :: re, im
    type(SymComplex) :: res
    res = complex_new(SymInteger(re), SymInteger(im))
end function

function complex_new_f_f(x, y) result(res)
    real(kind=real32) :: x
    real(kind=real32) :: y
    type(ComplexDouble) :: res
    type(Basic) :: i
    i = SymComplex(SymInteger(0), SymInteger(1))
    res = RealDouble(x) + i * RealDouble(y)
end function

function complex_new_d_f(x, y) result(res)
    real(kind=real64) :: x
    real(kind=real32) :: y
    type(ComplexDouble) :: res
    type(Basic) :: i
    i = SymComplex(SymInteger(0), SymInteger(1))
    res = RealDouble(x) + i * RealDouble(y)
end function

function complex_new_f_d(x, y) result(res)
    real(kind=real32) :: x
    real(kind=real64) :: y
    type(ComplexDouble) :: res
    type(Basic) :: i
    i = SymComplex(SymInteger(0), SymInteger(1))
    res = RealDouble(x) + i * RealDouble(y)
end function

function complex_new_d_d(x, y) result(res)
    real(kind=real64) :: x
    real(kind=real64) :: y
    type(ComplexDouble) :: res
    type(Basic) :: i
    i = SymComplex(SymInteger(0), SymInteger(1))
    res = RealDouble(x) + i * RealDouble(y)
end function

function complex_new_cmplx(z) result(res)
    complex(kind=real32) :: z
    type(ComplexDouble) :: res
    type(RealDouble) :: x, y
    type(Basic) :: i
    x = RealDouble(z%re)
    y = RealDouble(z%im)
    i = SymComplex(SymInteger(0), SymInteger(1))
    res = x + y * i
end function

function complex_new_cmplx_d(z) result(res)
    complex(kind=real64) :: z
    type(ComplexDouble) :: res
    type(RealDouble) :: x, y
    type(Basic) :: i
    x = RealDouble(z%re)
    y = RealDouble(z%im)
    i = SymComplex(SymInteger(0), SymInteger(1))
    res = x + y * i
end function

function ptr(a) result(res)
    class(Basic) :: a
    type(c_ptr) :: res
    res = a%ptr
end function

function setbasic_new() result(res)
    type(SetBasic) :: res
    res%ptr = c_setbasic_new()
end function

subroutine setbasic_free(this)
    type(SetBasic) :: this
    call c_setbasic_free(this%ptr)
end subroutine

subroutine setbasic_assign(a, b)
    class(SetBasic), intent(inout) :: a
    class(SetBasic), intent(in) :: b
    integer :: i, dummy
    type(Basic) :: temp
    if (c_associated(a%ptr)) then
        call setbasic_free(a)
    end if
    a%ptr = c_setbasic_new()
    do i = 1, b%size()
        temp = b%get(i)
        dummy = c_setbasic_insert(a%ptr, temp%ptr)
    end do
end subroutine

function setbasic_size(this) result(res)
    class(SetBasic) :: this
    integer :: res
    res = int(c_setbasic_size(this%ptr))
end function

function setbasic_get(this, n) result(res)
    class(SetBasic) :: this
    integer :: n
    type(Basic) :: res
    
    res = Basic()
    call c_setbasic_get(this%ptr, n - 1, res%ptr)
end function

function basic_new() result(new)
    type(Basic) :: new
    new%ptr = c_basic_new_heap()
end function

subroutine basic_free(this)
    type(Basic) :: this
    call c_basic_free_heap(this%ptr)
end subroutine

function str(e) result(res)
    class(Basic) :: e
    character(:), allocatable :: res
    type(c_ptr) :: cstring
    cstring = c_basic_str(e%ptr)
    res = convert_string(cstring)
    call c_basic_str_free(cstring)
end function

real(real64) function dbl(e) result(res)
    class(Basic) :: e
    res = cdbl(str(e))
end function

function subs(e, a, b) result(res)
    class(Basic) :: e, a, b
    integer(c_long) :: exception
    type(Basic) :: res

    res = Basic()
    exception = c_basic_subs2(res%ptr, e%ptr, a%ptr, b%ptr) 
    call handle_exception(exception)
end function

function basic_free_symbols(this) result(res)
    class(Basic) :: this
    type(SetBasic) :: res
    integer(c_long) :: exception
    res = SetBasic()
    exception = c_basic_free_symbols(this%ptr, res%ptr)
    call handle_exception(exception)
end function

subroutine basic_assign(a, b)
    class(basic), intent(inout) :: a
    class(basic), intent(in) :: b
    integer(c_long) :: exception
    if (.not. c_associated(a%ptr)) then
        a%ptr = c_basic_new_heap()
    end if
    exception = c_basic_assign(a%ptr, b%ptr)
    call handle_exception(exception)
end subroutine

subroutine basic_assign_i(a, b)
    class(basic), intent(inout) :: a
    integer(kind=int32), intent(in) :: b
    call basic_assign(a, SymInteger(b))
end subroutine

subroutine basic_assign_i64(a, b)
    class(basic), intent(inout) :: a
    integer(kind=int64), intent(in) :: b
    call basic_assign(a, SymInteger(b))
end subroutine

subroutine basic_assign_f(a, b)
    class(basic), intent(inout) :: a
    real(kind=real32), intent(in) :: b
    call basic_assign(a, RealDouble(b))
end subroutine

subroutine basic_assign_d(a, b)
    class(basic), intent(inout) :: a
    real(kind=real64), intent(in) :: b
    call basic_assign(a, RealDouble(b))
end subroutine

subroutine basic_assign_c(a, b)
    class(basic), intent(inout) :: a
    complex(kind=real32), intent(in) :: b
    call basic_assign(a, ComplexDouble(b))
end subroutine

subroutine basic_assign_c64(a, b)
    class(basic), intent(inout) :: a
    complex(kind=real64), intent(in) :: b
    call basic_assign(a, ComplexDouble(b))
end subroutine

function basic_add(a, b) result(res)
    class(basic), intent(in) :: a, b
    type(basic) :: res
    integer(c_long) :: exception

    res = Basic()
    exception = c_basic_add(res%ptr, a%ptr, b%ptr)
    call handle_exception(exception)
end function

function basic_add_i_left(this, b) result(res)
    class(basic), intent(in) :: this
    integer(kind=int32), intent(in) :: b
    type(basic) :: res
    res = basic_add(this, SymInteger(b))
end function

function basic_add_i_right(b, this) result(res)
    class(basic), intent(in) :: this
    integer(kind=int32), intent(in) :: b
    type(basic) :: res
    res = basic_add(this, SymInteger(b))
end function

function basic_add_i64_left(this, b) result(res)
    class(basic), intent(in) :: this
    integer(kind=int64), intent(in) :: b
    type(basic) :: res
    res = basic_add(this, SymInteger(b))
end function

function basic_add_i64_right(b, this) result(res)
    class(basic), intent(in) :: this
    integer(kind=int64), intent(in) :: b
    type(basic) :: res
    res = basic_add(this, SymInteger(b))
end function

function basic_add_f_left(this, b) result(res)
    class(basic), intent(in) :: this
    real(kind=real32), intent(in) :: b
    type(basic) :: res
    res = basic_add(this, RealDouble(b))
end function

function basic_add_f_right(b, this) result(res)
    class(basic), intent(in) :: this
    real(kind=real32), intent(in) :: b
    type(basic) :: res
    res = basic_add(this, RealDouble(b))
end function

function basic_add_d_left(this, b) result(res)
    class(basic), intent(in) :: this
    real(kind=real64), intent(in) :: b
    type(basic) :: res
    res = basic_add(this, RealDouble(b))
end function

function basic_add_d_right(b, this) result(res)
    class(basic), intent(in) :: this
    real(kind=real64), intent(in) :: b
    type(basic) :: res
    res = basic_add(this, RealDouble(b))
end function

function basic_add_c_left(this, b) result(res)
    class(basic), intent(in) :: this
    complex(kind=real32), intent(in) :: b
    type(basic) :: res
    res = basic_add(this, ComplexDouble(b))
end function

function basic_add_c_right(b, this) result(res)
    class(basic), intent(in) :: this
    complex(kind=real32), intent(in) :: b
    type(basic) :: res
    res = basic_add(this, ComplexDouble(b))
end function

function basic_add_c64_left(this, b) result(res)
    class(basic), intent(in) :: this
    complex(kind=real64), intent(in) :: b
    type(basic) :: res
    res = basic_add(this, ComplexDouble(b))
end function

function basic_add_c64_right(b, this) result(res)
    class(basic), intent(in) :: this
    complex(kind=real64), intent(in) :: b
    type(basic) :: res
    res = basic_add(this, ComplexDouble(b))
end function

function basic_sub(a, b) result(res)
    class(basic), intent(in) :: a, b
    type(basic) :: res
    integer(c_long) :: exception
    res = Basic()
    exception = c_basic_sub(res%ptr, a%ptr, b%ptr)
    call handle_exception(exception)
end function

function basic_sub_i_left(this, b) result(res)
    class(basic), intent(in) :: this
    integer(kind=int32), intent(in) :: b
    type(basic) :: res
    res = basic_sub(this, SymInteger(b))
end function

function basic_sub_i_right(b, this) result(res)
    class(basic), intent(in) :: this
    integer(kind=int32), intent(in) :: b
    type(basic) :: res
    res = basic_sub(SymInteger(b), this)
end function

function basic_sub_i64_left(this, b) result(res)
    class(basic), intent(in) :: this
    integer(kind=int64), intent(in) :: b
    type(basic) :: res
    res = basic_sub(this, SymInteger(b))
end function

function basic_sub_i64_right(b, this) result(res)
    class(basic), intent(in) :: this
    integer(kind=int64), intent(in) :: b
    type(basic) :: res
    res = basic_sub(SymInteger(b), this)
end function

function basic_sub_f_left(this, b) result(res)
    class(basic), intent(in) :: this
    real(kind=real32), intent(in) :: b
    type(basic) :: res
    res = basic_sub(this, RealDouble(b))
end function

function basic_sub_f_right(b, this) result(res)
    class(basic), intent(in) :: this
    real(kind=real32), intent(in) :: b
    type(basic) :: res
    res = basic_sub(RealDouble(b), this)
end function

function basic_sub_d_left(this, b) result(res)
    class(basic), intent(in) :: this
    real(kind=real64), intent(in) :: b
    type(basic) :: res
    res = basic_sub(this, RealDouble(b))
end function

function basic_sub_d_right(b, this) result(res)
    class(basic), intent(in) :: this
    real(kind=real64), intent(in) :: b
    type(basic) :: res
    res = basic_sub(RealDouble(b), this)
end function

function basic_sub_c_left(this, b) result(res)
    class(basic), intent(in) :: this
    complex(kind=real32), intent(in) :: b
    type(basic) :: res
    res = basic_sub(this, ComplexDouble(b))
end function

function basic_sub_c_right(b, this) result(res)
    class(basic), intent(in) :: this
    complex(kind=real32), intent(in) :: b
    type(basic) :: res
    res = basic_sub(ComplexDouble(b), this)
end function

function basic_sub_c64_left(this, b) result(res)
    class(basic), intent(in) :: this
    complex(kind=real64), intent(in) :: b
    type(basic) :: res
    res = basic_sub(this, ComplexDouble(b))
end function

function basic_sub_c64_right(b, this) result(res)
    class(basic), intent(in) :: this
    complex(kind=real64), intent(in) :: b
    type(basic) :: res
    res = basic_sub(ComplexDouble(b), this)
end function

function basic_neg(a) result(res)
    class(basic), intent(in) :: a
    type(basic) :: res
    type(basic) :: zero
    integer(c_long) :: exception
    zero = SymInteger(0)
    res = Basic()
    exception = c_basic_sub(res%ptr, zero%ptr, a%ptr)
    call handle_exception(exception)
end function

function basic_mul(a, b) result(res)
    class(basic), intent(in) :: a, b
    type(basic) :: res
    integer(c_long) :: exception
    res = Basic()
    exception = c_basic_mul(res%ptr, a%ptr, b%ptr)
    call handle_exception(exception)
end function

function basic_mul_i_left(this, b) result(res)
    class(basic), intent(in) :: this
    integer(kind=int32), intent(in) :: b
    type(basic) :: res
    res = basic_mul(this, SymInteger(b))
end function

function basic_mul_i_right(b, this) result(res)
    class(basic), intent(in) :: this
    integer(kind=int32), intent(in) :: b
    type(basic) :: res
    res = basic_mul(SymInteger(b), this)
end function

function basic_mul_i64_left(this, b) result(res)
    class(basic), intent(in) :: this
    integer(kind=int64), intent(in) :: b
    type(basic) :: res
    res = basic_mul(this, SymInteger(b))
end function

function basic_mul_i64_right(b, this) result(res)
    class(basic), intent(in) :: this
    integer(kind=int64), intent(in) :: b
    type(basic) :: res
    res = basic_mul(SymInteger(b), this)
end function

function basic_mul_f_left(this, b) result(res)
    class(basic), intent(in) :: this
    real(kind=real32), intent(in) :: b
    type(basic) :: res
    res = basic_mul(this, RealDouble(b))
end function

function basic_mul_f_right(b, this) result(res)
    class(basic), intent(in) :: this
    real(kind=real32), intent(in) :: b
    type(basic) :: res
    res = basic_mul(RealDouble(b), this)
end function

function basic_mul_d_left(this, b) result(res)
    class(basic), intent(in) :: this
    real(kind=real64), intent(in) :: b
    type(basic) :: res
    res = basic_mul(this, RealDouble(b))
end function

function basic_mul_d_right(b, this) result(res)
    class(basic), intent(in) :: this
    real(kind=real64), intent(in) :: b
    type(basic) :: res
    res = basic_mul(RealDouble(b), this)
end function

function basic_mul_c_left(this, b) result(res)
    class(basic), intent(in) :: this
    complex(kind=real32), intent(in) :: b
    type(basic) :: res
    res = basic_mul(this, ComplexDouble(b))
end function

function basic_mul_c_right(b, this) result(res)
    class(basic), intent(in) :: this
    complex(kind=real32), intent(in) :: b
    type(basic) :: res
    res = basic_mul(ComplexDouble(b), this)
end function

function basic_mul_c64_left(this, b) result(res)
    class(basic), intent(in) :: this
    complex(kind=real64), intent(in) :: b
    type(basic) :: res
    res = basic_mul(this, ComplexDouble(b))
end function

function basic_mul_c64_right(b, this) result(res)
    class(basic), intent(in) :: this
    complex(kind=real64), intent(in) :: b
    type(basic) :: res
    res = basic_mul(ComplexDouble(b), this)
end function

function basic_div(a, b) result(res)
    class(basic), intent(in) :: a, b
    type(basic) :: res
    integer(c_long) :: exception
    res = Basic()
    exception = c_basic_div(res%ptr, a%ptr, b%ptr)
    call handle_exception(exception)
end function

function basic_div_i_left(this, b) result(res)
    class(basic), intent(in) :: this
    integer(kind=int32), intent(in) :: b
    type(basic) :: res
    res = basic_div(this, SymInteger(b))
end function

function basic_div_i_right(b, this) result(res)
    class(basic), intent(in) :: this
    integer(kind=int32), intent(in) :: b
    type(basic) :: res
    res = basic_div(SymInteger(b), this)
end function

function basic_div_i64_left(this, b) result(res)
    class(basic), intent(in) :: this
    integer(kind=int64), intent(in) :: b
    type(basic) :: res
    res = basic_div(this, SymInteger(b))
end function

function basic_div_i64_right(b, this) result(res)
    class(basic), intent(in) :: this
    integer(kind=int64), intent(in) :: b
    type(basic) :: res
    res = basic_div(SymInteger(b), this)
end function

function basic_div_f_left(this, b) result(res)
    class(basic), intent(in) :: this
    real(kind=real32), intent(in) :: b
    type(basic) :: res
    res = basic_div(this, RealDouble(b))
end function

function basic_div_f_right(b, this) result(res)
    class(basic), intent(in) :: this
    real(kind=real32), intent(in) :: b
    type(basic) :: res
    res = basic_div(RealDouble(b), this)
end function

function basic_div_d_left(this, b) result(res)
    class(basic), intent(in) :: this
    real(kind=real64), intent(in) :: b
    type(basic) :: res
    res = basic_div(this, RealDouble(b))
end function

function basic_div_d_right(b, this) result(res)
    class(basic), intent(in) :: this
    real(kind=real64), intent(in) :: b
    type(basic) :: res
    res = basic_div(RealDouble(b), this)
end function

function basic_div_c_left(this, b) result(res)
    class(basic), intent(in) :: this
    complex(kind=real32), intent(in) :: b
    type(basic) :: res
    res = basic_div(this, ComplexDouble(b))
end function

function basic_div_c_right(b, this) result(res)
    class(basic), intent(in) :: this
    complex(kind=real32), intent(in) :: b
    type(basic) :: res
    res = basic_div(ComplexDouble(b), this)
end function

function basic_div_c64_left(this, b) result(res)
    class(basic), intent(in) :: this
    complex(kind=real64), intent(in) :: b
    type(basic) :: res
    res = basic_div(this, ComplexDouble(b))
end function

function basic_div_c64_right(b, this) result(res)
    class(basic), intent(in) :: this
    complex(kind=real64), intent(in) :: b
    type(basic) :: res
    res = basic_div(ComplexDouble(b), this)
end function

function basic_pow(a, b) result(res)
    class(basic), intent(in) :: a, b
    type(basic) :: res
    integer(c_long) :: exception
    res = Basic()
    exception = c_basic_pow(res%ptr, a%ptr, b%ptr)
    call handle_exception(exception)
end function

function basic_pow_i_left(this, b) result(res)
    class(basic), intent(in) :: this
    integer(kind=int32), intent(in) :: b
    type(basic) :: res
    res = basic_pow(this, SymInteger(b))
end function

function basic_pow_i_right(b, this) result(res)
    class(basic), intent(in) :: this
    integer(kind=int32), intent(in) :: b
    type(basic) :: res
    res = basic_pow(SymInteger(b), this)
end function

function basic_pow_i64_left(this, b) result(res)
    class(basic), intent(in) :: this
    integer(kind=int64), intent(in) :: b
    type(basic) :: res
    res = basic_pow(this, SymInteger(b))
end function

function basic_pow_i64_right(b, this) result(res)
    class(basic), intent(in) :: this
    integer(kind=int64), intent(in) :: b
    type(basic) :: res
    res = basic_pow(SymInteger(b), this)
end function

function basic_pow_f_left(this, b) result(res)
    class(basic), intent(in) :: this
    real(kind=real32), intent(in) :: b
    type(basic) :: res
    res = basic_pow(this, RealDouble(b))
end function

function basic_pow_f_right(b, this) result(res)
    class(basic), intent(in) :: this
    real(kind=real32), intent(in) :: b
    type(basic) :: res
    res = basic_pow(RealDouble(b), this)
end function

function basic_pow_d_left(this, b) result(res)
    class(basic), intent(in) :: this
    real(kind=real64), intent(in) :: b
    type(basic) :: res
    res = basic_pow(this, RealDouble(b))
end function

function basic_pow_d_right(b, this) result(res)
    class(basic), intent(in) :: this
    real(kind=real64), intent(in) :: b
    type(basic) :: res
    res = basic_pow(RealDouble(b), this)
end function

function basic_pow_c_left(this, b) result(res)
    class(basic), intent(in) :: this
    complex(kind=real32), intent(in) :: b
    type(basic) :: res
    res = basic_pow(this, ComplexDouble(b))
end function

function basic_pow_c_right(b, this) result(res)
    class(basic), intent(in) :: this
    complex(kind=real32), intent(in) :: b
    type(basic) :: res
    res = basic_pow(ComplexDouble(b), this)
end function

function basic_pow_c64_left(this, b) result(res)
    class(basic), intent(in) :: this
    complex(kind=real64), intent(in) :: b
    type(basic) :: res
    res = basic_pow(this, ComplexDouble(b))
end function

function basic_pow_c64_right(b, this) result(res)
    class(basic), intent(in) :: this
    complex(kind=real64), intent(in) :: b
    type(basic) :: res
    res = basic_pow(ComplexDouble(b), this)
end function

function basic_eq(a, b) result(res)
    class(basic), intent(in) :: a, b
    logical :: res
    integer(c_int) :: dummy
    dummy = c_basic_eq(a%ptr, b%ptr)
    res = (dummy /= 0)
end function

function basic_eq_i_left(b, this) result(res)
    class(basic), intent(in) :: this
    integer(kind=int32), intent(in) :: b
    logical :: res
    res = basic_eq(this, SymInteger(b))
end function

function basic_eq_i_right(this, b) result(res)
    class(basic), intent(in) :: this
    integer(kind=int32), intent(in) :: b
    logical :: res
    res = basic_eq(this, SymInteger(b))
end function

function basic_eq_i64_left(b, this) result(res)
    class(basic), intent(in) :: this
    integer(kind=int64), intent(in) :: b
    logical :: res
    res = basic_eq(this, SymInteger(b))
end function

function basic_eq_i64_right(this, b) result(res)
    class(basic), intent(in) :: this
    integer(kind=int64), intent(in) :: b
    logical :: res
    res = basic_eq(this, SymInteger(b))
end function

function basic_eq_f_left(b, this) result(res)
    class(basic), intent(in) :: this
    real(kind=real32), intent(in) :: b
    logical :: res
    res = basic_eq(this, RealDouble(b))
end function

function basic_eq_f_right(this, b) result(res)
    class(basic), intent(in) :: this
    real(kind=real32), intent(in) :: b
    logical :: res
    res = basic_eq(this, RealDouble(b))
end function

function basic_eq_d_left(b, this) result(res)
    class(basic), intent(in) :: this
    real(kind=real64), intent(in) :: b
    logical :: res
    res = basic_eq(this, RealDouble(b))
end function

function basic_eq_d_right(this, b) result(res)
    class(basic), intent(in) :: this
    real(kind=real64), intent(in) :: b
    logical :: res
    res = basic_eq(this, RealDouble(b))
end function

function basic_eq_c_left(b, this) result(res)
    class(basic), intent(in) :: this
    complex(kind=real32), intent(in) :: b
    logical :: res
    res = basic_eq(this, ComplexDouble(b))
end function

function basic_eq_c_right(this, b) result(res)
    class(basic), intent(in) :: this
    complex(kind=real32), intent(in) :: b
    logical :: res
    res = basic_eq(this, ComplexDouble(b))
end function

function basic_eq_c64_left(b, this) result(res)
    class(basic), intent(in) :: this
    complex(kind=real64), intent(in) :: b
    logical :: res
    res = basic_eq(this, ComplexDouble(b))
end function

function basic_eq_c64_right(this, b) result(res)
    class(basic), intent(in) :: this
    complex(kind=real64), intent(in) :: b
    logical :: res
    res = basic_eq(this, ComplexDouble(b))
end function

function basic_neq(a, b) result(res)
    class(basic), intent(in) :: a, b
    logical :: res
    integer(c_int) :: dummy
    dummy = c_basic_neq(a%ptr, b%ptr)
    res = (dummy /= 0)
end function

function basic_neq_i_left(b, this) result(res)
    class(basic), intent(in) :: this
    integer(kind=int32), intent(in) :: b
    logical :: res
    res = basic_neq(this, SymInteger(b))
end function

function basic_neq_i_right(this, b) result(res)
    class(basic), intent(in) :: this
    integer(kind=int32), intent(in) :: b
    logical :: res
    res = basic_neq(this, SymInteger(b))
end function

function basic_neq_i64_left(b, this) result(res)
    class(basic), intent(in) :: this
    integer(kind=int64), intent(in) :: b
    logical :: res
    res = basic_neq(this, SymInteger(b))
end function

function basic_neq_i64_right(this, b) result(res)
    class(basic), intent(in) :: this
    integer(kind=int64), intent(in) :: b
    logical :: res
    res = basic_neq(this, SymInteger(b))
end function

function basic_neq_f_left(b, this) result(res)
    class(basic), intent(in) :: this
    real(kind=real32), intent(in) :: b
    logical :: res
    res = basic_neq(this, RealDouble(b))
end function

function basic_neq_f_right(this, b) result(res)
    class(basic), intent(in) :: this
    real(kind=real32), intent(in) :: b
    logical :: res
    res = basic_neq(this, RealDouble(b))
end function

function basic_neq_d_left(b, this) result(res)
    class(basic), intent(in) :: this
    real(kind=real64), intent(in) :: b
    logical :: res
    res = basic_neq(this, RealDouble(b))
end function

function basic_neq_d_right(this, b) result(res)
    class(basic), intent(in) :: this
    real(kind=real64), intent(in) :: b
    logical :: res
    res = basic_neq(this, RealDouble(b))
end function

function basic_neq_c_left(b, this) result(res)
    class(basic), intent(in) :: this
    complex(kind=real32), intent(in) :: b
    logical :: res
    res = basic_neq(this, ComplexDouble(b))
end function

function basic_neq_c_right(this, b) result(res)
    class(basic), intent(in) :: this
    complex(kind=real32), intent(in) :: b
    logical :: res
    res = basic_neq(this, ComplexDouble(b))
end function

function basic_neq_c64_left(b, this) result(res)
    class(basic), intent(in) :: this
    complex(kind=real64), intent(in) :: b
    logical :: res
    res = basic_neq(this, ComplexDouble(b))
end function

function basic_neq_c64_right(this, b) result(res)
    class(basic), intent(in) :: this
    complex(kind=real64), intent(in) :: b
    logical :: res
    res = basic_neq(this, ComplexDouble(b))
end function

function evalf(b, bits, r) result(res)
    class(basic), intent(in) :: b
    integer(c_long), optional :: bits
    integer(c_int), optional :: r
    type(basic) :: res
    integer(c_long) :: exception
    integer(c_int) :: domain

    domain = 1
    if (present(r)) domain = r

    res = Basic()
    if (present(bits)) then
        exception = c_basic_evalf(res%ptr, b%ptr, bits, domain)
    else
        exception = c_basic_evalf(res%ptr, b%ptr, 53_c_long, domain)
    end if
    call handle_exception(exception)
end function

end module
