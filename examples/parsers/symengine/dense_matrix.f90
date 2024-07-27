module dense_matrix

use iso_c_binding, only: c_int, c_long, c_double, c_ptr, c_null_ptr, c_null_char, c_f_pointer, c_associated
use iso_fortran_env, only: int32, int64, real32, real64
use symengine_interface
use symengine_basic
use exceptions 

implicit none

type DenseMatrix
    type(c_ptr) :: ptr = c_null_ptr
contains
    procedure :: str => matrix_str
    procedure :: matrix_assign, matrix_eq
    procedure, pass(this) :: matrix_add_scalar_left, matrix_add_scalar_right
    procedure, pass(this) :: matrix_add_scalar_i_left, matrix_add_scalar_i_right
    procedure, pass(this) :: matrix_add_scalar_i64_left, matrix_add_scalar_i64_right
    procedure, pass(this) :: matrix_add_scalar_f_left, matrix_add_scalar_f_right
    procedure, pass(this) :: matrix_add_scalar_d_left, matrix_add_scalar_d_right
    procedure, pass(this) :: matrix_neg
    procedure, pass(this) :: matrix_sub_scalar_left, matrix_sub_scalar_right
    procedure, pass(this) :: matrix_sub_scalar_i_left, matrix_sub_scalar_i_right
    procedure, pass(this) :: matrix_sub_scalar_i64_left, matrix_sub_scalar_i64_right
    procedure, pass(this) :: matrix_sub_scalar_f_left, matrix_sub_scalar_f_right
    procedure, pass(this) :: matrix_sub_scalar_d_left, matrix_sub_scalar_d_right
    procedure, pass(this) :: matrix_mul_scalar_left, matrix_mul_scalar_right
    procedure, pass(this) :: matrix_mul_scalar_i_left, matrix_mul_scalar_i_right
    procedure, pass(this) :: matrix_mul_scalar_i64_left, matrix_mul_scalar_i64_right
    procedure, pass(this) :: matrix_mul_scalar_f_left, matrix_mul_scalar_f_right
    procedure, pass(this) :: matrix_mul_scalar_d_left, matrix_mul_scalar_d_right
    procedure :: matrix_mul_dense
    procedure :: matrix_add_dense
    procedure :: matrix_sub_dense
    procedure, pass(this) :: matrix_div_scalar_left
    procedure, pass(this) :: matrix_div_scalar_i_left
    procedure, pass(this) :: matrix_div_scalar_i64_left
    procedure, pass(this) :: matrix_div_scalar_f_left
    procedure, pass(this) :: matrix_div_scalar_d_left
    generic :: assignment(=) => matrix_assign
    generic :: operator(+) => matrix_add_scalar_left, matrix_add_scalar_right
    generic :: operator(+) => matrix_add_scalar_i_left, matrix_add_scalar_i_right
    generic :: operator(+) => matrix_add_scalar_i64_left, matrix_add_scalar_i64_right
    generic :: operator(+) => matrix_add_scalar_f_left, matrix_add_scalar_f_right
    generic :: operator(+) => matrix_add_scalar_d_left, matrix_add_scalar_d_right
    generic :: operator(+) => matrix_add_dense
    generic :: operator(-) => matrix_neg
    generic :: operator(-) => matrix_sub_scalar_left, matrix_sub_scalar_right
    generic :: operator(-) => matrix_sub_scalar_i_left, matrix_sub_scalar_i_right
    generic :: operator(-) => matrix_sub_scalar_i64_left, matrix_sub_scalar_i64_right
    generic :: operator(-) => matrix_sub_scalar_f_left, matrix_sub_scalar_f_right
    generic :: operator(-) => matrix_sub_scalar_d_left, matrix_sub_scalar_d_right
    generic :: operator(-) => matrix_sub_dense
    generic :: operator(*) => matrix_mul_scalar_left, matrix_mul_scalar_right
    generic :: operator(*) => matrix_mul_scalar_i_left, matrix_mul_scalar_i_right
    generic :: operator(*) => matrix_mul_scalar_i64_left, matrix_mul_scalar_i64_right
    generic :: operator(*) => matrix_mul_scalar_f_left, matrix_mul_scalar_f_right
    generic :: operator(*) => matrix_mul_scalar_d_left, matrix_mul_scalar_d_right
    generic :: operator(*) => matrix_mul_dense
    generic :: operator(/) => matrix_div_scalar_left
    generic :: operator(/) => matrix_div_scalar_i_left
    generic :: operator(/) => matrix_div_scalar_i64_left
    generic :: operator(/) => matrix_div_scalar_f_left
    generic :: operator(/) => matrix_div_scalar_d_left
    generic :: operator(==) => matrix_eq
    final :: matrix_free
end type

interface DenseMatrix
    module procedure matrix_new
end interface

interface transpose
    module procedure transpose_dense
end interface

private
public :: DenseMatrix, transpose, ones, zeros, eye

contains

function matrix_new(rows, cols, d) result(res)
    integer :: rows, cols
    type(c_ptr), dimension(:) :: d
    type(DenseMatrix) :: res

    type(c_ptr) :: vec
    integer(c_long) :: exception, rlong, clong
    integer :: i
    
    vec = c_vecbasic_new()

    do i = 1, size(d)
        exception = c_vecbasic_push_back(vec, d(i))
        call handle_exception(exception)
    end do

    rlong = int(rows)
    clong = int(cols)
    res%ptr = c_dense_matrix_new_vec(rlong, clong, vec)
    call c_vecbasic_free(vec)
end function

subroutine matrix_free(this)
    type(DenseMatrix) :: this
    call c_dense_matrix_free(this%ptr)
end subroutine

subroutine matrix_assign(a, b)
    class(DenseMatrix), intent(inout) :: a
    class(DenseMatrix), intent(in) :: b
    integer(c_long) :: exception
    if (.not. c_associated(a%ptr)) then
        a%ptr = c_dense_matrix_new()
    end if
    exception = c_dense_matrix_set(a%ptr, b%ptr)
    call handle_exception(exception)
end subroutine

function matrix_add_scalar_left(this, b) result(res)
    class(DenseMatrix), intent(in) :: this
    class(Basic), intent(in) :: b
    type(DenseMatrix) :: res
    integer(c_long) :: exception

    res%ptr = c_dense_matrix_new()
    exception = c_dense_matrix_add_scalar(res%ptr, this%ptr, b%ptr)
    call handle_exception(exception)
end function

function matrix_add_scalar_right(a, this) result(res)
    class(Basic), intent(in) :: a
    class(DenseMatrix), intent(in) :: this
    type(DenseMatrix) :: res
    res = matrix_add_scalar_left(this, a)
end function

function matrix_add_scalar_i_left(this, b) result(res)
    class(DenseMatrix), intent(in) :: this
    integer(kind=int32), intent(in) :: b
    type(DenseMatrix) :: res
    type(basic) :: i
    i = SymInteger(b)
    res = matrix_add_scalar_left(this, i)
end function

function matrix_add_scalar_i_right(a, this) result(res)
    integer(kind=int32), intent(in) :: a
    class(DenseMatrix), intent(in) :: this
    type(DenseMatrix) :: res
    res = matrix_add_scalar_i_left(this, a)
end function

function matrix_add_scalar_i64_left(this, b) result(res)
    class(DenseMatrix), intent(in) :: this
    integer(kind=int64), intent(in) :: b
    type(DenseMatrix) :: res
    type(basic) :: i
    i = SymInteger(b)
    res = matrix_add_scalar_left(this, i)
end function

function matrix_add_scalar_i64_right(a, this) result(res)
    integer(kind=int64), intent(in) :: a
    class(DenseMatrix), intent(in) :: this
    type(DenseMatrix) :: res
    res = matrix_add_scalar_i64_left(this, a)
end function

function matrix_add_scalar_f_left(this, b) result(res)
    class(DenseMatrix), intent(in) :: this
    real(kind=real32), intent(in) :: b
    type(DenseMatrix) :: res
    type(basic) :: i
    i = RealDouble(b)
    res = matrix_add_scalar_left(this, i)
end function

function matrix_add_scalar_f_right(a, this) result(res)
    real(kind=real32), intent(in) :: a
    class(DenseMatrix), intent(in) :: this
    type(DenseMatrix) :: res
    res = matrix_add_scalar_f_left(this, a)
end function

function matrix_add_scalar_d_left(this, b) result(res)
    class(DenseMatrix), intent(in) :: this
    real(kind=real64), intent(in) :: b
    type(DenseMatrix) :: res
    type(basic) :: i
    i = RealDouble(b)
    res = matrix_add_scalar_left(this, i)
end function

function matrix_add_scalar_d_right(a, this) result(res)
    real(kind=real64), intent(in) :: a
    class(DenseMatrix), intent(in) :: this
    type(DenseMatrix) :: res
    res = matrix_add_scalar_d_left(this, a)
end function

function matrix_str(e) result(res)
    class(DenseMatrix) :: e
    character(:), allocatable :: res
    type(c_ptr) :: cstring
    cstring = c_dense_matrix_str(e%ptr)
    res = convert_string(cstring)
    call c_basic_str_free(cstring)
end function

function matrix_eq(a, b) result(res)
    class(DenseMatrix), intent(in) :: a, b
    logical :: res
    integer(c_int) :: dummy
    dummy = c_dense_matrix_eq(a%ptr, b%ptr)
    res = (dummy /= 0)
end function

function matrix_neg(this) result(res)
    class(DenseMatrix), intent(in) :: this
    type(DenseMatrix) :: res
    type(Basic) :: m1
    integer(c_long) :: exception

    res%ptr = c_dense_matrix_new()
    m1 = SymInteger(-1)
    exception = c_dense_matrix_mul_scalar(res%ptr, this%ptr, m1%ptr)
    call handle_exception(exception)
end function

function matrix_sub_scalar_left(this, b) result(res)
    class(DenseMatrix), intent(in) :: this
    class(Basic), intent(in) :: b
    type(DenseMatrix) :: res
    res = matrix_add_scalar_left(this, -b)
end function

function matrix_sub_scalar_right(a, this) result(res)
    class(Basic), intent(in) :: a
    class(DenseMatrix), intent(in) :: this
    type(DenseMatrix) :: res
    res = matrix_add_scalar_left(-this, a)
end function

function matrix_sub_scalar_i_left(this, b) result(res)
    class(DenseMatrix), intent(in) :: this
    integer(kind=int32), intent(in) :: b
    type(DenseMatrix) :: res
    res = matrix_add_scalar_left(this, SymInteger(-b))
end function

function matrix_sub_scalar_i_right(a, this) result(res)
    integer(kind=int32), intent(in) :: a
    class(DenseMatrix), intent(in) :: this
    type(DenseMatrix) :: res
    res = matrix_add_scalar_left(-this, SymInteger(a))
end function

function matrix_sub_scalar_i64_left(this, b) result(res)
    class(DenseMatrix), intent(in) :: this
    integer(kind=int64), intent(in) :: b
    type(DenseMatrix) :: res
    res = matrix_add_scalar_left(this, SymInteger(-b))
end function

function matrix_sub_scalar_i64_right(a, this) result(res)
    integer(kind=int64), intent(in) :: a
    class(DenseMatrix), intent(in) :: this
    type(DenseMatrix) :: res
    res = matrix_add_scalar_left(-this, SymInteger(a))
end function

function matrix_sub_scalar_f_left(this, b) result(res)
    class(DenseMatrix), intent(in) :: this
    real(kind=real32), intent(in) :: b
    type(DenseMatrix) :: res
    res = matrix_add_scalar_left(this, RealDouble(-b))
end function

function matrix_sub_scalar_f_right(a, this) result(res)
    real(kind=real32), intent(in) :: a
    class(DenseMatrix), intent(in) :: this
    type(DenseMatrix) :: res
    res = matrix_add_scalar_left(-this, RealDouble(a))
end function

function matrix_sub_scalar_d_left(this, b) result(res)
    class(DenseMatrix), intent(in) :: this
    real(kind=real64), intent(in) :: b
    type(DenseMatrix) :: res
    res = matrix_add_scalar_left(this, RealDouble(-b))
end function

function matrix_sub_scalar_d_right(a, this) result(res)
    real(kind=real64), intent(in) :: a
    class(DenseMatrix), intent(in) :: this
    type(DenseMatrix) :: res
    res = matrix_add_scalar_left(-this, RealDouble(a))
end function

function matrix_mul_scalar_left(this, b) result(res)
    class(DenseMatrix), intent(in) :: this
    class(Basic), intent(in) :: b
    type(DenseMatrix) :: res
    integer(c_long) :: exception

    res%ptr = c_dense_matrix_new()
    exception = c_dense_matrix_mul_scalar(res%ptr, this%ptr, b%ptr)
    call handle_exception(exception)
end function

function matrix_mul_scalar_right(a, this) result(res)
    class(Basic), intent(in) :: a
    class(DenseMatrix), intent(in) :: this
    type(DenseMatrix) :: res
    res = matrix_mul_scalar_left(this, a)
end function

function matrix_mul_scalar_i_left(this, b) result(res)
    class(DenseMatrix), intent(in) :: this
    integer(kind=int32), intent(in) :: b
    type(DenseMatrix) :: res
    type(basic) :: i
    i = SymInteger(b)
    res = matrix_mul_scalar_left(this, i)
end function

function matrix_mul_scalar_i_right(a, this) result(res)
    integer(kind=int32), intent(in) :: a
    class(DenseMatrix), intent(in) :: this
    type(DenseMatrix) :: res
    res = matrix_mul_scalar_i_left(this, a)
end function

function matrix_mul_scalar_i64_left(this, b) result(res)
    class(DenseMatrix), intent(in) :: this
    integer(kind=int64), intent(in) :: b
    type(DenseMatrix) :: res
    type(basic) :: i
    i = SymInteger(b)
    res = matrix_mul_scalar_left(this, i)
end function

function matrix_mul_scalar_i64_right(a, this) result(res)
    integer(kind=int64), intent(in) :: a
    class(DenseMatrix), intent(in) :: this
    type(DenseMatrix) :: res
    res = matrix_mul_scalar_i64_left(this, a)
end function

function matrix_mul_scalar_f_left(this, b) result(res)
    class(DenseMatrix), intent(in) :: this
    real(kind=real32), intent(in) :: b
    type(DenseMatrix) :: res
    type(basic) :: i
    i = RealDouble(b)
    res = matrix_mul_scalar_left(this, i)
end function

function matrix_mul_scalar_f_right(a, this) result(res)
    real(kind=real32), intent(in) :: a
    class(DenseMatrix), intent(in) :: this
    type(DenseMatrix) :: res
    res = matrix_mul_scalar_f_left(this, a)
end function

function matrix_mul_scalar_d_left(this, b) result(res)
    class(DenseMatrix), intent(in) :: this
    real(kind=real64), intent(in) :: b
    type(DenseMatrix) :: res
    type(basic) :: i
    i = RealDouble(b)
    res = matrix_mul_scalar_left(this, i)
end function

function matrix_mul_scalar_d_right(a, this) result(res)
    real(kind=real64), intent(in) :: a
    class(DenseMatrix), intent(in) :: this
    type(DenseMatrix) :: res
    res = matrix_mul_scalar_d_left(this, a)
end function

function matrix_div_scalar_left(this, b) result(res)
    class(DenseMatrix), intent(in) :: this
    class(Basic), intent(in) :: b
    type(DenseMatrix) :: res
    type(Basic) :: inverse
    integer(c_long) :: exception

    inverse = 1 / b
    res%ptr = c_dense_matrix_new()
    exception = c_dense_matrix_mul_scalar(res%ptr, this%ptr, inverse%ptr)
    call handle_exception(exception)
end function

function matrix_div_scalar_i_left(this, b) result(res)
    class(DenseMatrix), intent(in) :: this
    integer(kind=int32), intent(in) :: b
    type(DenseMatrix) :: res
    type(basic) :: i
    i = SymInteger(b)
    res = matrix_div_scalar_left(this, i)
end function

function matrix_div_scalar_i64_left(this, b) result(res)
    class(DenseMatrix), intent(in) :: this
    integer(kind=int64), intent(in) :: b
    type(DenseMatrix) :: res
    type(basic) :: i
    i = SymInteger(b)
    res = matrix_div_scalar_left(this, i)
end function

function matrix_div_scalar_f_left(this, b) result(res)
    class(DenseMatrix), intent(in) :: this
    real(kind=real32), intent(in) :: b
    type(DenseMatrix) :: res
    type(basic) :: i
    i = RealDouble(b)
    res = matrix_div_scalar_left(this, i)
end function

function matrix_div_scalar_d_left(this, b) result(res)
    class(DenseMatrix), intent(in) :: this
    real(kind=real64), intent(in) :: b
    type(DenseMatrix) :: res
    type(basic) :: i
    i = RealDouble(b)
    res = matrix_div_scalar_left(this, i)
end function

function transpose_dense(a) result(res)
    class(DenseMatrix), intent(in) :: a
    type(DenseMatrix) :: res
    integer(c_long) :: exception

    res%ptr = c_dense_matrix_new()
    exception = c_dense_matrix_transpose(res%ptr, a%ptr)
    call handle_exception(exception)
end function

function matrix_mul_dense(a, b) result(res)
    class(DenseMatrix), intent(in) :: a, b
    type(DenseMatrix) :: res
    integer(c_long) :: exception

    res%ptr = c_dense_matrix_new()
    exception = c_dense_matrix_mul_matrix(res%ptr, a%ptr, b%ptr)
    call handle_exception(exception)
end function

function matrix_add_dense(a, b) result(res)
    class(DenseMatrix), intent(in) :: a, b
    type(DenseMatrix) :: res
    integer(c_long) :: exception

    res%ptr = c_dense_matrix_new()
    exception = c_dense_matrix_add_matrix(res%ptr, a%ptr, b%ptr)
    call handle_exception(exception)
end function

function matrix_sub_dense(a, b) result(res)
    class(DenseMatrix), intent(in) :: a, b
    type(DenseMatrix) :: res
    type(DenseMatrix) :: neg
    neg = -b
    res = matrix_add_dense(a, neg)
end function

function ones(r, c) result(res)
    integer(kind=int32) :: r, c
    type(DenseMatrix) :: res
    integer(c_long) :: exception

    res%ptr = c_dense_matrix_new()
    exception = c_dense_matrix_ones(res%ptr, int(r, c_long), int(c, c_long))
    call handle_exception(exception)
end function

function zeros(r, c) result(res)
    integer(kind=int32) :: r, c
    type(DenseMatrix) :: res
    integer(c_long) :: exception

    res%ptr = c_dense_matrix_new()
    exception = c_dense_matrix_zeros(res%ptr, int(r, c_long), int(c, c_long))
    call handle_exception(exception)
end function

function eye(n, k) result(res)
    integer(kind=int32) :: n
    integer(kind=int32) :: k
    type(DenseMatrix) :: res
    integer(c_long) :: exception

    res%ptr = c_dense_matrix_new()
    exception = c_dense_matrix_eye(res%ptr, int(n, c_long), int(n, c_long), k)
    call handle_exception(exception)
end function

end module
