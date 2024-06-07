module sets

use iso_fortran_env, only: int32, int64, real32, real64

use symengine_interface
use symengine_basic
use exceptions

implicit none

interface interval
    module procedure basic_interval
    module procedure basic_interval_i_i, basic_interval_i_i64, basic_interval_i64_i, basic_interval_i64_i64
    module procedure basic_interval_f_f, basic_interval_f_d, basic_interval_d_f, basic_interval_d_d
    module procedure basic_interval_f_i, basic_interval_i_f, basic_interval_f_i64, basic_interval_i64_f
    module procedure basic_interval_d_i, basic_interval_i_d, basic_interval_d_i64, basic_interval_i64_d
end interface

interface finiteset
    module procedure basic_finiteset
end interface

private
public :: emptyset, universalset, complexes, reals, rationals, integers, set_union, set_intersection
public :: interval, finiteset

contains

function emptyset() result(res)
    type(basic) :: res
    res = Basic()
    call c_basic_set_emptyset(res%ptr)
end function

function universalset() result(res)
    type(basic) :: res

    res = Basic()
    call c_basic_set_universalset(res%ptr)
end function

function complexes() result(res)
    type(basic) :: res
    res = Basic()
    call c_basic_set_complexes(res%ptr)
end function

function reals() result(res)
    type(basic) :: res
    res = Basic()
    call c_basic_set_reals(res%ptr)
end function

function rationals() result(res)
    type(basic) :: res

    res = Basic()
    call c_basic_set_rationals(res%ptr)
end function

function integers() result(res)
    type(basic) :: res
    res = Basic()
    call c_basic_set_integers(res%ptr)
end function

function basic_interval(a, b, left_open, right_open) result(res)
    class(basic), intent(in) :: a, b
    logical, optional :: left_open, right_open
    integer(c_int) :: a_left = 1, a_right = 1
    integer(c_long) :: exception
    type(basic) :: res
    res = Basic()
    if (present(left_open) .and. .not. left_open) then
        a_left = 0
    end if
    if (present(right_open) .and. .not. right_open) then
        a_right = 0
    end if
    res = Basic()
    exception = c_basic_set_interval(res%ptr, a%ptr, b%ptr, a_left, a_right)
    call handle_exception(exception)
end function

type(basic) function basic_interval_i_i(a, b, left_open, right_open) result(res)
    integer(kind=int32) :: a, b
    logical, optional :: left_open, right_open
    res = basic_interval(SymInteger(a), SymInteger(b), left_open, right_open)
end function

type(basic) function basic_interval_i_i64(a, b, left_open, right_open) result(res)
    integer(kind=int32) :: a
    integer(kind=int64) :: b
    logical, optional :: left_open, right_open
    res = basic_interval(SymInteger(a), SymInteger(b), left_open, right_open)
end function

type(basic) function basic_interval_i64_i(a, b, left_open, right_open) result(res)
    integer(kind=int64) :: a
    integer(kind=int32) :: b
    logical, optional :: left_open, right_open
    res = basic_interval(SymInteger(a), SymInteger(b), left_open, right_open)
end function

type(basic) function basic_interval_i64_i64(a, b, left_open, right_open) result(res)
    integer(kind=int64) :: a, b
    logical, optional :: left_open, right_open
    res = basic_interval(SymInteger(a), SymInteger(b), left_open, right_open)
end function

type(basic) function basic_interval_f_f(a, b, left_open, right_open) result(res)
    real(kind=real32) :: a, b
    logical, optional :: left_open, right_open
    res = basic_interval(RealDouble(a), RealDouble(b), left_open, right_open)
end function

type(basic) function basic_interval_f_d(a, b, left_open, right_open) result(res)
    real(kind=real32) :: a
    real(kind=real64) :: b
    logical, optional :: left_open, right_open
    res = basic_interval(RealDouble(a), RealDouble(b), left_open, right_open)
end function

type(basic) function basic_interval_d_f(a, b, left_open, right_open) result(res)
    real(kind=real64) :: a
    real(kind=real32) :: b
    logical, optional :: left_open, right_open
    res = basic_interval(RealDouble(a), RealDouble(b), left_open, right_open)
end function

type(basic) function basic_interval_d_d(a, b, left_open, right_open) result(res)
    real(kind=real64) :: a, b
    logical, optional :: left_open, right_open
    res = basic_interval(RealDouble(a), RealDouble(b), left_open, right_open)
end function

type(basic) function basic_interval_f_i(a, b, left_open, right_open) result(res)
    real(kind=real32) :: a
    integer(kind=int32) :: b
    logical, optional :: left_open, right_open
    res = basic_interval(RealDouble(a), SymInteger(b), left_open, right_open)
end function

type(basic) function basic_interval_i_f(a, b, left_open, right_open) result(res)
    integer(kind=int32) :: a
    real(kind=real32) :: b
    logical, optional :: left_open, right_open
    res = basic_interval(SymInteger(a), RealDouble(b), left_open, right_open)
end function

type(basic) function basic_interval_f_i64(a, b, left_open, right_open) result(res)
    real(kind=real32) :: a
    integer(kind=int64) :: b
    logical, optional :: left_open, right_open
    res = basic_interval(RealDouble(a), SymInteger(b), left_open, right_open)
end function

type(basic) function basic_interval_i64_f(a, b, left_open, right_open) result(res)
    integer(kind=int64) :: a
    real(kind=real32) :: b
    logical, optional :: left_open, right_open
    res = basic_interval(SymInteger(a), RealDouble(b), left_open, right_open)
end function

type(basic) function basic_interval_d_i(a, b, left_open, right_open) result(res)
    real(kind=real64) :: a
    integer(kind=int32) :: b
    logical, optional :: left_open, right_open
    res = basic_interval(RealDouble(a), SymInteger(b), left_open, right_open)
end function

type(basic) function basic_interval_i_d(a, b, left_open, right_open) result(res)
    integer(kind=int32) :: a
    real(kind=real64) :: b
    logical, optional :: left_open, right_open
    res = basic_interval(SymInteger(a), RealDouble(b), left_open, right_open)
end function

type(basic) function basic_interval_d_i64(a, b, left_open, right_open) result(res)
    real(kind=real64) :: a
    integer(kind=int64) :: b
    logical, optional :: left_open, right_open
    res = basic_interval(RealDouble(a), SymInteger(b), left_open, right_open)
end function

type(basic) function basic_interval_i64_d(a, b, left_open, right_open) result(res)
    integer(kind=int64) :: a
    real(kind=real64) :: b
    logical, optional :: left_open, right_open
    res = basic_interval(SymInteger(a), RealDouble(b), left_open, right_open)
end function

function basic_finiteset(d) result(res)
    type(c_ptr), dimension(:) :: d
    type(c_ptr) :: set
    integer(c_long) :: exception
    integer :: i
    type(basic) :: res
    
    set = c_setbasic_new()

    do i = 1, size(d)
        exception = c_setbasic_insert(set, d(i))
    end do

    res = Basic()
    exception = c_basic_set_finiteset(res%ptr, set)
    call handle_exception(exception)
    call c_setbasic_free(set)
end function

function set_union(a, b) result(res)
    class(basic), intent(in) :: a, b
    integer(c_long) :: exception
    type(basic) :: res

    res = Basic()
    exception = c_basic_set_union(res%ptr, a%ptr, b%ptr)
    call handle_exception(exception)
end function

function set_intersection(a, b) result(res)
    class(basic), intent(in) :: a, b
    integer(c_long) :: exception
    type(basic) :: res

    res = Basic()
    exception = c_basic_set_intersection(res%ptr, a%ptr, b%ptr)
    call handle_exception(exception)
end function

end module
