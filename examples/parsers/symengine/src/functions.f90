module functions

use iso_fortran_env, only: int32, int64, real32, real64
use symengine_interface
use symengine_basic
use exceptions 
implicit none

interface exp
    module procedure basic_exp
end interface

interface log
    module procedure basic_log
end interface

interface log10
    module procedure basic_log10
end interface

interface abs
    module procedure basic_abs
end interface

interface sqrt
    module procedure basic_sqrt
end interface

interface atan2
    module procedure basic_atan2
    module procedure basic_atan2_i_left
    module procedure basic_atan2_i_right
    module procedure basic_atan2_i64_left
    module procedure basic_atan2_i64_right
    module procedure basic_atan2_f_left
    module procedure basic_atan2_f_right
    module procedure basic_atan2_d_left
    module procedure basic_atan2_d_right
end interface

interface max
    module procedure basic_max
end interface

interface erf
    module procedure basic_erf
end interface

interface erfc
    module procedure basic_erfc
end interface

interface sin
    module procedure basic_sin
end interface

interface cos
    module procedure basic_cos
end interface

interface tan
    module procedure basic_tan
end interface

interface asin
    module procedure basic_asin
end interface

interface acos
    module procedure basic_acos
end interface

interface atan
    module procedure basic_atan
end interface

interface csc
    module procedure basic_csc
end interface

interface sec
    module procedure basic_sec
end interface

interface cot
    module procedure basic_cot
end interface

interface acsc
    module procedure basic_acsc
end interface

interface asec
    module procedure basic_asec
end interface

interface acot
    module procedure basic_acot
end interface

interface sinh
    module procedure basic_sinh
end interface

interface cosh
    module procedure basic_cosh
end interface

interface tanh
    module procedure basic_tanh
end interface

interface asinh
    module procedure basic_asinh
end interface

interface acosh
    module procedure basic_acosh
end interface

interface atanh
    module procedure basic_atanh
end interface

interface csch
    module procedure basic_csch
end interface

interface sech
    module procedure basic_sech
end interface

interface coth
    module procedure basic_coth
end interface

interface acsch
    module procedure basic_acsch
end interface

interface asech
    module procedure basic_asech
end interface

interface acoth
    module procedure basic_acoth
end interface

interface lambertw
    module procedure basic_lambertw
end interface

interface zeta
    module procedure basic_zeta
end interface

interface dirichlet_eta
    module procedure basic_dirichlet_eta
end interface

interface gamma
    module procedure basic_gamma
end interface

interface loggamma
    module procedure basic_loggamma
end interface

interface floor
    module procedure basic_floor
end interface

interface ceiling
    module procedure basic_ceiling
end interface

private
public :: sin, cos, tan, asin, acos, atan
public :: csc, sec, cot, acsc, asec, acot
public :: sinh, cosh, tanh, asinh, acosh, atanh
public :: csch, sech, coth, acsch, asech, acoth
public :: exp, log, abs, sqrt, atan2, max, erf, erfc
public :: lambertw, zeta, dirichlet_eta, gamma, loggamma
public :: floor, ceiling

contains

function basic_erf(a) result(res)
    class(basic), intent(in) :: a
    type(basic) :: res
    integer(c_long) :: exception

    res = Basic()
    exception = c_basic_erf(res%ptr, a%ptr)
    call handle_exception(exception)
end function

function basic_erfc(a) result(res)
    class(basic), intent(in) :: a
    type(basic) :: res
    integer(c_long) :: exception

    res = Basic()
    exception = c_basic_erfc(res%ptr, a%ptr)
    call handle_exception(exception)
end function

function basic_sin(a) result(res)
    class(basic), intent(in) :: a
    type(basic) :: res
    integer(c_long) :: exception

    res = Basic()
    exception = c_basic_sin(res%ptr, a%ptr)
    call handle_exception(exception)
end function

function basic_cos(a) result(res)
    class(basic), intent(in) :: a
    type(basic) :: res
    integer(c_long) :: exception

    res = Basic()
    exception = c_basic_cos(res%ptr, a%ptr)
    call handle_exception(exception)
end function

function basic_tan(a) result(res)
    class(basic), intent(in) :: a
    type(basic) :: res
    integer(c_long) :: exception

    res = Basic()
    exception = c_basic_tan(res%ptr, a%ptr)
    call handle_exception(exception)
end function

function basic_asin(a) result(res)
    class(basic), intent(in) :: a
    type(basic) :: res
    integer(c_long) :: exception

    res = Basic()
    exception = c_basic_asin(res%ptr, a%ptr)
    call handle_exception(exception)
end function

function basic_acos(a) result(res)
    class(basic), intent(in) :: a
    type(basic) :: res
    integer(c_long) :: exception

    res = Basic()
    exception = c_basic_acos(res%ptr, a%ptr)
    call handle_exception(exception)
end function

function basic_atan(a) result(res)
    class(basic), intent(in) :: a
    type(basic) :: res
    integer(c_long) :: exception

    res = Basic()
    exception = c_basic_atan(res%ptr, a%ptr)
    call handle_exception(exception)
end function

function basic_csc(a) result(res)
    class(basic), intent(in) :: a
    type(basic) :: res
    integer(c_long) :: exception

    res = Basic()
    exception = c_basic_csc(res%ptr, a%ptr)
    call handle_exception(exception)
end function

function basic_sec(a) result(res)
    class(basic), intent(in) :: a
    type(basic) :: res
    integer(c_long) :: exception

    res = Basic()
    exception = c_basic_sec(res%ptr, a%ptr)
    call handle_exception(exception)
end function

function basic_cot(a) result(res)
    class(basic), intent(in) :: a
    type(basic) :: res
    integer(c_long) :: exception

    res = Basic()
    exception = c_basic_cot(res%ptr, a%ptr)
    call handle_exception(exception)
end function

function basic_acsc(a) result(res)
    class(basic), intent(in) :: a
    type(basic) :: res
    integer(c_long) :: exception

    res = Basic()
    exception = c_basic_acsc(res%ptr, a%ptr)
    call handle_exception(exception)
end function

function basic_asec(a) result(res)
    class(basic), intent(in) :: a
    type(basic) :: res
    integer(c_long) :: exception

    res = Basic()
    exception = c_basic_asec(res%ptr, a%ptr)
    call handle_exception(exception)
end function

function basic_acot(a) result(res)
    class(basic), intent(in) :: a
    type(basic) :: res
    integer(c_long) :: exception

    res = Basic()
    exception = c_basic_acot(res%ptr, a%ptr)
    call handle_exception(exception)
end function

function basic_sinh(a) result(res)
    class(basic), intent(in) :: a
    type(basic) :: res
    integer(c_long) :: exception

    res = Basic()
    exception = c_basic_sinh(res%ptr, a%ptr)
    call handle_exception(exception)
end function

function basic_cosh(a) result(res)
    class(basic), intent(in) :: a
    type(basic) :: res
    integer(c_long) :: exception

    res = Basic()
    exception = c_basic_cosh(res%ptr, a%ptr)
    call handle_exception(exception)
end function

function basic_tanh(a) result(res)
    class(basic), intent(in) :: a
    type(basic) :: res
    integer(c_long) :: exception

    res = Basic()
    exception = c_basic_tanh(res%ptr, a%ptr)
    call handle_exception(exception)
end function

function basic_asinh(a) result(res)
    class(basic), intent(in) :: a
    type(basic) :: res
    integer(c_long) :: exception

    res = Basic()
    exception = c_basic_asinh(res%ptr, a%ptr)
    call handle_exception(exception)
end function

function basic_acosh(a) result(res)
    class(basic), intent(in) :: a
    type(basic) :: res
    integer(c_long) :: exception

    res = Basic()
    exception = c_basic_acosh(res%ptr, a%ptr)
    call handle_exception(exception)
end function

function basic_atanh(a) result(res)
    class(basic), intent(in) :: a
    type(basic) :: res
    integer(c_long) :: exception

    res = Basic()
    exception = c_basic_atanh(res%ptr, a%ptr)
    call handle_exception(exception)
end function

function basic_csch(a) result(res)
    class(basic), intent(in) :: a
    type(basic) :: res
    integer(c_long) :: exception

    res = Basic()
    exception = c_basic_csch(res%ptr, a%ptr)
    call handle_exception(exception)
end function

function basic_sech(a) result(res)
    class(basic), intent(in) :: a
    type(basic) :: res
    integer(c_long) :: exception

    res = Basic()
    exception = c_basic_sech(res%ptr, a%ptr)
    call handle_exception(exception)
end function

function basic_coth(a) result(res)
    class(basic), intent(in) :: a
    type(basic) :: res
    integer(c_long) :: exception

    res = Basic()
    exception = c_basic_coth(res%ptr, a%ptr)
    call handle_exception(exception)
end function

function basic_acsch(a) result(res)
    class(basic), intent(in) :: a
    type(basic) :: res
    integer(c_long) :: exception

    res = Basic()
    exception = c_basic_acsch(res%ptr, a%ptr)
    call handle_exception(exception)
end function

function basic_asech(a) result(res)
    class(basic), intent(in) :: a
    type(basic) :: res
    integer(c_long) :: exception

    res = Basic()
    exception = c_basic_asech(res%ptr, a%ptr)
    call handle_exception(exception)
end function

function basic_acoth(a) result(res)
    class(basic), intent(in) :: a
    type(basic) :: res
    integer(c_long) :: exception

    res = Basic()
    exception = c_basic_acoth(res%ptr, a%ptr)
    call handle_exception(exception)
end function

function basic_exp(a) result(res)
    class(basic), intent(in) :: a
    type(basic) :: res
    integer(c_long) :: exception

    res = Basic()
    exception = c_basic_exp(res%ptr, a%ptr)
    call handle_exception(exception)
end function

function basic_log(a) result(res)
    class(basic), intent(in) :: a
    type(basic) :: res
    integer(c_long) :: exception

    res = Basic()
    exception = c_basic_log(res%ptr, a%ptr)
    call handle_exception(exception)
end function

!to do: point to c_basic_log10
function basic_log10(a) result(res)
    class(basic), intent(in) :: a
    type(basic) :: res
    integer(c_long) :: exception

    res = Basic()
    exception = c_basic_log(res%ptr, a%ptr)
    call handle_exception(exception)
end function

function basic_lambertw(a) result(res)
    class(basic), intent(in) :: a
    type(basic) :: res
    integer(c_long) :: exception

    res = Basic()
    exception = c_basic_lambertw(res%ptr, a%ptr)
    call handle_exception(exception)
end function

function basic_zeta(a) result(res)
    class(basic), intent(in) :: a
    type(basic) :: res
    integer(c_long) :: exception

    res = Basic()
    exception = c_basic_zeta(res%ptr, a%ptr)
    call handle_exception(exception)
end function

function basic_dirichlet_eta(a) result(res)
    class(basic), intent(in) :: a
    type(basic) :: res
    integer(c_long) :: exception

    res = Basic()
    exception = c_basic_dirichlet_eta(res%ptr, a%ptr)
    call handle_exception(exception)
end function

function basic_gamma(a) result(res)
    class(basic), intent(in) :: a
    type(basic) :: res
    integer(c_long) :: exception

    res = Basic()
    exception = c_basic_gamma(res%ptr, a%ptr)
    call handle_exception(exception)
end function

function basic_loggamma(a) result(res)
    class(basic), intent(in) :: a
    type(basic) :: res
    integer(c_long) :: exception

    res = Basic()
    exception = c_basic_loggamma(res%ptr, a%ptr)
    call handle_exception(exception)
end function

function basic_floor(a) result(res)
    class(basic), intent(in) :: a
    type(basic) :: res
    integer(c_long) :: exception

    res = Basic()
    exception = c_basic_floor(res%ptr, a%ptr)
    call handle_exception(exception)
end function

function basic_ceiling(a) result(res)
    class(basic), intent(in) :: a
    type(basic) :: res
    integer(c_long) :: exception

    res = Basic()
    exception = c_basic_ceiling(res%ptr, a%ptr)
    call handle_exception(exception)
end function

function basic_abs(a) result(res)
    class(basic), intent(in) :: a
    type(basic) :: res
    integer(c_long) :: exception

    res = Basic()
    exception = c_basic_abs(res%ptr, a%ptr)
    call handle_exception(exception)
end function

function basic_sqrt(a) result(res)
    class(basic), intent(in) :: a
    type(basic) :: res
    integer(c_long) :: exception

    res = Basic()
    exception = c_basic_sqrt(res%ptr, a%ptr)
    call handle_exception(exception)
end function

function basic_atan2(a, b) result(res)
    class(basic), intent(in) :: a, b
    type(basic) :: res
    integer(c_long) :: exception

    res = Basic()
    exception = c_basic_atan2(res%ptr, a%ptr, b%ptr)
    call handle_exception(exception)
end function

function basic_atan2_i_left(a, b) result(res)
    class(basic), intent(in) :: a
    integer(kind=int32), intent(in) :: b
    type(basic) :: res
    type(basic) :: temp

    temp = SymInteger(b)
    res = basic_atan2(a, temp)
end function

function basic_atan2_i_right(a, b) result(res)
    integer(kind=int32), intent(in) :: a
    class(basic), intent(in) :: b
    type(basic) :: res
    type(basic) :: temp

    temp = SymInteger(a)
    res = basic_atan2(temp, b)
end function

function basic_atan2_i64_left(a, b) result(res)
    class(basic), intent(in) :: a
    integer(kind=int64), intent(in) :: b
    type(basic) :: res
    type(basic) :: temp

    temp = SymInteger(b)
    res = basic_atan2(a, temp)
end function

function basic_atan2_i64_right(a, b) result(res)
    integer(kind=int64), intent(in) :: a
    class(basic), intent(in) :: b
    type(basic) :: res
    type(basic) :: temp

    temp = SymInteger(a)
    res = basic_atan2(temp, b)
end function

function basic_atan2_f_left(a, b) result(res)
    class(basic), intent(in) :: a
    real(kind=real32), intent(in) :: b
    type(basic) :: res
    type(basic) :: temp

    temp = RealDouble(b)
    res = basic_atan2(a, temp)
end function

function basic_atan2_f_right(a, b) result(res)
    real(kind=real32), intent(in) :: a
    class(basic), intent(in) :: b
    type(basic) :: res
    type(basic) :: temp

    temp = RealDouble(a)
    res = basic_atan2(temp, b)
end function

function basic_atan2_d_left(a, b) result(res)
    class(basic), intent(in) :: a
    real(kind=real64), intent(in) :: b
    type(basic) :: res
    type(basic) :: temp

    temp = RealDouble(b)
    res = basic_atan2(a, temp)
end function

function basic_atan2_d_right(a, b) result(res)
    real(kind=real64), intent(in) :: a
    class(basic), intent(in) :: b
    type(basic) :: res
    type(basic) :: temp

    temp = RealDouble(a)
    res = basic_atan2(temp, b)
end function

function basic_max(d) result(res)
    type(c_ptr), dimension(:) :: d
    type(Basic) :: res
    integer :: i
    type(c_ptr) :: vec
    integer(c_long) :: exception
    vec = c_vecbasic_new()

    do i = 1, size(d)
        exception = c_vecbasic_push_back(vec, d(i))
        call handle_exception(exception)
    end do

    res = Basic()
    exception = c_basic_max(res%ptr, vec)
    call c_vecbasic_free(vec)
    call handle_exception(exception)
end function

end module
