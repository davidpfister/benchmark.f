module symengine_rational

use iso_c_binding, only: c_long
use symengine_interface
use symengine_basic
use exceptions

implicit none

type, extends(Basic) :: Rational
end type Rational

interface Rational
    module procedure rational_new
end interface

private
public :: Rational

contains

function rational_new(a, b)
    integer :: a, b
    !private
    integer(c_long) :: x, y
    integer(c_long) :: exception
    type(Rational) :: rational_new
    
    x = int(a)
    y = int(b)
    rational_new%ptr = c_basic_new_heap()
    exception = c_rational_set_si(rational_new%ptr, x, y)
    call handle_exception(exception)
end function

end module
