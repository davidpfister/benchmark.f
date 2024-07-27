module symengine_symbol

use iso_c_binding, only: c_long, c_null_char
use symengine_interface
use symengine_basic
use exceptions

implicit none

type, extends(Basic) :: Symbol
end type Symbol

interface Symbol
    module procedure symbol_new
end interface

private
public :: Symbol

contains

function symbol_new(c)
    character(*) :: c
    !private
    character(len_trim(c) + 1) :: new_c
    integer(c_long) :: exception
    type(Symbol) :: symbol_new
    
    new_c = trim(c) // c_null_char
    symbol_new%ptr = c_basic_new_heap()
    exception = c_symbol_set(symbol_new%ptr, new_c) 
    call handle_exception(exception)
end function

end module
