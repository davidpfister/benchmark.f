module exceptions

use iso_c_binding, only: c_long
implicit none

contains

subroutine handle_exception(a)
    integer(c_long) :: a
    if (a == 1) then
        error stop "Runtime error"
    else if (a == 2) then
        error stop "Division by zero"
    else if (a == 3) then
        error stop "Not implemented"
    else if (a == 4) then
        error stop "Domain error"
    else if (a == 5) then
        error stop "Parse error"
    end if
end subroutine

end module
