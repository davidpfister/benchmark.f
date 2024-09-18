module runner2
    use parser_abstract
    use evaluate

    implicit none

    private

    character(len=*), dimension(11), parameter :: var = ['x ', 'y ', 'z ', 'x1', 'x2', &
    'a ', 'b ', 'c ', 'd ', 'e ', 'f ']
    real(kind=kind(0.0d0)), dimension(11) :: val

    type, extends(parser_x), public :: eqnevaluate
        private
    contains
        procedure, nopass :: initialize
        procedure, nopass :: compute
    end type

    interface eqnevaluate
        module procedure :: constructor_null
    end interface

contains

    type(eqnevaluate) function constructor_null() result(this)
        this%name = 'Evaluate'
    end function

    subroutine initialize()
        call defparam(var(1), 0.175_r8)
        call defparam(var(2), 0.110_r8)
        call defparam(var(3), 0.900_r8)
        call defparam(var(4), 0.508_r8)
        call defparam(var(5), 30.000_r8)
        call defparam(var(6), 0.900_r8)
        call defparam(var(7), 0.100_r8)
        call defparam(var(8), 0.110_r8)
        call defparam(var(9), 0.120_r8)
        call defparam(var(10), 0.120_r8)
        call defparam(var(11), 0.140_r8)
    end subroutine

    real(r8) function compute(eq)
        character(*), intent(in) :: eq
        call evalexpr(eq, compute)
    end function
end module
