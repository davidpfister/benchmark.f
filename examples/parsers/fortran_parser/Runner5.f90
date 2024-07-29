module test_interpreter
    use parser_abstract
    use interpreter
    use parameters
    
    implicit none
    
    private
    
    integer, parameter :: wp = 8
    character(:), allocatable :: var
    real(wp), dimension(11) :: x
    
    type, extends(parser_x), public :: interpreter_parser
        private
    contains
        procedure, nopass :: initialize
        procedure, nopass :: compute
    end type
    
    interface interpreter_parser
        module procedure :: constructor_null
    end interface

contains

    type(interpreter_parser) function constructor_null() result(this)
        this%name = 'interpreter'
    end function
    
    subroutine initialize()
        var = 'x y z x1 x2 a b c d e f'
        x(1) = 0.175_wp
        x(2) = 0.110_wp
        x(3) = 0.900_wp
        x(4) = 0.508_wp
        x(5) = 30.000_wp
        x(6) = 0.900_wp
        x(7) = 0.100_wp
        x(8) = 0.110_wp
        x(9) = 0.120_wp
        x(10) = 0.120_wp
        x(11) = 0.140_wp
    end subroutine
    
    real(r8) function compute(eq)
        character(*), intent(in) :: eq
        !private
        character(len=255) :: error1, error2
        real(wp) :: res, rerr, ttol
        integer :: funcnum

        call s_createfn(eq, var, funcnum, error1)
        call s_evaluatefn(funcnum, x, res, error2)
        compute = res
    end function
end module