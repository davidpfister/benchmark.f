module runner5
    use parser_abstract
    use interpreter
    use parameters
    
    implicit none
    
    private
    
    integer, parameter :: wp = 8
    character(:), allocatable :: var
    real(wp), dimension(11) :: val
    
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
        val(1) = 0.175_wp
        val(2) = 0.110_wp
        val(3) = 0.900_wp
        val(4) = 0.508_wp
        val(5) = 30.000_wp
        val(6) = 0.900_wp
        val(7) = 0.100_wp
        val(8) = 0.110_wp
        val(9) = 0.120_wp
        val(10) = 0.120_wp
        val(11) = 0.140_wp
    end subroutine
    
    real(r8) function compute(eq)
        character(*), intent(in) :: eq
        !private
        character(len=255) :: error1, error2
        real(wp) :: res, rerr, ttol
        integer :: funcnum

        call s_createfn(eq, var, funcnum, error1)
        call s_evaluatefn(funcnum, val, res, error2)
        compute = res
    end function
end module