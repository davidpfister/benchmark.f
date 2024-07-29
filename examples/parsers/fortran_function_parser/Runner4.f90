module test_function_parser
    use parser_abstract
    use function_parser, wp => fparser_rk
    use parameters
    
    implicit none
    
    private
    
    character(len=*), dimension(11), parameter :: var = ['x', 'y', 'z', 'x1', 'x2', &
            'a', 'b', 'c', 'd', 'e', 'f']
    real(wp), dimension(11) :: x
    
    type, extends(parser_x), public :: ffunction_parser
        private
    contains
        procedure, nopass :: initialize
        procedure, nopass :: compute
    end type
    
    interface ffunction_parser
        module procedure :: constructor_null
    end interface

contains

    type(ffunction_parser) function constructor_null() result(this)
        this%name = 'function_parser'
    end function
    
    subroutine initialize()
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
        real(wp) :: res
        !private
        type(fparser) :: parser
        call parser%parse(eq, var, .false.) 
        call parser%evaluate(x, res)
        compute = res
    end function
    
end module