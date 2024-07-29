!feq-parse does not deal with variable names with more than 1 character
module test_FEQParse
    use parser_abstract
    use FEQParse
    use parameters
    
    implicit none
    
    private 
    
    character(len=10),dimension(1:11) :: independentVars
    real(r8) :: x(1:11)
    
    
    type, extends(parser_x), public :: FEQParse_parser
        private
    contains
        procedure, nopass :: initialize
        procedure, nopass :: compute
    end type
    
    interface FEQParse_parser
        module procedure :: constructor_null
    end interface

contains

    type(FEQParse_parser) function constructor_null() result(this)
        this%name = 'FEQParse'
    end function
    
    subroutine initialize()
        independentVars = ['x', 'y', 'z', 'x1', 'x2', &
            'a', 'b', 'c', 'd', 'e', 'f']
        x(1) = 0.175_r8
        x(2) = 0.110_r8
        x(3) = 0.900_r8
        x(4) = 0.508_r8
        x(5) = 30.000_r8
        x(6) = 0.900_r8
        x(7) = 0.100_r8
        x(8) = 0.110_r8
        x(9) = 0.120_r8
        x(10) = 0.120_r8
        x(11) = 0.140_r8
    end subroutine
    
    real(r8) function compute(eq)
        character(*), intent(in) :: eq
        !private
        type(EquationParser) :: f
        f = EquationParser(eq, independentVars)
        compute = f%evaluate(x)
    end function
end module