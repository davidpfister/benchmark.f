module test_M_calculator
    use parser_abstract
    use M_calculator, only : dnum0
    use parameters
    
    implicit none
    
    private 
    
    type, extends(parser_x), public :: M_calculator_parser
        private
    contains
        procedure, nopass :: initialize
        procedure, nopass :: compute
    end type
    
    interface M_calculator_parser
        module procedure :: constructor_null
    end interface

contains

    type(M_calculator_parser) function constructor_null() result(this)
        this%name = 'interpreter'
    end function
    
    subroutine initialize()
        real(r8) :: res
        res = dnum0('x = 0.175')
        res = dnum0('y = 0.110')
        res = dnum0('z = 0.900')
        res = dnum0('x1 = 0.508')
        res = dnum0('x2 = 30.000')
        res = dnum0('a = 0.900')
        res = dnum0('b = 0.100')
        res = dnum0('c = 0.110')
        res = dnum0('d = 0.120')
        res = dnum0('e = 0.120')
        res = dnum0('f = 0.140')
    end subroutine
    
    real(r8) function compute(i)
        integer, intent(in) :: i
        compute = dnum0(test_data(i))
    end function
end module