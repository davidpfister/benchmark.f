module test_fparser
    use parser_abstract
    use parameters
    use fparser, only: initf, parsef, evalf
    
    implicit none
    
    private
    
    character(len=*), dimension(11), parameter :: var = ['x', 'y', 'z', 'x1', 'x2', &
            'a', 'b', 'c', 'd', 'e', 'f']
    real(r8), dimension(11) :: x
    
    type, extends(parser_x), public :: ffparser
        private
    contains
        procedure, nopass :: initialize
        procedure, nopass :: compute
    end type
    
    interface ffparser
        module procedure :: constructor_null
    end interface

contains

    type(ffparser) function constructor_null() result(this)
        this%name = 'fparser'
    end function
    
    subroutine initialize()
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
    
    real(r8) function compute(i)
        integer, intent(in) :: i
        real(r8) :: res
        !private
        call initf(1)
        call parsef(1, test_data(i), var)
        res = evalf(1, x)  
        compute = res
    end function
end module