module test_fparser
    use parser_abstract
    use parameters, only: wp => r4
    use fparser, only: initf, parsef, evalf
    
    implicit none
    
    private
    
    character(len=*), dimension(11), parameter :: var = ['x', 'y', 'z', 'x1', 'x2', &
            'a', 'b', 'c', 'd', 'e', 'f']
    real(kind=kind(0.0d0)), dimension(11) :: x
    
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
    
    real(r8) function compute(i)
        integer, intent(in) :: i
        real(wp) :: res
        !private
        call initf(1)
        call parsef(1, test_data(i), var)
        res = evalf(1, x)  
        compute = res
    end function
end module