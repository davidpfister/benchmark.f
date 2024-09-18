module runner7
    use parser_abstract
    use parameters
    use fparser, only: initf, parsef, evalf
    
    implicit none
    
    private
    
    character(len=*), dimension(11), parameter :: var = ['x ', 'y ', 'z ', 'x1', 'x2', &
            'a ', 'b ', 'c ', 'd ', 'e ', 'f ']
    real(r8), dimension(11) :: val
    
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
        val(1) = 0.175_r8
        val(2) = 0.110_r8
        val(3) = 0.900_r8
        val(4) = 0.508_r8
        val(5) = 30.000_r8
        val(6) = 0.900_r8
        val(7) = 0.100_r8
        val(8) = 0.110_r8
        val(9) = 0.120_r8
        val(10) = 0.120_r8
        val(11) = 0.140_r8
    end subroutine
    
    real(r8) function compute(eq)
        character(*), intent(in) :: eq
        real(r8) :: res
        !private
        call initf(1)
        call parsef(1, eq, var)
        res = evalf(1, val)  
        compute = res
    end function
end module