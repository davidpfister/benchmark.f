module runner6
    use parser_abstract
    use FortranParser_parameters, only: wp => rn
    use FortranParser, only: EquationParser
    use parameters
    
    implicit none
    
    private
    
    character(len=*), dimension(11), parameter :: var = ['x ', 'y ', 'z ', 'x1', 'x2', &
            'a ', 'b ', 'c ', 'd ', 'e ', 'f ']
    real(wp), dimension(11) :: val
    
    type, extends(parser_x), public :: ffortranparser
        private
    contains
        procedure, nopass :: initialize
        procedure, nopass :: compute
    end type
    
    interface ffortranparser
        module procedure :: constructor_null
    end interface

contains

    type(ffortranparser) function constructor_null() result(this)
        this%name = 'FortranParser'
    end function
    
    subroutine initialize()
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
        real(wp) :: res
        !private
        type(EquationParser) :: eqParser
        eqParser = EquationParser(eq, var)
        res = eqParser%evaluate(val)
        compute = res
    end function
end module