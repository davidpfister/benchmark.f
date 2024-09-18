module runner8
    use parser_abstract
    use fee
    use parameters
    
    implicit none
    
    private
    
    integer, parameter  :: rk = selected_real_kind(p=13,r=200)
    integer, parameter  :: wp = 8
    character(10) :: var(11)
    real(rk) :: val(11)
    
    type, extends(parser_x), public :: fee_parser
        private
    contains
        procedure, nopass :: initialize
        procedure, nopass :: compute
    end type
    
    interface fee_parser
        module procedure :: constructor_null
    end interface

contains

    type(fee_parser) function constructor_null() result(this)
        this%name = 'fee'
    end function
    
    subroutine initialize()
        var = ['x ', 'y ', 'z ', 'x1', 'x2', &
        'a ', 'b ', 'c ', 'd ', 'e ', 'f ']
        val(1) = 0.175_realkind
        val(2) = 0.110_realkind
        val(3) = 0.900_realkind
        val(4) = 0.508_realkind
        val(5) = 30.000_realkind
        val(6) = 0.900_realkind
        val(7) = 0.100_realkind
        val(8) = 0.110_realkind
        val(9) = 0.120_realkind
        val(10) = 0.120_realkind
        val(11) = 0.140_realkind
    end subroutine
    
    real(r8) function compute(eq)
        character(*), intent(in) :: eq
        !private
        real(r8) :: res
        character (len = 5)  :: status
        character(len=256) :: func
        func = eq

        call init (func, var, status)
        res = evaluate(val)
        call destroyfunc()
        compute = res
    end function
end module