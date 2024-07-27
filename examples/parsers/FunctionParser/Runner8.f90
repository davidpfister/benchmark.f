module test_fee
    use parser_abstract
    use fee
    use parameters
    
    implicit none
    
    private
    
    integer, parameter	 :: realkind = selected_real_kind(p=13,r=200)
    integer, parameter :: wp = 8
    character(10) :: var(11)
    real(realkind) :: x(11)
    
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
        var = ['x', 'y', 'z', 'x1', 'x2', &
            'a', 'b', 'c', 'd', 'e', 'f']
        x(1) = 0.175_realkind
        x(2) = 0.110_realkind
        x(3) = 0.900_realkind
        x(4) = 0.508_realkind
        x(5) = 30.000_realkind
        x(6) = 0.900_realkind
        x(7) = 0.100_realkind
        x(8) = 0.110_realkind
        x(9) = 0.120_realkind
        x(10) = 0.120_realkind
        x(11) = 0.140_realkind
    end subroutine
    
    real(r8) function compute(i)
        integer, intent(in) :: i
        !private
        real(r8) :: res
        character (len = 5)  :: status
        character(len=256) :: func
        func = test_data(i)

        call init (func, var, status)
        res = evaluate(x)
        call destroyfunc()
        compute = res
    end function
end module