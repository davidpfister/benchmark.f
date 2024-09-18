#ifdef _INCLUDE_SYMENGINE
module runner10
    use parser_abstract
    use parameters
    use symengine
    
    implicit none
    
    private
    
    type(Symbol) :: var(11)
    type(RealDouble) :: val(11)
    type(Basic) :: func
    
    type, extends(parser_x), public :: symengine_parser
        private
    contains
        procedure, nopass :: initialize
        procedure, nopass :: compute
    end type
    
    interface symengine_parser
        module procedure :: constructor_null
    end interface

contains

    type(symengine_parser) function constructor_null() result(this)
        this%name = 'symengine'
    end function
    
    subroutine initialize()
        var = [Symbol('x'), Symbol('y'), Symbol('z'), Symbol('x1'), Symbol('x2'), &
            Symbol('a'), Symbol('b'), Symbol('c'), Symbol('d'), Symbol('e'), Symbol('func')]
        val(1) = RealDouble(0.175)
        val(2) = RealDouble(0.110)
        val(3) = RealDouble(0.900)
        val(4) = RealDouble(0.508)
        val(5) = RealDouble(30.000)
        val(6) = RealDouble(0.900)
        val(7) = RealDouble(0.100)
        val(8) = RealDouble(0.110)
        val(9) = RealDouble(0.120)
        val(10) = RealDouble(0.120)
        val(11) = RealDouble(0.140)
    end subroutine
    
    real(r8) function compute(eq)
        character(*), intent(in) :: eq
        !private
        type(Basic) :: func
        integer :: j
        func = parse(eq)
        do j=1,11
            func = func%subs(var(j), val(j))
        end do
        func = func%evalf()
        compute = func%dbl()
    end function
end module
#endif