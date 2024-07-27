module test_symengine
    use parser_abstract
    use parameters
    use symengine
    
    implicit none
    
    private
    
    type(Symbol) :: var(11)
    type(RealDouble) :: x(11)
    type(Basic) :: f
    
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
            Symbol('a'), Symbol('b'), Symbol('c'), Symbol('d'), Symbol('e'), Symbol('f')]
        x(1) = RealDouble(0.175)
        x(2) = RealDouble(0.110)
        x(3) = RealDouble(0.900)
        x(4) = RealDouble(0.508)
        x(5) = RealDouble(30.000)
        x(6) = RealDouble(0.900)
        x(7) = RealDouble(0.100)
        x(8) = RealDouble(0.110)
        x(9) = RealDouble(0.120)
        x(10) = RealDouble(0.120)
        x(11) = RealDouble(0.140)
    end subroutine
    
    real(r8) function compute(i)
        integer, intent(in) :: i
        !private
        type(Basic) :: f
        integer :: j
        f = parse(test_data(i))
        do j=1,11
            f = f%subs(var(j), x(j))
        end do
        f = f%evalf()
        compute = f%dbl()
    end function
end module