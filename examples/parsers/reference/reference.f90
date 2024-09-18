module test_reference
    use parser_abstract
    use parameters
    
    implicit none; private
    
    integer :: count = 0
    character(200) :: eqstring = ' '
    
    type, extends(parser_x), public :: reference
        procedure(double precision), nopass, pointer :: ptr
    contains
        procedure, nopass :: initialize
        procedure, nopass :: compute
    end type
    
    type(reference), dimension(neq) :: refs
    
    interface reference
        module procedure :: reference_new
    end interface

    contains
    
    function reference_new() result(this)
        type(reference) :: this
        this%name = "benchmark"
    end function
    
    subroutine initialize()
        refs(1)%ptr => f1
        refs(2)%ptr => f2
        refs(3)%ptr => f3
        refs(4)%ptr => f4
        refs(5)%ptr => f5
        refs(6)%ptr => f6
        refs(7)%ptr => f7
        refs(8)%ptr => f8
        refs(9)%ptr => f9
        refs(10)%ptr => f10
        refs(11)%ptr => f11
        refs(12)%ptr => f12
        refs(13)%ptr => f13
        refs(14)%ptr => f14
        refs(15)%ptr => f15
        refs(16)%ptr => f16
        refs(17)%ptr => f17
        refs(18)%ptr => f18
        refs(19)%ptr => f19
        refs(20)%ptr => f20
        refs(21)%ptr => f21
        refs(22)%ptr => f22
        refs(23)%ptr => f23
    end subroutine
    
    real(r8) function compute(eq) result(res)
        character(*), intent(in) :: eq
        if (eqstring /= eq) then
            count = count + 1
            eqstring = eq
        end if
        res = refs(count)%ptr(eq)
    end function
    
    real(r8) function f1(eq) result(res)
        character(*), intent(in) :: eq
        res = a+b*x1
    end function

    real(r8) function f2(eq) result(res)
        character(*), intent(in) :: eq
        res = (a*x**b)/(c+x**b)
    end function
    
    real(r8) function f3(eq) result(res)
        character(*), intent(in) :: eq
        res = (a*x)/(b+(x*(1+x/c)))
    end function

    real(r8) function f4(eq) result(res)
        character(*), intent(in) :: eq
        res = a+b*exp(c*x)+d*exp(e*x)
    end function

    real(r8) function f5(eq) result(res)
        character(*), intent(in) :: eq
        res = a+b*(exp(c*x) - 1)/c
    end function
    
    real(r8) function f6(eq) result(res)
        character(*), intent(in) :: eq
        res = a+b*log(x)+c*log(x)**2
    end function

    real(r8) function f7(eq) result(res)
        character(*), intent(in) :: eq
        res = a-log(1+b*exp(-c*x))
    end function

    real(r8) function f8(eq) result(res)
        character(*), intent(in) :: eq
        res = (a+b*x)/(c+x)
    end function
  
    real(r8) function f9(eq) result(res)
        character(*), intent(in) :: eq
        res = a+b*exp(-(c*x))
    end function

    real(r8) function f10(eq) result(res)
        character(*), intent(in) :: eq
        res = a+b*sin(2*3.14*x/c+d)
    end function

    real(r8) function f11(eq) result(res)
        character(*), intent(in) :: eq
        res = a+b*sin(2*4.14*x/c+d)**2
    end function

    real(r8) function f12(eq) result(res)
        character(*), intent(in) :: eq
        res = 1-exp(-a*x)
    end function

    real(r8) function f13(eq) result(res)
        character(*), intent(in) :: eq
        res = a+b*x1+c*x2
    end function

    real(r8) function f14(eq) result(res)
        character(*), intent(in) :: eq
        res = a+b*log(x1)+c*log(x2)
    end function

    real(r8) function f15(eq) result(res)
        character(*), intent(in) :: eq
        res = a*x1**b*x2**c
    end function

    real(r8) function f16(eq) result(res)
        character(*), intent(in) :: eq
        res = cosh(log(abs(y*z+x**2+x1**x2)))+a*d*(exp(c*f)+154.3)
    end function

    real(r8) function f17(eq) result(res)
        character(*), intent(in) :: eq
        res = a+b*log(x1)+c*x2+d*x2**2
    end function

    real(r8) function f18(eq) result(res)
        character(*), intent(in) :: eq
        res = atan(sinh(log(abs(exp(z/x)*sqrt(y+a**c+f*e)))))
    end function

    real(r8) function f19(eq) result(res)
        character(*), intent(in) :: eq
        res = a+b/x1+c*log(x2)+d*log(x2)**2+e*log(x2)**3
    end function

    real(r8) function f20(eq) result(res)
        character(*), intent(in) :: eq
        res = atan(sinh(log(abs(exp(z/x)*sqrt(y+a**c+f*e)))))*cos(log(abs(sqrt(y+a**c+f*e))))
    end function

    real(r8) function f21(eq) result(res)
        character(*), intent(in) :: eq
        res = a+b*log(x1)+c*log(x1)**2+d/x2+e/x2**2
    end function

    real(r8) function f22(eq) result(res)
        character(*), intent(in) :: eq
        res = (x+a)/(b+c*(x+a)+d*(x+a)**2)
    end function

    real(r8) function f23(eq) result(res)
        character(*), intent(in) :: eq
        res = a+b*log(x1)+c*log(x1)**2+d*log(x1)**3+e/x
    end function

end module