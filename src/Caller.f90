module benchmark_caller
    use benchmark_kinds
    
    implicit none
    
    interface
        subroutine caller_a0_x(f)
            external :: f
        end subroutine
    end interface
    
    interface
        subroutine caller_a1_x(f, a1)
            external :: f
            class(*), intent(in) :: a1
        end subroutine
    end interface
    
    interface
        subroutine caller_a2_x(f, a1, a2)
            external :: f
            class(*), intent(in) :: a1
            class(*), intent(in) :: a2
        end subroutine
    end interface
    
    interface
        subroutine caller_a3_x(f, a1, a2, a3)
            external :: f
            class(*), intent(in) :: a1
            class(*), intent(in) :: a2
            class(*), intent(in) :: a3
        end subroutine
    end interface
    
    interface
        subroutine caller_a4_x(f, a1, a2, a3, a4)
            external :: f
            class(*), intent(in) :: a1
            class(*), intent(in) :: a2
            class(*), intent(in) :: a3
            class(*), intent(in) :: a4
        end subroutine
    end interface
    
    interface
        subroutine caller_a5_x(f, a1, a2, a3, a4, a5)
            external :: f
            class(*), intent(in) :: a1
            class(*), intent(in) :: a2
            class(*), intent(in) :: a3
            class(*), intent(in) :: a4
            class(*), intent(in) :: a5
        end subroutine
    end interface
    
    interface
        subroutine caller_a6_x(f, a1, a2, a3, a4, a5, a6)
            external :: f
            class(*), intent(in) :: a1
            class(*), intent(in) :: a2
            class(*), intent(in) :: a3
            class(*), intent(in) :: a4
            class(*), intent(in) :: a5
            class(*), intent(in) :: a6
        end subroutine
    end interface
    
    interface
        subroutine caller_a7_x(f, a1, a2, a3, a4, a5, a6, a7)
            external :: f
            class(*), intent(in) :: a1
            class(*), intent(in) :: a2
            class(*), intent(in) :: a3
            class(*), intent(in) :: a4
            class(*), intent(in) :: a5
            class(*), intent(in) :: a6
            class(*), intent(in) :: a7
        end subroutine
    end interface
end module