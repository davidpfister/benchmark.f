module test_utility
    use benchmark_kinds
    
    implicit none
    
    type :: arg_type
        real(r8), allocatable :: r(:)
        integer :: i
    end type
    
    contains
    
    pure subroutine test_dummy(a, m)
        real(r8), intent(in) :: a
        integer, intent(in)  :: m
        !private
        integer :: i
        real(r8) :: b
        
        b = a
        do i = 1, m
            b = b + b
        end do
    end subroutine
    
    subroutine caller_dummy(f, a, m)
        external :: f
        real(r8), intent(in) :: a
        integer, intent(in)  :: m
        !private
        real(r8) :: b
        
        b = a
        call f(b, m)
    end subroutine
    
    pure subroutine test_dummy2(a, m)
        real(r8), intent(inout) :: a
        integer, intent(in)  :: m
        !private
        integer :: i
        
        do i = 1, m
            a = a + a
        end do
    end subroutine
    
end module