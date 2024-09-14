#include <benchmark.inc>
program simple
    use benchmark_kinds
    use benchmark_library
    
    implicit none
    
    type(runner) :: br
    
    interface
        real(r8) function compute_pi(n) result(x)
            import r8
            integer, intent(in) :: n
        end function
    end interface

    benchmark(br, run(10000000, compute_pi))
    benchmark(br, run(100000000, compute_pi))
    benchmark(br, run(200000000, compute_pi))

    read(*,*)
end program

!> @brief Gregory-Leibniz series for calculating Pi
real(r8) function compute_pi(n) result(x)
    use benchmark_kinds
    
    implicit none
    
    integer, intent(in) :: n
    !private
    integer :: i
    
    x = 1.0_r8
    do i = 1, n
        if (mod(i, 2) == 1) then
            x = x - (1.0_r8 / (1.0_r8 + 2.0_r8*real(i, r8)))
        else
            x = x + (1.0_r8 / (1.0_r8 + 2.0_r8*real(i, r8)))
        end if
    end do
    
    x = 4.0_r8*x
end function
            
