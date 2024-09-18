#include <benchmark.inc>
program simple
    use benchmark_kinds
    use benchmark_library
    
    implicit none
    
    type(runner) :: br
    
    interface
        subroutine compute_pi(n)
            class(*), intent(in) :: n
        end subroutine
    end interface
#ifdef _DEBUG
    benchmark(br, run(10000000, compute_pi))
    benchmark(br, run(100000000, compute_pi))
    benchmark(br, run(200000000, compute_pi))
#else
    benchmark(br, run(100000000000_i8, compute_pi))
    benchmark(br, run(1000000000000_i8, compute_pi))
    benchmark(br, run(10000000000000_i8, compute_pi))
#endif
    call br%dispose()
    
    read(*,*)
    
    end program

!> @brief Gregory-Leibniz series for calculating Pi
subroutine compute_pi(n)
    use benchmark_kinds
    
    implicit none
 
#ifndef __INTEL_COMPILER    
    class(*), intent(in) :: n
#else
    integer(i8), intent(in) :: n
#endif
    !private
    integer(i8) :: i
    real(r8) :: x
    
#ifndef __INTEL_COMPILER  
    select type(n)
    type is (integer(i8))
#endif
        x = 1.0_r8
        do i = 1, n
            if (mod(i, 2) == 1) then
                x = x - (1.0_r8 / (1.0_r8 + 2.0_r8*real(i, r8)))
            else
                x = x + (1.0_r8 / (1.0_r8 + 2.0_r8*real(i, r8)))
            end if
        end do
#ifndef __INTEL_COMPILER 
    end select
#endif
    x = 4.0_r8*x
end subroutine
            
