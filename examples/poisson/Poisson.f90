module rhofunc
    implicit none
    public :: test_poisson
    
    integer, parameter :: dp=kind(0.d0)
    real(dp),parameter :: epsilon0=8.85E-12_dp, a=0.01_dp
    
    contains
    
    pure subroutine test_poisson(target, M)
        real(dp), intent(in) :: target
        integer, intent(in)  :: M
        !private
        integer            :: i,j, iter
        real(dp)           :: delta, phiprime(M,M), phi(M,M), a2, rhoarr(M,M)
    

        delta = 1.0_dp
        iter = 0
        phiprime(:,:) = 0.0_dp
        phi(:,:) = 0.0_dp
        do i=1, M
            do j=1, M
                rhoarr(i,j) = rho(i*a,j*a)
            end do
        end do
    
        do while (delta > target )
            iter = iter + 1
            a2 = a**2.0_dp
            do i=2, M-1
                do j=2, M-1
                    phiprime(i,j) = (phi(i+1,j) + phi(i-1,j) + phi(i,j+1) + phi(i,j-1))/4.0_dp &
                    + a2/4.0_dp/epsilon0*rhoarr(i,j)
                end do
            end do
            delta = maxval(abs(phiprime - phi))
            phi = phiprime 
        end do
    end subroutine
    
    pure real(dp) function rho(x,y)
        real(dp), intent(in) :: x,y
        if (x > 0.6_dp .and. x < 0.8_dp .and. y > 0.6_dp .and. y<0.8_dp) then
            rho = 1.0_dp
        else if (x> 0.2_dp .and. x<0.4_dp .and. y>0.2_dp .and. y<0.4_dp) then
            rho = -1.0_dp
        else
            rho = 0.0_dp
        end if
    end function

end module

#include <benchmark.inc>    
program poisson
    use rhofunc
    use benchmark_library
    
    implicit none
    
    type(runner) :: br

    !calling using preprocessor macro
    benchmark(br, run(1.0d-6, 30, test_poisson))
    
    !calling using the derived type and the 'full' name
    br%name = '(1.0d-6, 30, test_poisson)'
    call br%run(1.0d-6, 30, test_poisson)
    
    !calling using the derived type and the 'full' name without brackets
    br%name = '1.0d-6, 30, test_poisson'
    call br%run(1.0d-6, 30, test_poisson)
    
    !calling using the derived type and partial name
    br%name = '30, test_poisson'
    call br%run(1.0d-6, 30, test_poisson)
    
    !calling using the derived type and derived type
    br%name = '1.0d-6,, test_poisson'
    call br%run(1.0d-6, 30, test_poisson)
    
    !calling using the derived type and only the function name
    br%name = 'test_poisson'
    call br%run(1.0d-6, 30, test_poisson)
    
    !calling using the derived type and and empty name
    br%name = ''
    call br%run(1.0d-6, 30, test_poisson)

end program
