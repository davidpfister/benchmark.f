module rhofunc
    implicit none
    
    public :: poisson_optimized, &
              poisson_naive
    
    integer, parameter :: rp = kind(0.0d0)
    real(rp),parameter :: epsilon0 = 8.85e-12_rp, a = 0.01_rp
    
    contains
    
    pure subroutine poisson_naive(treshold, m)
        real(rp), intent(in) :: treshold
        integer, intent(in)  :: m
        !private
        integer :: i, j, iter
        real(rp) :: delta, b, e, phiprime(m,m), phi(m,m), a2, rhoarr(m,m), temp(m,m)
    

        delta = 1.0_rp
        iter = 0
        phiprime(:,:) = 0.0_rp
        phi(:,:) = 0.0_rp
    
        do while (delta > treshold)
            iter = iter + 1
            a2 = a**2.0_rp
            do i=2, m-1
                do j=2, m-1
                    phiprime(i,j) = (phi(i+1,j) + phi(i-1,j) + phi(i,j+1) + phi(i,j-1))/4.0_rp &
                    + a2/4.0_rp/epsilon0*rho(i*a,j*a)
                end do
            end do
            delta = maxval(abs(phiprime - phi))
            temp = phi
            phi = phiprime 
            phiprime = temp
        end do
    end subroutine
    
    pure subroutine poisson_optimized(treshold, m)
        real(rp), intent(in) :: treshold
        integer, intent(in)  :: m
        !private
        integer :: i,j, iter
        real(rp) :: delta, phiprime(m,m), phi(m,m), a2, rhoarr(m,m)
        real(rp), parameter :: quart = 0.25_rp

        delta = 1.0_rp
        iter = 0
        phiprime(:,:) = 0.0_rp
        phi(:,:) = 0.0_rp
        do i=1, m
            do j=1, m
                rhoarr(i,j) = rho(i*a,j*a)
            end do
        end do
    
        do while (delta > treshold )
            iter = iter + 1
            a2 = a**2
            do i=2, m-1
                do j=2, m-1
                    phiprime(i,j) = (phi(i+1,j) + phi(i-1,j) + phi(i,j+1) + phi(i,j-1))*quart &
                    + a2*quart/epsilon0*rhoarr(i,j)
                end do
            end do
            delta = maxval(abs(phiprime - phi))
            phi = phiprime 
        end do
    end subroutine
    
    pure real(rp) function rho(x,y)
        real(rp), intent(in) :: x,y
        if (x > 0.6_rp .and. x < 0.8_rp .and. y > 0.6_rp .and. y < 0.8_rp) then
            rho = 1.0_rp
        else if (x> 0.2_rp .and. x < 0.4_rp .and. y > 0.2_rp .and. y < 0.4_rp) then
            rho = -1.0_rp
        else
            rho = 0.0_rp
        end if
    end function

end module