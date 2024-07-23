!> @ingroup group_all
!> @author davidpfister
module benchmark_steady_state_detection
    use benchmark_kinds
    
    implicit none
    
    private 
    
    public :: ssd
    
    contains
    
    !> @brief R. R. Rhinehart, Automated steady and transient state identification in
    !> noisy processes, in Proceedings of the American Control Conference, 2013,
    !> no. June 2013, pp. 4477–4493.
    real(r8) function ssd(x, offset, alpha)
        real(r8), intent(in)            :: x(:)
        integer, intent(in)             :: offset
        real(r8), intent(in)            :: alpha
        !private
        integer  :: i, j, k, n, ierr
        real(r8) :: mu, sd
        real(r8) :: y(size(x))
        integer df
        real(r8) fx, fx2
        real(r8) lambda
        integer n_data
        real(r8) z
        
        n = size(y)
        y(1:n-offset) = x(offset+1:n)
        y(n-offset+1:) = x(:offset)
            
        mu = sum(y)/real(n, r8)

        sd = 0.0_r8;
        do j = 1, n
            sd = sd + (y(j) - mu)**2
        end do
        sd = sqrt(sd/real(n-2, r8))

        ssd = 0.0_r8
        associate(t_crit => student(n, alpha))
            do j = 1, n
                if (abs(y(j)-mu) <=  t_crit * sd) ssd = ssd + 1.0_r8
            end do
        end associate
        ssd = ssd/real(n, r8)
            
    end function
    
    real(r8) function student(dof, a) result(t)
        integer, intent(in)     :: dof
        real(r8), intent(in)    :: a
        !private
        real(r8) :: f, fold, x, p, integral
        real(r8), parameter :: dt = 1.0e-2
        
        f(x) = (1+x**2/real(dof, r8))**(-(dof+1)/2.0_r8)*gamma((dof+1)/2.0_r8)/sqrt(3.14*dof)/gamma(dof/2.0_r8)
        
        integral = 0.0_r8
        fold = 0.0_r8
        p = 0.5*a
        t = 1.0e3
        
        do while (f(t) > 1.0e-20)
            t = t * 2
        end do
        
        do while (integral < p)
            associate(fnew => f(t+dt))
                integral = integral + 0.5*(fold+fnew)*dt
                t = t - dt
                fold = fnew
                if (t < 0) stop
            end associate
        end do
    end function

end module