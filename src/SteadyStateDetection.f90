!> @defgroup group_ssd benchmark_steady_state_detection
!! @brief Provides methods for detecting steady-state conditions
!! based on t-test.
!! @par
!! <h2>Examples</h2>
!! The following example demonstrates some of the methods found in the 
!! @link benchmark_steady_state_detection benchmark_steady_state_detection @endlink module.
!!
!! @snippet snippet.f90 ssd
!! @par
!! <h2>Remarks</h2>
!! The steady-state detection algorithm is based on the work by 
!! J. D. Kelly and J. D. Hedengren, “A steady-state detection (SSD) 
!! algorithm to detect non-stationary drifts in processes,” 
!! J. Process Control, vol. 23, no. 3, pp. 326–331, 2013.
!! The original algorithm has been altered to be used with dynamic real-time 
!! data.
!! @{
module benchmark_steady_state_detection
    use benchmark_kinds
    
    implicit none; private 
    
    public :: ssd
    
    contains
    
    !> @ingroup group_ssd
    !> @brief Compute the probability that a sample `x` reached steady-state.
    !!        The SSD algorithm presented in this work is also window-based and utilizes the
    !!        Student-t test to determine if the difference between the process signal value 
    !!        minus its mean is above or below the standard-deviation times its statistical critical value. If less
    !!        than, then that time instant or point is steady and if greater than, then it is unsteady
    !!        where the aggregation is computed over the window approximating a probability or
    !!        frequency of being at steady-state. 
    !! @param[in] input sample of real(r8) values
    !! @param[in] offset Index of the first values inside the periodic array x
    !! @param[in] alpha Significance level for the student test.
    !! 
    !! The mean is defined as:
    !! @f[
    !! \mu = \frac{1}{n}\left(\sum x-m\sum i\right)
    !! @f]
    !! where @f$m@f$ is the slope of a linear drift. In the present case, @f$m=0@f$
    !! It comes that the standard deviation is given by 
    !! @f[
    !! \sigma = \sqrt{\frac{1}{n-2}\sum (x-\mu)^2}
    !! @f]
    !! At this point along with a specified Student-t critical or threshold value at a particu100 lar significance level α and degrees-of-freedom n, all of the necessary information is
    !! available to test the null-hypothesis that the process signal is steady or is stationary
    !! about @f$\mu@f$
    !! @f[
    !! if\  x - \mu < t_{crit}-\sigma\ , then\  1,\  else\  0
    !! @f]
    !! @f$t_{crit}@f$ is evaluated for a given significance level and a given degree of freedom
    !! using the t-distribution.
    real(r8) function ssd(x, offset, alpha) result(res)
        real(r8), intent(in)            :: x(:)
        integer, intent(in)             :: offset
        real(r8), intent(in)            :: alpha
        !private
        integer  :: j, n
        real(r8) :: mu, sd
        real(r8) :: y(size(x))
        
        n = size(y)
        y(1:n-offset) = x(offset+1:n)
        y(n-offset+1:) = x(:offset)
            
        mu = sum(y)/real(n, r8)

        sd = 0.0_r8;
        do j = 1, n
            sd = sd + (y(j) - mu)**2
        end do
        sd = sqrt(sd/real(n-2, r8))

        res = 0.0_r8
        associate(t_crit => student(n, alpha))
            do j = 1, n
                if (abs(y(j)-mu) <=  t_crit * sd) res = res + 1.0_r8
            end do
        end associate
        res = res/real(n, r8)
            
    end function
    
    !> @private
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
!> @}