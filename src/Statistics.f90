
!> @defgroup group_statistics benchmark_statistics
!! @brief Statistics module
module benchmark_statistics
    use benchmark_kinds
    
    implicit none; private
    
    public :: stats
    
    !> @class stats
    !! @ingroup group_statistics
    !! @brief Provides methods to compute mean, standard deviation and 
    !!          variance of a given sample.
    !! <h2>Examples</h2>
    !! @code{.f90}
    !! use benchmark_statistics
    !! 
    !! type(stats) :: s
    !! integer     :: i
    !! real(r8)    :: array(10)
    !!
    !! do i = 1, 10
    !!      array(i) = real(i, r8)
    !! end do
    !! 
    !! call s%compute(array)
    !! write(*, '(A)') str(s%mean, '(f12.3)') // ' ms'//' +/- '//str(s%stddev, '(f12.3)') // ' ms'
    !! @endcode
    !! <h2>Remarks</h2>
    !! The statistics computed at the moment are limited to mean and standard 
    !! deviations. The implementation is quite naive and serve only the purpose
    !! of the current library. 
    !! @par
    !! <h2>Constructors</h2>
    !! Initializes a new instance of the @ref stats class
    !! <h3>stats(integer, real(r8), real(r8), real(r8))</h3>
    !! @verbatim type(stats) :: s @endverbatim
    !! 
    !! 
    !! @b Examples
    !! ```fortran
    !! use benchmark_statistics
    !! 
    !! type(stats) :: s
    !! ```
    !! @b Remarks
    type stats
    !> @name Variables
        !! @{
        private
        integer             :: n !< Sample size
        real(r8), public    :: mean     = 0.0_r8 !< Mean value of the sample
        real(r8), public    :: stddev   = 0.0_r8 !< Standard deviation of the sample
        real(r8), public    :: variance = 0.0_r8 !< Variance of the sample
        !> @}
    contains
        procedure, pass(this) :: compute => stats_compute
    end type

    contains
    
    !> @brief Compute mean, standard deviation and 
    !! variance of a given vector.
    !! @param[inout] this bound argument
    !! @param[in] y sample array
    !!
    !! @b Remarks @n
    !! 
    !! @note At the moment it only works for real(r8) arrays
    !!
    !! @b Examples @n
    !! 
    !! @code{.f90}
    !! type(stats) :: s
    !! integer :: i
    !! real(r8) :: array(10)
    !!
    !! call random_number(array)
    !! call s%compute(array)
    !! @endcode
    subroutine stats_compute(this, y)
        class(stats), intent(inout) :: this
        real(r8), intent(in) :: y(:)

        this%n = size(y)
        this%mean = sum(y)/real(this%n, r8)
        this%variance = sum((y - this%mean)**2)/real(this%n-1, r8)
        this%stddev = sqrt(this%variance)
    end subroutine
    
end module
!> @}