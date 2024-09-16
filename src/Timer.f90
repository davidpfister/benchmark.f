!> @defgroup group_timer benchmark_timer
!! @brief Returns the actual time in milliseconds.
!! @par
!! <h2>Examples</h2>
!! The following example uses the @ref benchmark_timer::clock subroutine to get the 
!! actual time in milliseconds. 
!! @n
!! @snippet snippet.f90 timer
!! @par
!! <h2>Remarks</h2>
!! The system clock that is used can be modified. Three
!! options are available at the moment: CPUTIME, DATETIME, SYSTEMTIME. 
!! They corresponds to the underlying functions `cpu_time`, `date_and_time` and 
!! `system_clock`, respectively.
!! @{
module benchmark_timer
    use benchmark_kinds
    
    implicit none

    !> @name Enums
    !! @{
    !! <h3>CLOCK_ENUM</h3>
    !! @cond
    enum, bind(c)
    !! @endcond    
        enumerator :: CPUTIME = 0 !< Value related to the function `cpu_time`
        enumerator :: DATETIME = 1 !< Value related to the function `date_and_time`
        enumerator :: SYSTEMTIME = 2 !< Value related to the function `system_clock`
    end enum
    !> @}

    integer, parameter, public :: CLOCK_ENUM = kind(CPUTIME)

    private 
    
    public :: clock
    
    contains
    
    !> @brief Returns the actual time in milliseconds.
    !! @param[inout] r The parameter containing the clock time in milliseconds
    !! @param[in] option (optional) Clock type. 
    !!
    !! @b Remarks
    !!
    !! Possible values are 
    !! - CPUTIME
    !! - DATETIME
    !! - SYSTEMTIME
    subroutine clock(r, option)
        real(r8), intent(inout)                     :: r
        integer(CLOCK_ENUM), intent(in), optional   :: option
        !private
        integer(CLOCK_ENUM) :: dft_option
        
        dft_option = SYSTEMTIME
        if (present(option)) dft_option = option
#ifdef _OPENMP
        if (with_openmp()) then
            r = omp_get_wtime()
        else
            call get_time(r, dft_option)
        end if
#else
        call get_time(r, dft_option)
#endif
    end subroutine

    subroutine get_time(ctime, option)! in milliseconds
        real(r8), intent(out)           :: ctime
        integer(CLOCK_ENUM), intent(in) :: option

        select case (option)
        case (CPUTIME)
            call cpu_time(ctime)
            ctime = ctime * 1000_r8
        case (DATETIME) 
            block
                integer(i8) :: dt(8)

                call date_and_time(values=dt)
                ctime = (dt(5) * 3600_r8 + dt(6) * 60_r8 + dt(7)) * 1000_r8 + dt(8) * 1_r8
            end block
        case (SYSTEMTIME)
            block
                integer(i8) :: clock_max, clock_rate, clock_reading

                call system_clock(clock_reading, clock_rate, clock_max)
                ctime = 1000_r8 * real(clock_reading, r8) / real(clock_rate, r8)
            end block
        end select
    end subroutine
end module
!> @}