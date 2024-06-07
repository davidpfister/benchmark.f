module benchmark_timer
    use benchmark_kinds
    
    implicit none
    
    enum, bind(c)
        enumerator :: CPUTIME = 0
        enumerator :: DATETIME = 1
        enumerator :: SYSTEMTIME = 2
    end enum
        
    private 
    
    public :: clock
    
    contains
    
    subroutine clock(r)
        real(r8), intent(inout) :: r
#ifdef _OPENMP
        if (with_openmp()) then
            r = omp_get_wtime()
        else
            call get_time(r)
        end if
#else
        call get_time(r)
#endif
    end subroutine

    subroutine get_time(ctime, option)
        real(r8), intent(out) :: ctime
        integer, intent(in), optional :: option
        !private
        integer :: dft_option

        dft_option = DATETIME

        if (present(option)) dft_option = option
        select case (dft_option)
        case (CPUTIME)
            call cpu_time(ctime)
            ctime = ctime * 1000_r8
        case (DATETIME)
            block
                integer(i4) :: dt(8)

                call date_and_time(values=dt)
                ctime = (dt(5) * 3600_r8 + dt(6) * 60_r8 + dt(7)) * 1000_r8 + dt(8) * 1_r8
            end block
        case (SYSTEMTIME)
            block
                integer(i4) :: clock_max, clock_rate, clock_reading

                call system_clock(clock_reading, clock_rate, clock_max)
                ctime = 1000_r8 * real(clock_reading, r8) / real(clock_rate, r8)
            end block
        end select
    end subroutine
end module
    