module benchmark_statistics
    use iso_fortran_env, only: i1 => int8, &
                               i2 => int16, &
                               i4 => int32, &
                               i8 => int64, &
                               r4 => real32, &
                               r8 => real64, &
                               r16 => real128
    
    implicit none
    
    public :: stats
    
    type stats
        integer :: n = 0
        integer(i4) :: sum = 0_r8
        integer(i4) :: sum2 = 0_r8
        double precision :: mean
        double precision :: stddev
        double precision :: variance
    contains
        procedure, pass(this) :: reset => stats_reset
        procedure, pass(this) :: update => stats_update
        procedure, pass(this) :: finalize => stats_finalize
    end type
    
    private
    
    contains
    
    subroutine stats_reset(this)
        class(stats), intent(inout) :: this

        this%n = 0
        this%mean = 0_r8
        this%variance = 0_r8
        this%stddev = 1_r8
        this%sum = 0_r8
        this%sum2 = 0_r8
    end subroutine

    subroutine stats_update(this, start, finish)
        class(stats), intent(inout) :: this
        real(r8), intent(in) :: start
        real(r8), intent(in) :: finish

        this%sum = this%sum + (finish - start)
        this%sum2 = this%sum2 + (finish - start)**2
    end subroutine

    subroutine stats_finalize(this, count)
        class(stats), intent(inout) :: this
        integer, intent(in) :: count

        this%mean = this%sum / dble(this%n) / dble(count)
        if (this%n > 1) this%variance = (this%sum2 - this%sum * this%sum / dble(this%n)) / dble(this%n - 1) / dble(count)**2
        this%stddev = sqrt(abs(this%variance))
    end subroutine
    
end module