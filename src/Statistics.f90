module benchmark_statistics
    use benchmark_kinds
    
    implicit none
    
    public :: stats
    
    type stats
        integer     :: n
        real(r8)    :: mean     = 0.0_r8
        real(r8)    :: stddev   = 0.0_r8
        real(r8)    :: variance = 0.0_r8
    contains
        procedure, pass(this) :: compute => stats_compute
    end type
    
    private
    
    contains
    
    subroutine stats_compute(this, y)
        class(stats), intent(inout) :: this
        real(r8), intent(in) :: y(:)

        this%n = size(y)
        this%mean = sum(y)/this%n
        this%variance = sum(y**2)/this%n - this%mean**2
        this%stddev = sqrt(this%variance)
    end subroutine
    
end module