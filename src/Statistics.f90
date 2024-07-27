
!> @ingroup group_all
!> @author davidpfister
module benchmark_statistics
    use benchmark_kinds
    
    implicit none; private
    
    public :: stats
    
    type stats
        integer     :: n
        real(r8)    :: mean     = 0.0_r8
        real(r8)    :: stddev   = 0.0_r8
        real(r8)    :: variance = 0.0_r8
    contains
        procedure, pass(this) :: compute => stats_compute
    end type

    contains
    
    subroutine stats_compute(this, y)
        class(stats), intent(inout) :: this
        real(r8), intent(in) :: y(:)

        this%n = size(y)
        this%mean = sum(y)/real(this%n, r8)
        this%variance = sum((y - this%mean)**2)/real(this%n-1, r8)
        this%stddev = sqrt(this%variance)
    end subroutine
    
end module