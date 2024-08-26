!> @defgroup group_version benchmark_version
!! <h2>Examples</h2>
!! The following example demonstrates ...
!! @n@n
!! The first example shows how to use the ...
!! @n
!! @code{.f90}
!! use benchmark_version
!! @endcode
!! <h2>Remarks</h2>
!! ... 
!! @{
module benchmark_version
    implicit none
    
    public 
    
    !> @param The current version number, using semver notation
    character(*), parameter :: version = "1.0.0"
end module
!> @}