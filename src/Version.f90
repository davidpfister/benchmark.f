!> @defgroup group_version benchmark_version
!! @brief Represents the version number of the library.
!! @par
!! <h2>Examples</h2>
!! The following example uses the @link benchmark_version::version version @endlink
!! variable to print the version number of the current library. At 
!! compile time, the version number is stored in the module variable 
!! @link benchmark_version::version version @endlink. 
!! @n
!! @snippet snippet.f90 version
!! <h2>Remarks</h2>
!! The version number follows the <a href="https://semver.org/">semver</a> convention
!! with MAJOR.MINOR.PATCH.
!! @{
module benchmark_version
    implicit none
    
    public 
    
    !> @name Variables
    !! @{
    character(*), parameter :: version = '1.0.0' !< @brief The current version number, using semver notation
    !> @}
end module
!> @}