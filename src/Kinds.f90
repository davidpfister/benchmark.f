!> @defgroup group_kinds benchmark_kinds
!! @brief Provides constants for defining standard integer and real values.
!! @par
!! <h2>Examples</h2>
!! The following example demonstrates some of the constants found in the 
!! @link benchmark_kinds benchmark_kinds @endlink module.
!! @n
!! @code{.f90}
!! real(r8) a      :: 0_r8
!! integer(i4) i  :: 1_i4 
!! @endcode
!! @par
!! <h2>Remarks</h2>
!! The constants are coming from the intrinsic module `iso_fortran_env`. 
!! If the compiler does not provide such intrinsic module then the 
!! compilation will fail. 
!! @{
module benchmark_kinds
    use, intrinsic :: iso_fortran_env, only: int8, &
                               int16, &
                               int32, &
                               int64, &
                               real32, &
                               real64, &
                               real128
    
    implicit none; private
    
    !> @name Parameters
    !! @{
    integer, parameter, public :: i1  = int8 !< @brief 8-bit integer constant deriving from the int8 intrinsic constant of the `iso_fortran_env` module
    integer, parameter, public :: i2  = int16!< @brief 16-bit integer constant deriving from the int16 intrinsic constant of the `iso_fortran_env` module
    integer, parameter, public :: i4  = int32 !< @brief 32-bit integer constant deriving from the int32 intrinsic constant of the `iso_fortran_env` module
    integer, parameter, public :: i8  = int64 !< @brief 64-bit integer constant deriving from the int64 intrinsic constant of the `iso_fortran_env` module
    integer, parameter, public :: r4  = real32 !< @brief single precision, 32-bit real constant deriving from the real32 intrinsic constant of the `iso_fortran_env` module
    integer, parameter, public :: r8  = real64 !< @brief double precision, 64-bit real constant deriving from the real64 intrinsic constant of the `iso_fortran_env` module
    integer, parameter, public :: r16 = real128 !< @brief quadruple precision, 128-bit real constant deriving from the real128 intrinsic constant of the `iso_fortran_env` module
    !> @}
end module
!> @}