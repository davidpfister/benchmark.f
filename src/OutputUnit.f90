!> @defgroup group_output benchmark_output_unit
!! @brief Output unit module
module benchmark_output_unit
    use, intrinsic :: iso_fortran_env, only: stdout => output_unit
    
    implicit none
    
    private
      
    !> @name Variables
    !! @{
    integer, public :: output_unit = stdout !< @brief Static variable controling the output unit number
    !> @}

    !> @class output
    !! @ingroup group_output
    !! @brief Integer property dedicated to manipulating the output unit
    !! <h2>Examples</h2>
    !! @code{.f90}
    !! type(output) :: prop
    !! integer      :: unit
    !!
    !! unit = prop
    !! if (unit /= stdout) stop
    !! @endcode
    !! <h2>Remarks</h2>
    !! The default value corresponds to the standard error `stdout`.
    !! @par
    !! <h2>Constructors</h2>
    !! Initializes a new instance of the @ref output class
    !! <h3>output()</h3>
    !! @verbatim type(output) :: o @endverbatim
    !! 
    !! @b Examples
    !! @code{.f90}
    !! use benchmark_output_unit
    !!
    !! type(output) :: o
    !! @endcode
    !! @b Remarks
    type, public :: output
    contains
    private
        procedure, pass(lhs), private :: iprop_equals_int
        procedure, pass(rhs), private :: int_equals_iprop
        generic, public :: assignment(=) => iprop_equals_int, &
                                            int_equals_iprop
    end type
    
    contains
    
    !> @brief Overloading of the assigment procedure.
    !! @param[inout] lhs The output variable
    !! @param[in] rhs Integer value
    !! 
    !! @b Examples
    !! 
    !! The following example uses the `assigment(\=)` the set the 
    !! value of the output unit
    !! @code{.f90}
    !! type(output) :: prop
    !! integer :: unit = 15
    !! prop = unit
    !! if (output_unit /= 15) stop
    !! @endcode
    !!
    !! @b Remarks
    !! 
    !! The value of lhs is not used and only the static variable @ref output will be used. 
    subroutine iprop_equals_int(lhs, rhs)
       class(output), intent(inout) :: lhs
       integer, intent(in) :: rhs
       
       output_unit = rhs
    end subroutine
    
    !> @brief Overloading of the assigment procedure.
    !! @param[inout] lhs Integer value
    !! @param[in] rhs The output variable
    !! 
    !! @b Examples
    !! 
    !! The following example uses the `assigment(\=)` the set the 
    !! value of the output unit
    !! @code{.f90}
    !! type(output) :: prop
    !! integer      :: unit
    !!
    !! unit = prop
    !! if (unit /= stdout) stop
    !! @endcode
    !!
    !! @b Remarks
    !! 
    !! The value of rhs is not used and only the static variable @ref output will be used. 
    subroutine int_equals_iprop(lhs, rhs)
       integer, intent(inout) :: lhs
       class(output), intent(in) :: rhs
       
       lhs = output_unit
    end subroutine
    
end module
!> @}