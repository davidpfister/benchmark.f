!> @ingroup group_benchmark
!> @defgroup group_arg_base arg_base
!> @par 
!> <h2>Examples</h2>
!> @code{.f90}
!> type, extends(arg_base), public :: arg
!>  private
!>  contains
!>  procedure, pass(lhs), private   :: any_assign_argument
!>  generic :: assignment(=) => any_assign_argument
!>  procedure, pass(this), public :: to_string
!> end type
!> @endcode
!> @par 
!> <h2>Remarks</h2>
!> arg_base is an abstract base class of @ref group_arg "arg", 
!> which also contains bound procedures.
!> @note This type contains a ::value component
!> @{
module benchmark_method_argument_base
    
    implicit none; private

    !> @class arg_base
    !> @details Represents an abstract type
    !>          of the method argument
    type, abstract, public :: arg_base
        character(:), allocatable :: display !< String characterizing the argument
        class(*), allocatable, public :: value !< Unlimited polymorphic value of the argument
    end type
    
end module
!> @}