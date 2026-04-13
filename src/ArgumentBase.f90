!> @file
!! @defgroup group_arg_base Argument_Base
!! @link benchmark_argument_base::arg_base Base Argument @endlink module
module benchmark_argument_base

    implicit none; private

    !> Represents an abstract type
    !! of the method argument. Method arguments
    !! are simple types containing a name for display and
    !! a polymorphic value.
    !! @verbatim type, public :: arg_base @endverbatim
    !! <h2>Examples</h2>
    !! The following example demonstrates how to use @ref arg_base to create
    !! the concrete type @link benchmark_argument::arg arg @endlink
    !! @snippet snippet.f90 arg_base_derivation
    !! <h2>Remarks</h2>
    !! @ref arg_base is an abstract base class of @ref group_arg "arg",
    !! which contains the type bound procedures.
    !!
    !! @ingroup group_arg_base
    !! @b Remarks
    type, abstract, public :: arg_base
        !> @brief String characterizing the argument. @n If not specified, the numeric value is stringified. For unknown types, the symbol '?' is used
        character(:), allocatable   :: display
        !> @brief Unlimited polymorphic value of the argument. @n This property is used to store any value of any type.
        class(*), allocatable       :: value
    end type

end module
