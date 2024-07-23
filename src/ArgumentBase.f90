!> @ingroup group_all group_method
!> @file
module benchmark_method_argument_base
    
    implicit none; private
    
    !> @brief Provides a base class for the method arguments
    !> @verbatim type, public :: arg_base @endverbatim
    !> @subsection Examples
    !! test
    !> @subsection Remarks
    !! test
    type, public :: arg_base
        character(:), allocatable :: display    !< String characterizing the argument
        class(*), allocatable, public :: value  !< Unlimited polymorphic value of the argument
    end type
    
end module