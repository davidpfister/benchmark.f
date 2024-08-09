!> @ingroup group_benchmark
!> @defgroup group_output output
!> @brief Set output unit
!> @{
module benchmark_output_unit
    use, intrinsic :: iso_fortran_env, only: stdout => output_unit
    
    implicit none
    
    private
       
    integer, public :: output_unit = stdout
    
    type, public :: iproperty
    contains
    private
        procedure, pass(lhs) :: iprop_equals_int
        procedure, pass(rhs) :: int_equals_iprop
        generic, public :: assignment(=) => iprop_equals_int, &
                                            int_equals_iprop
    end type
    
    contains
    
    subroutine iprop_equals_int(lhs, rhs)
       class(iproperty), intent(inout) :: lhs
       integer, intent(in) :: rhs
       
       output_unit = rhs
    end subroutine
    
    subroutine int_equals_iprop(lhs, rhs)
       integer, intent(inout) :: lhs
       class(iproperty), intent(in) :: rhs
       
       lhs = output_unit
    end subroutine
    
end module
!> @}