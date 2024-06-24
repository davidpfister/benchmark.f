module benchmark_method_argument
    implicit none
    
    type, public :: argument
        class(*), allocatable           :: value
    contains
        procedure, pass(lhs), private   :: any_assign_argument
        generic, public :: assignment(=) => any_assign_argument
    end type
    
    interface assignment(=)
        module procedure :: argument_assign_any
    end interface
    
    contains
    
    subroutine any_assign_argument(lhs, rhs)
        class(argument), intent(inout)   :: lhs
        class(*), intent(in)            :: rhs
        
        if (allocated(lhs%value)) deallocate(lhs%value)
        select type (rhs)
        type is (argument)
            allocate(lhs%value, source = rhs%value)
        class default
            allocate(lhs%value, source = rhs)
        end select
    end subroutine
    
    subroutine argument_assign_any(lhs, rhs)
        class(*), intent(inout)     :: lhs
        type(argument), intent(in)  :: rhs
        
        select type (lhs)
        type is (argument)
            allocate(lhs%value, source = rhs%value)
        class default
            lhs = rhs%value
        end select
    end subroutine
end module