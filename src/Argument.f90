module benchmark_method_argument
    use benchmark_kinds
    use benchmark_string
    
    implicit none
    
    type, public :: arg
        character(:), allocatable :: display
        class(*), allocatable     :: value
    contains
        procedure, pass(lhs), private   :: any_assign_argument
        generic, public :: assignment(=) => any_assign_argument
    end type
    
    interface assignment(=)
        module procedure :: argument_assign_any
    end interface
    
    interface arg
        module procedure :: arg_new, &
                            arg_new_from_chars, &
                            arg_new_from_string
    end interface
    
    contains
    
    type(arg) function arg_new(value) result(a)
        class(*), intent(in)                :: value
        a%display = str(value)
        
        allocate(a%value, source = value)
    end function
    
    type(arg) function arg_new_from_chars(value, display) result(a)
        class(*), intent(in)                :: value
        character(*), intent(in)            :: display
        
        if (len_trim(display) > 0) then
            a%display = trim(adjustl(display))
        else
            a%display = str(value)
        end if
        
        allocate(a%value, source = value)
    end function
    
    type(arg) function arg_new_from_string(value, display) result(a)
        class(*), intent(in)                :: value
        type(string), intent(in)            :: display
        
        if (len_trim(display%chars) > 0) then
            a%display = trim(adjustl(display%chars))
        else
            a%display = str(value)
        end if
        
        allocate(a%value, source = value)
    end function
    
    subroutine any_assign_argument(lhs, rhs)
        class(arg), intent(inout)   :: lhs
        class(*), intent(in)        :: rhs
        
        if (allocated(lhs%value)) deallocate(lhs%value)
        select type (rhs)
        type is (arg)
            lhs%display = rhs%display
            if (allocated(lhs%value)) deallocate(lhs%value) 
            allocate(lhs%value, source = rhs%value)
        class default
            lhs%display = str(rhs)
            if (allocated(lhs%display)) deallocate(lhs%display) 
            allocate(lhs%value, source = rhs)
        end select
    end subroutine
    
    subroutine argument_assign_any(lhs, rhs)
#ifdef __GFORTRAN__
       class(*), allocatable, intent(inout) :: lhs
#else
       class(*), intent(inout) :: lhs
#endif
        type(arg), intent(in)   :: rhs
        
        select type (lhs)
        type is (arg)
            lhs%display = rhs%display
            if (allocated(lhs%value)) deallocate(lhs%value) 
            allocate(lhs%value, source = rhs%value)
        class default
#ifdef __GFORTRAN__
            if (allocated(lhs)) deallocate(lhs)
            allocate(lhs, source = rhs%value)
#else
            lhs = rhs%value
#endif
        end select
    end subroutine

end module