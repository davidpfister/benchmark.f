module benchmark_method_argument
    use benchmark_kinds
    use benchmark_string
    
    implicit none
    
    type, public :: arg
        private
        character(:), allocatable :: display
        class(*), allocatable, public :: value
    contains
        procedure, pass(lhs), private   :: any_assign_argument
        generic :: assignment(=) => any_assign_argument
        procedure, pass(this), public :: to_string
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
        class(*), intent(in)        :: value
        
        allocate(a%value, source = value)
    end function
    
    type(arg) function arg_new_from_chars(value, display) result(a)
        class(*), intent(in)        :: value
        character(*), intent(in)    :: display
        
        if (len_trim(display) > 0) then
            a%display = trim(adjustl(display))
        end if
        
        allocate(a%value, source = value)
    end function
    
    type(arg) function arg_new_from_string(value, display) result(a)
        class(*), intent(in)        :: value
        class(string), intent(in)   :: display
        
        if (len_trim(display%chars) > 0) then
            a%display = trim(adjustl(display%chars))
        end if
        
        allocate(a%value, source = value)
    end function
    
    subroutine any_assign_argument(lhs, rhs)
        class(arg), intent(inout)   :: lhs
        class(*), intent(in)        :: rhs
        
        if (allocated(lhs%value)) deallocate(lhs%value)
        select type (rhs)
        type is (arg)
            if (allocated(lhs%display)) deallocate(lhs%display)
            if (allocated(rhs%display)) allocate(lhs%display, source = rhs%display)
            allocate(lhs%value, source = rhs%value)
        class default
            if (allocated(lhs%display)) deallocate(lhs%display)
            allocate(lhs%value, source = rhs)
        end select
    end subroutine
    
    subroutine argument_assign_any(lhs, rhs)
        use iso_c_binding
        class(*), intent(inout)       :: lhs
        type(arg), intent(in), target :: rhs
        !private
        interface
            subroutine memcpy(dest, src, n) bind(c, name='memcpy')
                import
                integer(c_intptr_t), intent(in), value :: dest
                integer(c_intptr_t), intent(in), value :: src
                integer(c_size_t), value :: n
            end subroutine
        end interface
      
        select type (lhs)
        type is (arg)
            if (allocated(lhs%display)) deallocate(lhs%display)
            if (allocated(rhs%display)) allocate(lhs%display, source = rhs%display)
            if (allocated(lhs%value)) deallocate(lhs%value) 
            allocate(lhs%value, source = rhs%value)
        class default
            if (same_type_as(lhs, rhs%value)) then
                call memcpy(loc(lhs), loc(rhs%value), storage_size(lhs, kind=c_size_t)/8_c_size_t)
            else
                stop 'Not supported assignment'
            end if
        end select
    end subroutine

    pure function to_string(this) result(s)
        class(arg), intent(in) :: this
        character(:), allocatable :: s

        if (allocated(this%display)) then
            s = this%display
        else
            s = str(this%value)
        end if
    end function
end module