!> @ingroup group_all group_method
!> @file
module benchmark_method_argument
    use benchmark_kinds
    use benchmark_method_argument_base
    use benchmark_string
    
    implicit none
    
    private
    
    public :: assignment(=)
    
    !> @class arg_base
    !> @brief Provides an extended class for the method arguments
    type, extends(arg_base), public :: arg
        private
    contains
        procedure, pass(lhs), private   :: any_assign_argument
        generic :: assignment(=) => any_assign_argument
        procedure, pass(this), public :: to_string
        procedure, pass(rhs), private   :: any_equal_argument
        procedure, pass(lhs), private   :: argument_equal_any
        generic :: operator(==) => any_equal_argument, argument_equal_any
    end type
    
    interface assignment(=)
        module procedure :: argument_assign_any
    end interface
    
    interface arg
        !> @name Public Constructors
        !> @par simple constructor
        !> @verbatim type(arg) function arg(*, char(*)) @endverbatim
        !> @param[in] value
        !> @param[in] display
        !> @n
        !> @b usage:
        !> @code{.f90}
        !> a = arg(5.0d0, 'arg1')
        !> @endcode
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
        class(arg_base), intent(in)         :: rhs
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
            associate(x => rhs%value)
                if (same_type_as(lhs, x)) then
                    call memcpy(loc(lhs), loc(x), storage_size(x, kind=c_size_t)/8_c_size_t)
                else
                    stop 'Not supported assignment'
                end if
            end associate
        end select
    end subroutine

    
    logical function any_equal_argument(lhs, rhs) result(res)
        class(*), intent(in)       :: lhs
        class(arg), intent(in)     :: rhs
        !private
        integer(kind=1), allocatable :: mold(:)
        
        res = all(transfer(lhs, mold) == transfer(rhs%value, mold))
        
    end function
    
    logical function argument_equal_any(lhs, rhs) result(res)
        class(arg), intent(in)     :: lhs
        class(*), intent(in)       :: rhs
        !private
        integer(kind=1), allocatable :: mold(:)
        
        res = all(transfer(lhs%value, mold) == transfer(rhs, mold))
    end function
    
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