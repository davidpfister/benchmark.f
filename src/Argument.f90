!> @ingroup group_all group_method
!> @file
module benchmark_method_argument
    use, intrinsic :: iso_c_binding
    use benchmark_kinds
    use benchmark_method_argument_base
    use benchmark_string
    
    implicit none; private
    
    !> @class arg_base
    !> @brief Provides an extended class for the method arguments
    type, extends(arg_base), public :: arg
        private
    contains
        procedure, pass(lhs), private   :: any_assign_argument
        generic :: assignment(=) => any_assign_argument
        procedure, pass(this), public :: to_string
    end type
    
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