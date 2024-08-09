!> @ingroup group_benchmark
!> @defgroup group_arg arg
!> @par 
!> <h2>Examples</h2>
!> @code{.f90}
!> use benchmark_method_argument
!> use benchmark_kinds
!>       
!> type(arg) :: a
!>        
!> a = arg(12.25_r8, 'arg1')
!> !!a%to_string(), 'arg1')
!>
!> select type(x => a%value)
!> type is (real(r8))
!>    !success
!> class default
!>    stop
!> end select
!> @endcode
!> @par 
!> <h2>Remarks</h2>
!> arg is the extended class of @ref group_arg "arg", 
!> which also contains bound procedures.
!> @note This type contains a ::value component
!> @{
module benchmark_method_argument
    use, intrinsic :: iso_c_binding
    use benchmark_kinds
    use benchmark_method
    use benchmark_method_argument_base
    use benchmark_string
    
    implicit none; private
    
    !> @class arg
    !> @details Represents the extended type for method arguments
    type, extends(arg_base), public :: arg
        private
    contains
    !> @defgroup group_arg_methods Methods
    !> @{
    !> @brief Type bound procedures
        procedure, pass(lhs), private   :: any_assign_argument
        generic :: assignment(=) => any_assign_argument
        procedure, pass(this), public :: to_string
    !> @}
    end type

    !> @defgroup group_arg_constructor Constructors
    !> @{
    !> @details Simple constractor
    !> @verbatim type(arg) function arg(class(*)) @endverbatim
    !> @param[in] value
    !> @param[in] display
    !> @par 
    !> <h2>Examples</h2>
    !> @code{.f90}
    !> a = arg(5.0d0, 'arg1')
    !> @endcode
    !> @par 
    !> <h2>Remarks</h2>
    interface arg
        !> @cond
        module procedure :: arg_new, &
                            arg_new_from_chars, &
                            arg_new_from_string
        !> @endcond
    end interface
    !> @}

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

    !> @brief Overloading of the assigment procedure.
    !> @param[inout] lhs class(arg)
    !> @param[in] rhs class(*), unlimited polymorphic
    !> @par Remarks
    !> The field `value` is an unlimited polymorphic component
    !> It accepts any value of any type. 
    !> @par Examples
    !> The following example uses the assigment(\=) the set the 
    !> value of the argument
    !> @code{.f90}
    !> type(arg) :: a
    !> a = 5.0_r8
    !> @endcode
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
    
    !> @brief Returns a string that represents the current object.
    !> @par Returns 
    !> character(:), allocatable @n@n A string that represents the current object.
    !> @par Remarks
    !> @par Examples
    !> @code{.f90}
    !> character(:), allocatable :: s
    !> type(arg) :: a
    !> a = arg(5.0_r8)
    !> s = a%to_string()
    !> @endcode
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
!> @}