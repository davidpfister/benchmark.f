!> @defgroup group_arg benchmark_argument
!! @brief @link benchmark_argument::arg Argument @endlink module
module benchmark_argument
    use, intrinsic :: iso_c_binding
    use benchmark_kinds
    use benchmark_string
    use benchmark_argument_base
    
    implicit none; private
    
    !> @class arg
    !! @ingroup group_arg
    !! @brief Represents the extended type for method @link benchmark_argument_base::arg_base argument @endlink
    !! <h2>Examples</h2>
    !! @snippet snippet.f90 arg_constructor
    !! <h2>Remarks</h2>
    !! arg is the extended class of @ref arg, 
    !! which also contains bound procedures.
    !! @par
    !! <h2>Constructors</h2>
    !! Initializes a new instance of the @ref arg class
    !! <h3>arg(class(*))</h3>
    !! @verbatim type(arg) function arg(class(*) val) @endverbatim
    !! 
    !! @param[in] val 
    !! 
    !! @b Examples
    !! ```fortran
    !! a = arg(5.0d0)
    !! ```
    !! @b Remarks
    !! <h3>arg(class(*), character(*))</h3>
    !! @verbatim type(arg) function arg(class(*) val, character(*) display) @endverbatim
    !! 
    !! @param[in] val The value to be stored in the @ref arg type for later reuse.
    !!                It can be of any type, inrinsic or not.
    !! @param[in] display A charater array that describes the argument. 
    !!                    The content of variable is intended to be understood by humans. 
    !! 
    !! @b Examples
    !! ```fortran
    !! a = arg(5.0d0, 'arg1')
    !! ```
    !! @b Remarks
    !! <h3>arg(class(*), type(string))</h3>
    !! @verbatim type(arg) function arg(class(*) val, type(string) display) @endverbatim
    !! 
    !! @param[in] val The value to be stored in the @ref arg type for later reuse.
    !!                It can be of any type, inrinsic or not.
    !! @param[in] display A string that describes the argument. 
    !!                    The content of variable is intended to be understood by humans. 
    !! 
    !! 
    !! @b Examples
    !! ```fortran
    !! a = arg(5.0d0, string('arg1'))
    !! ```
    !! @b Remarks
    type, extends(arg_base), public :: arg
        private
    contains
        procedure, pass(lhs), private   :: any_assign_argument
        generic :: assignment(=) => any_assign_argument
        procedure, pass(this), public :: to_string
    end type

    interface arg
        !> @cond
        module procedure :: arg_new, &
                            arg_new_from_chars, &
                            arg_new_from_string
        !> @endcond
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

    !> @brief Overloading of the assigment procedure.
    !! @param[inout] lhs class(arg)
    !! @param[in] rhs class(*), unlimited polymorphic
    !!
    !! @b Remarks
    !! 
    !! The field `value` is an unlimited polymorphic component
    !! It accepts any value of any type. 
    !! 
    !! @b Examples
    !! 
    !! The following example uses the assigment(\=) the set the 
    !! value of the argument
    !! @code{.f90}
    !! type(arg) :: a
    !! a = 5.0_r8
    !! @endcode
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
    !! @returns character(:), allocatable @n@n A string that represents the current object.
    !! 
    !! @b Examples
    !! 
    !! @code{.f90}
    !! character(:), allocatable :: s
    !! type(arg) :: a
    !!
    !! a = arg(5.0_r8)
    !! s = a%to_string()
    !! @endcode
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