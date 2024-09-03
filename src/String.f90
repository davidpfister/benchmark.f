!> @defgroup group_string benchmark_string
!! @brief String module
module benchmark_string
    use benchmark_kinds
    use benchmark_argument_base
    
    implicit none; private
    
    public :: str
    
    !> @class string
    !! @ingroup group_string
    !! @brief Represents text as a sequence of ASCII code units.
    !!          The derived type wraps an allocatable character array.
    !! <h2>Examples</h2>
    !! ```fortran
    !! type(string) :: s
    !! s = 'foo'
    !! ```
    !! <h2>Remarks</h2>
    !! @par
    !! The string implementation proposed here is kept at the bare 
    !! minimum of what is required by the library. There are many 
    !! other implementations that can be found.
    !! <h2>Constructors</h2>
    !! Initializes a new instance of the @ref string class
    !! <h3>string(character(:))</h3>
    !! @verbatim type(string) function string(character(:) chars) @endverbatim
    !! 
    !! @param[in] chars 
    !! 
    !! @b Examples
    !! ```fortran
    !! type(string) :: s
    !! s = string('foo')
    !! ```
    type, public :: string
        character(:), allocatable :: chars
    contains
        procedure, pass(this), public   :: len => string_len
        procedure, pass(lhs), private   :: character_assign_string
        procedure, pass(rhs), private   :: string_assign_character
        generic, public :: assignment(=) => character_assign_string, &
                                            string_assign_character
    end type
    
    contains
    
    !> @brief Assignment overloading. Assign a character array
    !!          to a string.
    !! @param[inout] lhs string
    !! @param[in] rhs character(*)
    !! 
    !! @b Examples
    !! ```fortran
    !! type(string) :: s
    !! 
    !! s = 'foo'
    !! ```
    !! @b Remarks
    subroutine character_assign_string(lhs, rhs)
        class(string), intent(inout)   :: lhs
        character(*), intent(in)       :: rhs
        
        if (allocated(lhs%chars)) deallocate(lhs%chars)
        allocate(lhs%chars, source = rhs)
    end subroutine
    
    !> @brief Assignment overloading. Assign a string to a 
    !!          character array
    !! @param[inout] lhs character(*), allocatable
    !! @param[in] rhs string
    !! 
    !! @b Examples
    !! ```fortran
    !! type(string) :: s
    !! character(:), allocatable :: c
    !! 
    !! s = 'foo'
    !! c = s
    !! ! The value of c is now 'foo'
    !! ```
    !! @b Remarks
    subroutine string_assign_character(lhs, rhs)
        character(*), allocatable, intent(inout) :: lhs
        class(string), intent(in)                :: rhs
        
        lhs = rhs%chars
    end subroutine
    
    !> @ingroup group_string
    !> @brief Length of the string entity
    !! @param[in] this string  
    !! 
    !! @b Examples
    !! ```fortran
    !! type(string) :: s
    !! integer :: l
    !! 
    !! s = string('foo')
    !! l = s%len()
    !! ! The value of l is 3
    !! ```
    !! @returns An integer cooresponding to the length of the string
    pure integer function string_len(this) result(res)
         class(string), intent(in) :: this
         res = len(this%chars)
    end function
    
    !> @ingroup group_string
    !> @brief Function to stringify anything
    !! 
    !! @param[in] value The value of any intrinsic type
    !!                  to be stringified
    !! @param[in] fmt (optional) The format value
    !!
    !! @b Examples
    !! ```fortran
    !! pure function to_string(this) result(s)
    !!      class(arg), intent(in) :: this
    !!      character(:), allocatable :: s
    !!
    !!      if (allocated(this%display)) then
    !!          s = this%display
    !!       else
    !!          s = str(this%value)
    !!      end if
    !! end function
    !! ```
    !! @returns A character array corresponding to the 
    !!          character representation of the value 
    !!          argument.
    pure recursive function str(value, fmt) result(chars)
        class(*), intent(in)               :: value
        character(*), intent(in), optional :: fmt
        character(:), allocatable          :: chars
        !private
        character(:), allocatable :: fmt_
        
        allocate(character(24) :: chars)
        select type(x => value)
        type is (integer(i1))
            fmt_ = '(i0)'; if (present(fmt)) fmt_ = fmt
            write(chars, fmt_) x
        type is (integer(i2))
            fmt_ = '(i0)'; if (present(fmt)) fmt_ = fmt
            write(chars, fmt_) x
        type is (integer(i4))
            fmt_ = '(i0)'; if (present(fmt)) fmt_ = fmt
            write(chars, fmt_) x
        type is (integer(i8))
            fmt_ = '(i0)'; if (present(fmt)) fmt_ = fmt
            write(chars, fmt_) x
        type is (real(r4))
            fmt_ = '(1pg0.3)'; if (present(fmt)) fmt_ = fmt
            write(chars, fmt_) x
        type is (real(r8))
            fmt_ = '(1pg0.3)'; if (present(fmt)) fmt_ = fmt
            write(chars, fmt_) x
        type is (real(r16))
            fmt_ = '(1pg0.3)'; if (present(fmt)) fmt_ = fmt
            write(chars, fmt_) x
        type is (logical)
            fmt_ = '(l1)'; if (present(fmt)) fmt_ = fmt
            write(chars, fmt_) x
        type is (character(*))
            chars = x
        class is (arg_base)
            chars = str(x%value)
        class is (string)
            chars = x%chars
        class default
            chars='?'
        end select
        chars = trim(adjustl(chars))
    end function
end module