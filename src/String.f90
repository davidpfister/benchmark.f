module benchmark_string
    use benchmark_kinds
    
    implicit none 
    
    public :: str
    
    type, public :: string
        character(:), allocatable :: chars
    contains
        procedure, pass(lhs), private   :: character_assign_string
        procedure, pass(rhs), private   :: string_assign_character
        generic, public :: assignment(=) => character_assign_string, &
                                            string_assign_character
    end type
        
    contains
    
    subroutine character_assign_string(lhs, rhs)
        class(string), intent(inout)   :: lhs
        character(*), intent(in)       :: rhs
        
        if (allocated(lhs%chars)) deallocate(lhs%chars)
        allocate(lhs%chars, source = rhs)
    end subroutine
    
    subroutine string_assign_character(lhs, rhs)
        character(*), allocatable, intent(inout) :: lhs
        class(string), intent(in)                :: rhs
        
        lhs = rhs%chars
    end subroutine
    
    pure function str(value, fmt) result(chars)
        class(*), intent(in)        :: value
        character(*), intent(in), optional :: fmt
        character(:), allocatable   :: chars
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
            fmt_ = '(g0.2)'; if (present(fmt)) fmt_ = fmt
            write(chars, fmt_) x
        type is (real(r8))
            fmt_ = '(g0.2)'; if (present(fmt)) fmt_ = fmt
            write(chars, fmt_) x
        type is (real(r16))
            fmt_ = '(g0.2)'; if (present(fmt)) fmt_ = fmt
            write(chars, fmt_) x
        type is (logical)
            fmt_ = '(l1)'; if (present(fmt)) fmt_ = fmt
            write(chars, fmt_) x
        type is (character(*))
            chars = x
        class is (string)
            chars = x%chars
        class default
            chars='?'
        end select
        chars = trim(adjustl(chars))
    end function
end module