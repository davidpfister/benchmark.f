module utility
    use benchmark_string
    
    implicit none; private
    
    public :: upper1, &
              upper2, &
              upper3, &
              string_data, &
              upper_caller
    
    integer :: i
    
    interface
        pure function upper_x(str) result(res)
            character(*), intent(in)   :: str 
            character(len(str)) :: res
        end function
    end interface
    
    character(1000) :: string_data(1:3)
    data(string_data(i), i=1, 3)/ &
    'abcdefghijklmnopqrstuvwxyz', &
    'ABCDEFGHIJKLMNOPQRSTUVWXYZ', &
    'Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum'/
    
    contains
    
    subroutine upper_caller(f, a)
        procedure(upper_x) :: f
        type(string), intent(in) :: a
        
        block
            character(len(a)) :: res
                
            res = f(a%chars)
        end block
    end subroutine
    
    pure function upper1(str) result(res)
        character(*), intent(in)   :: str 
        character(len(str)) :: res
        !private
        integer :: i, ade_char
        integer, parameter :: ade_a = iachar('a'), ade_z = iachar('z')
        integer, parameter :: diff = iachar('A') - iachar('a')

        do concurrent(i=1:len(str))
            ade_char = iachar(str(i:i))
            if (ade_char >= ade_a .and. ade_char <= ade_z) ade_char = ade_char + diff
            res(i:i) = achar(ade_char)
        end do
        if (len(str) == 0) res = str

    end function

    pure function upper2(str) result(res)
        character(*), intent(in)   :: str
        character(len(str)) :: res
        !private
        integer                       :: i
        integer, parameter :: diff = iachar('A') - iachar('a')

        do concurrent(i=1:len(str))
            select case (str(i:i))
            case ('a':'z'); res(i:i) = achar(iachar(str(i:i)) + diff)
            case default; res(i:i) = str(i:i)
            end select
        end do
        if (len(str) == 0) res = str

    end function

    pure function upper3(str) result(res)
        character(*), intent(in)   :: str 
        character(len_trim(str)) :: res
        !private
        integer :: i, ch
        integer, parameter :: diff = iachar('A') - iachar('a')
        integer, parameter :: ade_a = iachar('a'), ade_z = iachar('z')
    
        do concurrent(i=1:len(str))
            ch = iachar(str(i:i))
            select case (ch)
            case (ade_a:ade_z); res(i:i) = achar(ch + diff)
            case default; res(i:i) = str(i:i)
            end select
        end do
        if (len(str) == 0) res = str

    end function
end module