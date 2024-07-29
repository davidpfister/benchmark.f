#include <benchmark.inc>
program simple
    use benchmark_string
    use benchmark_library
    use utility
    
    implicit none
    
    integer :: i
    type(runner) :: br   
    character(1000) :: s(3)
    
    s(1) = 'abcdefghijklmnopqrstuvwxyz'
    s(2) = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    s(3) = 'Lorem ipsum dolor sit amet, consectetur adipiscing elit, '           // &
                  'sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.'  // &
                  ' Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris' // &
                  ' nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in ' // &
                  'reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla '// &
                  'pariatur. Excepteur sint occaecat cupidatat non proident, sunt in '  // &
                  'culpa qui officia deserunt mollit anim id est laborum'

    
    call br%set_caller(upper_caller)
    do i = 1, 3
        br%name = 'upper1'; call br%run(string(trim(s(i))), upper1)
        br%name = 'upper2'; call br%run(string(trim(s(i))), upper2)
        br%name = 'upper3'; call br%run(string(trim(s(i))), upper3)
    end do

    read(*,*)
end program
            
