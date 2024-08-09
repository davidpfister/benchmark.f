program parsers
    use parser_runner
    use parser_factory
    use benchmark_library
    use parameters
    
    implicit none
    
    real(r8) :: res
    integer :: i, j, k
    type(runner) :: br

    do j = 2, 2
        call current_parser%initialize(eq_parsers(j))
        br%name = trim(eq_parsers(j))
        do i = 1, 23
            current_parser%index = i
            call br%run(eqstring(i), runner_test)
        end do
    end do
    pause
end program
    