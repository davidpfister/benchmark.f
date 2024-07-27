program parsers
    use parser_runner
    use fx_benchmark, only: benchmark
    use parameters
    
    implicit none
    
    real(r8) :: res
    integer :: i, j, k
    type(benchmark) :: bench

    do j = 1, 8
        call runner%initialize(parsers(j))
        do i = 1, 24
            runner%index = i
            call bench%run(i, parsers(j), runner_test)
        end do
    end do
    pause
end program
    