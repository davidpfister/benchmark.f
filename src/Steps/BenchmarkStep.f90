module benchmark_steps_benchmark_step

    implicit none
    
    contains
    
    subroutine start_benchmark()
        write (*, '(A)') new_line('A'), &
                            &                '// * BENCHMARK STARTS *'
        call systeminfo()
    end subroutine
end module