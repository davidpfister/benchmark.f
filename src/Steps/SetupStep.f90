module benchmark_steps_setup_step
    use benchmark_version, only: version
    
    implicit none
    
    contains
    
    subroutine get_setup()
        write (*, '(A)') new_line('A'), &
                            &                '// * BENCHMARK *'
        write (*, '(A)') '        Benchmark execution engine version ' // version
    end subroutine
end module