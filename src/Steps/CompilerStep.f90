module benchmark_steps_compiler_step
    use benchmark_compilerinfo
    
    implicit none
    
    contains
    
    subroutine get_compilerinfo()
        write (*, '(A)') new_line('A'), &
                            &                '// * COMPILER INFO *'
        call compilerinfo()
    end subroutine
end module