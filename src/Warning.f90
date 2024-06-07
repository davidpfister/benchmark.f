module benchmark_warning
    use benchmark_kinds
    
    implicit none
    
    subroutine warning_debug()
        write (*, '(A)') new_line('A'), &
                            &                '// * WARNING *', &
                            &                new_line('A')
        write (*, '(A)') '        DEBUG mode detected. The results might be unreliable.'
    end subroutine
    
    subroutine warning_maxcalls()
        write (*, '(A)') new_line('A'), &
                        &                '// WARNING'
        write (*, '(A)') '        Maximum number of calls reached. The warmup phase may not have reached steady state.'
    end subroutine
    
end module