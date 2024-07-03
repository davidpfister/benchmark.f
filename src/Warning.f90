module benchmark_warning
    use benchmark_kinds
    
    implicit none
    
    contains
    
    subroutine warning_debug()
        write (*, '(A)') new_line('A'), &
                            &                '                                  <!>     WARNING     <!>'
        write (*, '(A)')                     '                           DEBUG profile detected. The results'//&
        ' might be unreliable.'
    end subroutine
    
    subroutine warning_maxcalls()
        write (*, '(A)') new_line('A'), &
                            &                '                                  <!>     WARNING     <!>'
        write (*, '(A)') '                           Maximum number of calls reached. The warmup phase may'//&
        ' not have reached steady state.'
    end subroutine
    
end module