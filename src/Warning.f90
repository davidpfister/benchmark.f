!> @ingroup group_all group_benchmark
!> @author davidpfister
module benchmark_warning
    use benchmark_kinds
    use benchmark_output_unit
    
    implicit none
    
    contains
    
    subroutine warning_debug()
        write (output_unit, '(A)') new_line('A'), &
                            &                '                                  <!>     WARNING     <!>'
        write (output_unit, '(A)')                     '                           DEBUG profile detected. The results'//&
        ' might be unreliable.'
    end subroutine
    
    subroutine warning_maxcalls()
        write (output_unit, '(A)') new_line('A'), &
                            &                '                                  <!>     WARNING     <!>'
        write (output_unit, '(A)') '                           Maximum number of calls reached. The warmup phase may'//&
        ' not have reached steady state.'
    end subroutine
    
end module