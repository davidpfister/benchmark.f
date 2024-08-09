!> @ingroup group_benchmark
!> @defgroup group_wqrning warning
!> @{
module benchmark_warning
    use benchmark_kinds
    use benchmark_output_unit
    
    implicit none
    
    private
    
    public :: display_maxcall_warning, &
              warning_debug, &
              warning_maxcalls
    
    logical :: display_maxcall_warning = .false.
    
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
        write (output_unit, '(A)') '                           Maximum number of calls reached for some cases. Steady state'//&
        ' may not have been reached for all runs.'
    end subroutine
    
end module
!> @}