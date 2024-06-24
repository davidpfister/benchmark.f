module benchmark_steps_system_step
    use benchmark_systeminfo
    
    implicit none
    
    contains
    
    subroutine get_systeminfo()
        write (*, '(A)') new_line('A'), &
                            &                '// * SYSTEM INFO *'
        call systeminfo()
    end subroutine
end module