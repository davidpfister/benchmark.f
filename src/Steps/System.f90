module benchmark_steps_system
    use benchmark_systeminfo
    use benchmark_workflow, only: workflow
    
    implicit none
    
    private
    
    public :: system
       
    interface system
        module procedure :: system_new
    end interface
    
    contains
    
    type(workflow) function system_new() result(step)
        step%header = '                                     -- SYSTEM INFO --                                               '
        step%action => step_run
    end function
    
    subroutine step_run(step)
        class(workflow), intent(inout) :: step
        
        write (*, '(A)') new_line('A'), step%header
        call get_systeminfo()
    end subroutine
end module