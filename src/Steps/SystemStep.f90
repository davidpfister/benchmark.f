module benchmark_steps_system_step
    use benchmark_systeminfo
    use benchmark_workflow, only: workflow
    
    implicit none
    
    private
    
    public :: system_step
       
    interface system_step
        module procedure :: system_step_new
    end interface
    
    contains
    
    type(workflow) function system_step_new() result(step)
        step%header = '// * SYSTEM INFO *'
        step%action => get_systeminfo
    end function
    
    subroutine get_systeminfo(step)
        class(workflow), intent(inout) :: step
        write (*, '(A)') new_line('A'), step%header
        call systeminfo()
    end subroutine
end module