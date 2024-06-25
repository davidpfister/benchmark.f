module benchmark_steps_compiler_step
    use benchmark_compilerinfo
    use benchmark_workflow, only: workflow
    
    implicit none
    
    private
    
    public :: compiler_step
       
    interface compiler_step
        module procedure :: compiler_step_new
    end interface
    
    contains
    
    type(workflow) function compiler_step_new() result(step)
        step%header = '// * COMPILER INFO *'
        step%action => get_compilerinfo
    end function
    
    subroutine get_compilerinfo(step)
        class(workflow), intent(inout) :: step
        
        write (*, '(A)') new_line('A'), step%header
        call compilerinfo()
    end subroutine
end module