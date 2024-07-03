module benchmark_steps_compiler
    use benchmark_compilerinfo
    use benchmark_workflow, only: workflow
    
    implicit none
    
    private
    
    public :: compiler
       
    interface compiler
        module procedure :: compiler_new
    end interface
    
    contains
    
    type(workflow) function compiler_new() result(step)
        step%header = '                                     -- COMPILER INFO --                                               '
        step%action => step_run
    end function
    
    subroutine step_run(step)
        class(workflow), intent(inout) :: step
        
        write (*, '(A)') new_line('A'), step%header
        call get_compilerinfo()
    end subroutine
end module