module benchmark_steps
    use benchmark_steps_setup_step
    use benchmark_steps_system_step
    use benchmark_steps_compiler_step
    use benchmark_steps_dryrunstep
    use benchmark_steps_benchmark_step
    use benchmark_workflow, only: workflow
        
    implicit none 
    
    public :: steps_initialize, &
              benchmark_step
    
    private
    
    contains
    
    subroutine steps_initialize(wf)
        type(workflow), allocatable, intent(inout), target :: wf

        if(.not. allocated(wf)) allocate(wf)

        call wf%add(setup_step())
        call wf%add(system_step())
        call wf%add(compiler_step())
        call wf%add(dryrun_step())
    end subroutine
    
end module