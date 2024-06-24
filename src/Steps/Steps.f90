module benchmark_steps
    use benchmark_steps_setup_step
    use benchmark_steps_system_step
    use benchmark_steps_compiler_step
    use benchmark_steps_dryrunstep
    use benchmark_steps_benchmark_step
    use benchmark_workflow
    
    implicit none 
    
    public :: steps_initialize, &
              start_dryrun, &
              start_benchmark
    
    private
    
    contains
    
    subroutine steps_initialize(wf)
        type(workflow), allocatable, intent(inout), target :: wf
        !private 
        class(workflow), pointer :: p => null()
        
        if(.not. allocated(wf)) allocate(wf)

        p => wf%add(get_setup)
        p => p%add(get_systeminfo)
        p => p%add(get_compilerinfo)
        nullify(p)
    end subroutine
    
end module