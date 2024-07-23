!> @ingroup group_all group_steps
!> @author davidpfister
!> @brief list of default steps
module benchmark_steps
    use benchmark_steps_setup
    use benchmark_steps_system
    use benchmark_steps_compiler
    use benchmark_steps_dryrun
    use benchmark_steps_benchmark_run
    use benchmark_workflow, only: workflow
    use benchmark_options
    
    implicit none 
    
    public :: steps_initialize, &
              benchmarker
    
    private
    
    contains
    
    subroutine steps_initialize(wf, options)
        type(workflow), allocatable, intent(inout), target :: wf
        class(runner_options), intent(in) :: options
        
        if(.not. allocated(wf)) allocate(wf)

        call wf%add(setup())
        call wf%add(system())
        call wf%add(compiler())
        call wf%add(dryrun(options))
    end subroutine
    
end module