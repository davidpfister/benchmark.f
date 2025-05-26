!> @defgroup group_steps benchmark_steps
!> @brief Steps
!> @{
module benchmark_steps
    use benchmark_steps_setup
    use benchmark_steps_system
    use benchmark_steps_compiler
    use benchmark_steps_dryrun
    use benchmark_steps_run
    use benchmark_workflow, only: workflow
    use benchmark_options
    
    implicit none 
    
    public :: steps_initialize, &
              benchmarker
    
    private
    
    contains
    
    subroutine steps_initialize(wf, options)
        type(workflow), allocatable, intent(inout), target  :: wf
        class(runner_options), intent(in)                   :: options
        
        if(.not. allocated(wf)) allocate(wf)
        call wf%add(setup())
        if (.not. options%skip_prelude) then
            call wf%add(system())
            call wf%add(compiler())
            call wf%add(dryrun(options))
        end if
    end subroutine
    
end module
!> @}