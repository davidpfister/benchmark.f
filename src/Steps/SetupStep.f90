module benchmark_steps_setup_step
    use benchmark_workflow, only: workflow
    use benchmark_version, only: version
    
    implicit none
    
    private
    
    public :: setup_step
       
    interface setup_step
        module procedure :: setup_step_new
    end interface
    
    contains
    
    type(workflow) function setup_step_new() result(step)
        step%header = '// * BENCHMARK *'
        step%action => get_setup
    end function
    
    subroutine get_setup(step)
        class(workflow), intent(inout) :: step
        write (*, '(A)') new_line('A'), step%header
        write (*, '(A)') '        Benchmark execution engine version ' // version
    end subroutine
end module