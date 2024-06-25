module benchmark_steps_benchmark_step
    use benchmark_workflow, only: workflow
    
    implicit none
    
    private
    
    public :: benchmark_step
       
    interface benchmark_step
        module procedure :: benchmark_step_new
    end interface
    
    contains
    
    type(workflow) function benchmark_step_new() result(step)
        step%header = '// * BENCHMARK STARTS *'
        step%action => start_benchmark
    end function
    
    subroutine start_benchmark(step)
        class(workflow), intent(inout) :: step
        write (*, '(A)') new_line('A'), step%header
        call systeminfo()
    end subroutine
end module