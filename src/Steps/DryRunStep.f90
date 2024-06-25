module benchmark_steps_dryrunstep
    use benchmark_workflow, only: workflow
    use benchmark_timer, only: clock
    use benchmark_kinds
    use benchmark_method
    
    implicit none
    
    private
    
    public :: dryrun_step
       
    interface dryrun_step
        module procedure :: dryrun_step_new
    end interface
    
    real(r8), parameter :: MINTIME = 100.0_r8
    
    contains
    
    type(workflow) function dryrun_step_new() result(step)
        step%header = '// * DRY RUN *'
        step%action => start_dryrun
    end function
    
    subroutine start_dryrun(step)
        class(workflow), intent(inout) :: step
        !private
        integer :: k, count
        real(r8) :: start, finish
        type(method(0)) :: mtd

        mtd = method(dummy_empty)
        count = 0

        !warm up
        block
            call clock(start)
            finish = start
            do while (finish - start < MINTIME)
                call mtd%invoke()
                count = count + 1
                call clock(finish)
            end do
        end block
    end subroutine
    
    subroutine dummy_empty()
        !do nothing
    end subroutine
    
end module