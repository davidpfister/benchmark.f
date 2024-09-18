!> @ingroup group_steps
!> @defgroup group_steps_dryrun benchmark_steps_dryrun
!> @brief Run the method calls with an empty dummy function
!> @{
module benchmark_steps_dryrun
    use benchmark_workflow, only: workflow
    use benchmark_timer, only: clock
    use benchmark_kinds
    use benchmark_method
    use benchmark_options
    use benchmark_string, only: str
    use benchmark_output_unit
    
    implicit none
    
    private
    
    public :: dryrun
    
    type, extends(workflow) :: benchmark_dryrun
        type(runner_options), pointer :: options => null()
    contains
        procedure, pass(this), public :: dispose
        final :: finalize
    end type
       
    interface dryrun
        module procedure :: dryrun_new
    end interface
    
    real(r8), parameter :: MINTIME = 100.0_r8
    
    contains
    
    type(benchmark_dryrun) function dryrun_new(options) result(step)
        class(runner_options), intent(in), target :: options    
        step%header = '                                       -- DRY RUN --                                                 '
        step%action => start_dryrun
        step%options => options
    end function
    
    subroutine start_dryrun(step)
        class(workflow), intent(inout) :: step
        !private
        integer :: k, count
        real(r8) :: start, finish, overhead
        type(method) :: mtd
        integer :: repeat
        
        ! write (output_unit, '(A)') new_line('A'), step%header
        
        mtd = method(dummy_empty)
        repeat = 0
        
        select type(step)
        type is (benchmark_dryrun)
            do while (repeat < 5)
                count = 0
                call clock(start)
                finish = start
                do while (finish - start < MINTIME)
                    call mtd%invoke()
                    count = count + 1
                    call clock(finish)
                end do
                repeat = repeat + 1
            end do
            overhead = (finish - start)/real(count, r8)
            step%options%overhead = overhead
            nullify(step%options)
        end select
        ! write (output_unit, '(A)') new_line('A'), '                          Overhead:                   '//str(overhead, '(f12.3)') // ' us'
    end subroutine
    
    subroutine dummy_empty()
        !do nothing
    end subroutine

    !> @brief Dispose resources associated with 
    !!        the bound type.
    !! @param[inout] this The type bound to the method
    !!
    !! @b Remarks
    subroutine dispose(this)
        class(benchmark_dryrun), intent(inout) :: this
        
        call finalize(this)
    end subroutine

    !> @private
    recursive subroutine finalize(this)
        type(benchmark_dryrun), intent(inout) :: this
        
        if (associated(this%options)) this%options => null()
    end subroutine
    
end module