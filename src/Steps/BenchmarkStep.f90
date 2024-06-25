module benchmark_steps_benchmark_step
    use benchmark_workflow, only: workflow
    use benchmark_options
    use benchmark_method
    use benchmark_timer, only: clock
    use benchmark_statistics, only: stats   
    use benchmark_warning
    
    implicit none
    
    private
    
    public :: benchmark_step
    
    logical :: first_call = .true.
    
    type, extends(workflow) :: bworkflow
        type(runner_options), allocatable :: options
        type(method(:)), pointer :: method => null()
    end type
       
    interface benchmark_step
        module procedure :: benchmark_step_new
    end interface
    
    contains
    
    type(bworkflow) function benchmark_step_new(options, mtd) result(step)
        class(runner_options), intent(in) :: options
         class(method(*)), intent(in), target :: mtd
            
        step%header = '// * BENCHMARK STARTS *'
        step%action => start_benchmark
        allocate(step%options, source = options)

        step%method => mtd
    end function
    
    subroutine start_benchmark(step)
        class(workflow), intent(inout) :: step
        !private
        integer :: k, count
        real(r8) :: start, finish
        type(stats) :: s
        
        select type(step)
        type is (bworkflow)
            if (first_call) then
                write (*, '(A)') new_line('A'), step%header
                first_call = .false.
            end if
            step%options%count = step%options%count + 1
        
            count = 0

            !warm up
            if (.not. step%options%skip_warmup) then
                block
                    call clock(start)
                    finish = start
                    do while (count <= step%options%maxcalls .and. finish - start < step%options%mintime)
                        call step%method%invoke()
                        count = count + 1
                        call clock(finish)
                    end do
                    if (count > step%options%maxcalls) call warning_maxcalls()
                end block
            else
                count = step%options%maxcalls
            end if

            call s%reset()
            do k = 1, step%options%repetitions
                call clock(start)
                block
                    integer :: m
                    do m = 1, count
                        call step%method%invoke()
                    end do
                end block
                call clock(finish)
                call s%update(start, finish)
                s%n = s%n + 1
            end do

            call s%finalize(count)

            call summary(step, s)
            nullify(step%method)
        end select
    end subroutine
    
    subroutine summary(step, s)
        class(bworkflow), intent(in) :: step
        class(stats), intent(in) :: s

        if (len_trim(step%options%name) > 0) then
            write (*, '(A,F15.2,A,F15.2)') trim(step%options%name), 1000_r8 * s%mean, ' us  +/- ', 1000_r8 * s%stddev
        else
            write (*, '(I20,F15.2,A,F15.2)') step%options%count, 1000_r8 * s%mean, ' us  +/- ', 1000_r8 * s%stddev
        end if
    end subroutine
end module