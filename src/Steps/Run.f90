module benchmark_steps_benchmark_run
    use benchmark_workflow, only: workflow
    use benchmark_options
    use benchmark_method
    use benchmark_timer, only: clock
    use benchmark_statistics, only: stats   
    use benchmark_warning
    use benchmark_string
    
    implicit none
    
    private
    
    public :: benchmark_run
    
    logical :: first_call = .true.
    
    type, extends(workflow) :: benchmark_run
        type(runner_options), pointer :: options => null()
        type(method), pointer :: method => null()
    end type
       
    interface benchmark_run
        module procedure :: benchmark_run_new
    end interface
    
    contains
    
    type(benchmark_run) function benchmark_run_new(options, mtd) result(step)
         class(runner_options), intent(in), target :: options
         class(method), intent(in), target :: mtd
         step%header = '                                     -- BENCHMARKING --                                              '
         step%action => step_run
         step%options => options

        step%method => mtd
    end function
    
    subroutine step_run(step)
        class(workflow), intent(inout) :: step
        !private
        integer :: k, count
        real(r8) :: start, finish
        type(stats) :: s
        
        select type(step)
        type is (benchmark_run)
            if (first_call) then
                write (*, '(A)') new_line('A'), step%header
                first_call = .false.
            end if
            step%options%count = step%options%count + 1
        
            count = 0

            !warm up
            if (.not. step%options%skip_warmup) then
                call clock(start)
                finish = start
                do while (count <= step%options%maxcalls .and. finish - start < step%options%mintime)
                    call step%method%invoke()
                    count = count + 1
                    call clock(finish)
                end do
                if (count > step%options%maxcalls) call warning_maxcalls()
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
            nullify(step%options)
        end select
    end subroutine
    
    subroutine summary(step, s)
        class(benchmark_run), intent(in) :: step
        class(stats), intent(in) :: s
        !private
        character(60) :: column
        character(:), allocatable :: c
        character(:), allocatable :: row
        integer :: i
        
        row = ' '
        
        c = '|'
        if (len_trim(step%options%name) > 0) then
            c = c // trim(adjustl(step%options%name))
        else
            c = c // 'Method'//str(step%options%count)
        end if
        
        c = c //'('
        do i = 1, step%method%nargs
            if (i == 1) then
                c = c // step%method%args(i)%display
            else
                c = c // ','// step%method%args(i)%display
            end if
        end do
        c = c // ')'
        column = c
        row = '         ' // column // '|'
        column = str(1000_r8 * s%mean, '(f12.3)') // ' us'
        row = row // adjustr(column(1:24)) // '|'
        column = ' +/- '//str(1000_r8 * s%stddev, '(f12.3)')
        row = row // adjustr(column(1:24)) // '|'
        
         write (*, '(A)') row
    end subroutine
    
end module