!> @ingroup group_all group_steps
!> @author davidpfister
!> @brief Run the method call and compute statistics
!> @details Runs the method call until reaching steady state. 
!>          Statistics as mean and standard deviations are 
!>          computed and reported
module benchmark_steps_benchmark_run
    use benchmark_steady_state_detection, only: ssd
    use benchmark_workflow, only: workflow
    use benchmark_options
    use benchmark_method
    use benchmark_timer, only: clock
    use benchmark_statistics, only: stats   
    use benchmark_warning
    use benchmark_string
    use benchmark_kinds
    use benchmark_output_unit
    
    implicit none
    
    private
    
    public :: benchmarker
    
    logical :: first_call = .true.
    
    type, extends(workflow) :: benchmark_run
        type(runner_options), pointer :: options => null()
        type(method), pointer :: method => null()
    end type
       
    interface benchmarker
        module procedure :: benchmarker_new
    end interface
    
    contains
    
    type(benchmark_run) function benchmarker_new(options, mtd) result(step)
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
        integer :: k
        real(r8) :: start, finish
        type(stats) :: s
        real(r8), allocatable :: times(:)
        real(r8) :: treshold
        
        select type(step)
        type is (benchmark_run)
            treshold = step%options%ssd_threshold
            if (first_call)  write (output_unit, '(A)') new_line('A'), step%header, new_line('A')

            step%options%count = step%options%count + 1
            allocate(times(step%options%sampling_window), source=0.0_r8)
            
            block
                integer     :: icount, jcount, count
                real(r8)    :: crit, t
                
                crit = 0.0_r8; icount = 0; jcount = 0; count = 0
                call clock(start)
                do while (crit <= (1.0_r8 - treshold))
                    call step%method%invoke(); call clock(finish)
                    t = finish - start
                    icount = icount + 1
                    if (t > step%options%mintime) then 
                        times(1 + modulo(jcount, step%options%sampling_window)) = t / real(icount, r8)
                        jcount = jcount + 1
                        
                        if (jcount >= step%options%sampling_window) crit = ssd(times, modulo(jcount, step%options%sampling_window), treshold)
                        
                        icount = 0
                        call clock(start)
                    end if
                    count = count + 1

                    if (count >= step%options%maxcalls) then 
                        call warning_maxcalls()
                        exit
                    end if
                end do
            end block

            call s%compute(times)
            if (step%options%csv_unit > 0) then
                call summary(step, s, step%options%csv_unit)
            else
                call summary(step, s)
            end if
            deallocate(times)
            nullify(step%method)
            nullify(step%options)
        end select
    end subroutine
    
    subroutine summary(step, s, csv_unit)
        class(benchmark_run), intent(in) :: step
        class(stats), intent(in) :: s
        integer, optional :: csv_unit
        !private
        character(48) :: column
        character(:), allocatable :: c
        character(:), allocatable :: row
        character(:), allocatable :: csv
        integer :: i
        
        if (first_call) then
            row = ''
        
            c = '|              Method Name'
            column = c
            row = '     ' // column // '|'
            column = '          Mean'
            row = row // column(1:24) // '|'
            column = '    Standard Deviation'
            row = row // column(1:24) // '|'

            if (present(csv_unit)) then
                write(csv_unit, '(A)') 'Method Name;'//'Mean;'//'Standard Deviation'
            end if
        
            write (output_unit, '(A)') row
            row = '     '//'|'//repeat('_', 47)//'|'
            row = row // repeat('_', 24) // '|'
            row = row // repeat('_', 24) // '|'
        
             write (output_unit, '(A)') row
             first_call = .false.
        end if
        
        row = ''
        
        c = '|'
        if (len_trim(step%options%name) > 0) then
            c = c // trim(adjustl(step%options%name))
        else
            c = c // 'Method'//str(step%options%count)
        end if
        
        c = c //'('
        do i = 1, step%method%nargs
            if (i == 1) then
                c = c // step%method%args(i)%to_string()
            else
                c = c // ','// step%method%args(i)%to_string()
            end if
        end do
        c = c // ')'
        column = c
        row = '     ' // column // '|'

        if (present(csv_unit)) then
            csv = trim(column(2:))
        end if

        select case (int(log10(s%mean)))
            case (1:2)
                column = str(s%mean, '(f12.3)') // ' ms'
                if (present(csv_unit)) then
                    csv = csv //';'//str(s%mean, '(f12.3)')
                end if        
                row = row // adjustr(column(1:24)) // '|'
                column = ' +/- '//str(s%stddev, '(f12.3)') // ' ms|'
                if (present(csv_unit)) then
                    csv = csv //';'//str(s%stddev, '(f12.3)')
                end if 
                row = row // adjustr(column(1:25))
            case (3:)
                column = str(s%mean / 1000.0_r8, '(f12.3)') // '  s'
                if (present(csv_unit)) then
                    csv = csv //';'//str(s%mean, '(f12.3)')
                end if  
                row = row // adjustr(column(1:24)) // '|'
                column = ' +/- '//str(s%stddev / 1000.0_r8, '(f12.3)') // '  s|'
                if (present(csv_unit)) then
                    csv = csv //';'//str(s%stddev, '(f12.3)')
                end if  
                row = row // adjustr(column(1:25))
            case default
                column = str(1000.0_r8 * s%mean, '(f12.3)') // ' us'
                if (present(csv_unit)) then
                    csv = csv //';'//str(s%mean, '(f12.3)')
                end if
                row = row // adjustr(column(1:24)) // '|'
                column = ' +/- '//str(1000.0_r8 * s%stddev, '(f12.3)') // ' us|'
                if (present(csv_unit)) then
                    csv = csv //';'//str(s%stddev, '(f12.3)')
                end if
                row = row // adjustr(column(1:25))
            end select
        
        write (output_unit, '(A)') row
        if (present(csv_unit)) then
            write (csv_unit, '(A)') csv
        end if
    end subroutine
    
end module