program snippet

block
!> [arg_constructor]
use benchmark_argument
use benchmark_kinds
      
type(arg) :: a
       
a = arg(12.25_r8, 'arg1')

select type(x => a%value)
type is (real(r8))
   !success
class default
   stop
end select
!> [arg_constructor]
end block

block
!> [arg_base_derivation]
type, extends(arg_base), public :: arg
     private
     contains
     procedure, pass(lhs), private     :: any_assign_argument
     generic :: assignment(=)          => any_assign_argument
     procedure, pass(this), public     :: to_string
end type
!> [arg_base_derivation]
end block

block
   !> [method_caller]
   use benchmark_string
    use benchmark_library
    use utility
    
    implicit none
    
    integer :: i
    type(runner) :: br   
    type(string) :: s(3)
    
    s(1) = 'abcdefghijklmnopqrstuvwxyz'
    s(2) = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    s(3) = 'Lorem ipsum dolor sit amet, consectetur adipiscing elit, '           // &
                  'sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.'  // &
                  ' Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris' // &
                  ' nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in ' // &
                  'reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla '// &
                  'pariatur. Excepteur sint occaecat cupidatat non proident, sunt in '  // &
                  'culpa qui officia deserunt mollit anim id est laborum'

    
    call br%set_caller(upper_caller)
    do i = 1, 3
        br%name = 'upper1'; call br%run(s(i), upper1)
        br%name = 'upper2'; call br%run(s(i), upper2)
        br%name = 'upper3'; call br%run(s(i), upper3)
    end do

    read(*,*)

    subroutine upper_caller(f, a)
      procedure(upper_x) :: f
      type(string), intent(in) :: a
      
      block
          character(len(a)) :: res
              
          res = f(a%chars)
      end block
    end subroutine
   !> [method_caller]
   end block

block
!> [workflow_example]
use benchmark_workflow

type(workflow), allocatable, target  :: wf
class(workflow), pointer             :: current

if(.not. allocated(wf)) allocate(wf)
call wf%add(setup())

if (.not. options%skip_prelude) then
   call wf%add(system())
   call wf%add(compiler())
   call wf%add(dryrun())
end if

current => wf%run()
!> [workflow_example]
end block

block
   !> [warning_example1]
   use benchmark_warning

#ifdef _DEBUG
   call warning_debug()
#endif
   !> [warning_example1]
   end block

   block
   !> [warning_example2]
   use benchmark_warning

   if (display_maxcall_warning) then
      call warning_maxcalls()
   end if
   !> [warning_example2]
   end block

   block
   !> [options_constructor]
   use benchmark_options

   type(runner_options) :: options

   options%maxcalls = 10000
   options%mintime  = 1000    ! in milliseconds
   options%maxtime  = 500000  ! in milliseconds
   !> [options_constructor]
   end block

   block
   !> [options_csv]
   use benchmark_options

   type(runner_options) :: options
   integer, parameter :: lu = 15

   open(unit=lu, file = 'report.csv')
   options%csv_unit = lu

   !> [options_csv]
   end block

   block
   !> [benchmark_ex1]
   #include <benchmark.inc>
   use rhofunc
   use benchmark_library
   
   implicit none
   
   type(runner) :: br

   !calling using preprocessor macro
   benchmark(br, run(1.0d-6, 30, foo))  
   benchmark(br, run(1.0d-6, 30, bar))

   read(*,*)
   !> [benchmark_ex1]
   end block

   block
      !> [benchmark_ex2]
      use benchmark_kinds
      use benchmark_library
      
      type(runner) :: br
      type(runner) :: br2
      logical :: exists
      integer :: lu
      
      br%maxcalls = 10
      br%csv_unit = -10
      br%mintime = 50_r8
      br%maxtime = 100_r8
      br%offset = 20.0_r8
      br%sampling_window = 10
      br%ssd_threshold = 0.025_r8
      
      call br%write('benchmark.nml')
      
      inquire(file='benchmark.nml', exist=exists)
      if (exists) then
         call br2%read('benchmark.nml')
      end if
      !> [benchmark_ex2]
   end block

   block
      !> [timer]
      use benchmark_timer
      
      real(r8) :: time
      
      call clock(time)
      write(*,*) 'The actual time is ', time, ' ms.'
      !> [timer]
   end block

   block
      !> [systeminfo]
      use benchmark_systeminfo
      
      write (*, '(A)') 'OS:'//os_name(get_os_type())
      !> [systeminfo]
   end block

   block
      !> [version]
      use benchmark_version
      
      character(:), allocatable :: v
      
      v = version
      write(*,*) 'The current version number is ' // v
      !The example display the following output
      !        'The current version number is 1.0.0'
      !> [version]
   end block

   block
   !> [ssd]
   use benchmark_steady_state_detection, only: ssd
   use benchmark_workflow, only: workflow
   use benchmark_options
   use benchmark_method
   use benchmark_timer, only: clock
   use benchmark_statistics, only: stats   
   use benchmark_warning, only: display_maxcall_warning
   use benchmark_string
   use benchmark_kinds
   use benchmark_output_unit

   class(benchmark_run), intent(inout) :: step
   !private
   integer :: k
   real(r8) :: start, finish
   type(stats) :: s
   real(r8), allocatable :: times(:)
   real(r8) :: treshold
      
      
   maxcall_reached = .false.
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
               times(1 + modulo(jcount, step%options%sampling_window)) =   &
                  t / real(icount, r8)
               jcount = jcount + 1
               
               if (jcount >= step%options%sampling_window) crit =          &
                  ssd(times, modulo(jcount, step%options%sampling_window), &
                  treshold)
               
               icount = 0
               call clock(start)
         end if
         count = count + 1

         if (count >= step%options%maxcalls) then 
               maxcall_reached = .true.
               display_maxcall_warning = .true.
               exit
         end if
      end do
   end block

   call s%compute(times)
   if (step%options%csv_unit /= 0) then
      call summary(step, s, step%options%csv_unit)
   else
      call summary(step, s)
   end if
   deallocate(times)
   nullify(step%method)
   nullify(step%options)
   !> [ssd]
   end block

end program