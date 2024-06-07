!'██████╗ ███████╗███╗   ██╗ ██████╗██╗  ██╗███╗   ███╗ █████╗ ██████╗ ██╗  ██╗'
!'██╔══██╗██╔════╝████╗  ██║██╔════╝██║  ██║████╗ ████║██╔══██╗██╔══██╗██║ ██╔╝'
!'██████╔╝█████╗  ██╔██╗ ██║██║     ███████║██╔████╔██║███████║██████╔╝█████╔╝ '
!'██╔══██╗██╔══╝  ██║╚██╗██║██║     ██╔══██║██║╚██╔╝██║██╔══██║██╔══██╗██╔═██╗ '
!'██████╔╝███████╗██║ ╚████║╚██████╗██║  ██║██║ ╚═╝ ██║██║  ██║██║  ██║██║  ██╗'
!'╚═════╝ ╚══════╝╚═╝  ╚═══╝ ╚═════╝╚═╝  ╚═╝╚═╝     ╚═╝╚═╝  ╚═╝╚═╝  ╚═╝╚═╝  ╚═╝'
module benchmark
    use, intrinsic :: iso_c_binding
    use benchmark_kinds
    use benchmark_timer, only: clock
    use benchmark_statistics, only: stats
    use benchmark_version
    use benchmark_systeminfo
    use benchmark_method
#ifdef _OPENMP
#include <openmp.use>
#endif

    implicit none

    private

    type, public :: benchmark_runner
        private
        integer, public     :: repetitions = 10 !< number of repetitions
        integer, public     :: maxcalls = 10000000 !< maximum number of function calls
        real(r8), public    :: mintime = 100.0_r8 !< minimum sampling time in ms to collect data
        real(r8), public    :: maxtime = 10.0d5_r8 !< maximum sampling time in ms to collect data
        logical, public     :: skip_warmup = .false. !< option to skip warm up phase
        logical, public     :: skip_dryrun = .false. !< option to skip dry run phase
        integer, public     :: sampling_window = 10 !< option to adjust the size of the sampling window
        integer, public     :: ssd_threshold = 0.05 !< acceptance threshold for the steady-state detection
        !private
        integer :: count = 0
        logical :: firstcall = .true.
        character(80) :: name
        type(stats) :: s
    contains
        procedure, pass(this), public :: load => benchmark_load
        procedure, pass(this), public :: serialize => benchmark_serialize
        procedure, pass(this), public :: deserialize => benchmark_deserialize
        procedure, pass(this), private :: benchmark_void
        procedure, pass(this), private :: benchmark_a1
        procedure, pass(this), private :: benchmark_a2
        generic, public :: run => benchmark_void, &
                                  benchmark_a1, &
                                  benchmark_a2
    end type

contains

    subroutine benchmark_load(this, path)
        class(benchmark_runner), intent(inout) :: this
        character(*), intent(in) :: path
        !private
        logical :: exists
        integer :: lu, ios

        inquire(file=trim(path), exists = exists)
        if (exists) then
            open (newunit=lu, file=trim(path), status='old', action='read', iostat = ios)
            if (ios == 0) then
                read(lu, *) 
            end if
        end if
    end subroutine

    subroutine setup(this)
        class(benchmark_runner), intent(inout) :: this
        if (this%firstcall) then                                                                            
            write (*, '(A)') new_line('A'), &
                            &                '// * BENCHMARK *'
            write (*, '(A)') '        Benchmark execution engine version ' // version
            
            write (*, '(A)') new_line('A'), &
                            &                '// * SYSTEM INFO *'
            call systeminfo()
#if  defined (DEBUG)  || !defined (_DEBUG)
            call warning_debug()
#endif
            write (*, '(A)') new_line('A'), &
    &                '// * STATISTICS *'
            this%firstcall = .false.
        end if
        this%count = this%count + 1
    end subroutine

    subroutine summary(this)
        class(benchmark_runner), intent(in) :: this

        if (len_trim(this%name) > 0) then
            write (*, '(A,F15.2,A,F15.2)') trim(this%name), 1000_r8 * this%s%mean, ' us  +/- ', 1000_r8 * this%s%stddev
        else
            write (*, '(I20,F15.2,A,F15.2)') this%count, 1000_r8 * this%s%mean, ' us  +/- ', 1000_r8 * this%s%stddev
        end if
    end subroutine

    subroutine benchmark_void(this, f)
        class(benchmark_runner), intent(inout) :: this
        procedure() :: f
        !private
        integer :: k, count
        real(r8) :: start, finish
        type(method(0)) :: mtd
        
        call method(f, mtd)

        call setup(this)

        count = 0

        !warm up
        if (.not. this%skip_warmup) then
            block
                call clock(start)
                finish = start
                do while (count <= this%maxcalls .and. finish - start < this%mintime)
                    call mtd%invoke()
                    count = count + 1
                    call clock(finish)
                end do
                if (count > this%maxcalls) call warning_maxcalls()
            end block
        else
            count = this%maxcalls
        end if

        associate (s => this%s)
            call s%reset()
            do k = 1, this%repetitions
                call clock(start)
                block
                    integer :: m
                    do m = 1, count
                        call mtd%invoke()
                    end do
                end block
                call clock(finish)
                call s%update(start, finish)
                s%n = s%n + 1
            end do

            call s%finalize(count)
        end associate

        call summary(this)
    end subroutine

    subroutine benchmark_a1(this, f, a1, name)
        class(benchmark_runner), intent(inout) :: this
        procedure() :: f
        class(*), intent(in) :: a1
        character(*), optional :: name
        !private
        integer :: j, k, count
        real(r8) :: start, finish
        type(stats) :: s
        type(method(1)) :: mtd
        
        call method(f, mtd)

        if (present(name)) this%name = name
        call setup(this)

        count = 0

        !warm up
        if (.not. this%skip_warmup) then
            block
                call clock(start)
                finish = start
                do while (count < this%maxcalls .and. finish - start < this%mintime)
                    call mtd%invoke(a1)
                    count = count + 1
                    call clock(finish)
                end do
                if (count == this%maxcalls) call warning_maxcalls()
            end block
        else
            count = this%maxcalls
        end if

        associate (s => this%s)
            call s%reset()

            do k = 1, this%repetitions
                call clock(start)
                block
                    integer :: m
                    do m = 1, count
                        call mtd%invoke(a1)
                    end do
                end block
                call clock(finish)
                call s%update(start, finish)
                s%n = s%n + 1
            end do
            call s%finalize(count)
        end associate

        call summary(this)
    end subroutine
    
    subroutine benchmark_a2(this, f, a1, a2, name)
        class(benchmark_runner), intent(inout) :: this
        procedure() :: f
        class(*), intent(in) :: a1, a2
        character(*), optional :: name
        !private
        integer :: j, k, count
        real(r8) :: start, finish
        type(stats) :: s
        type(method(2)) :: mtd
        
        call method(f, mtd)

        if (present(name)) this%name = name
        call setup(this)

        count = 0

        !warm up
        if (.not. this%skip_warmup) then
            block
                call clock(start)
                finish = start
                do while (count < this%maxcalls .and. finish - start < this%mintime)
                    call mtd%invoke(a1, a2)
                    count = count + 1
                    call clock(finish)
                end do
                if (count == this%maxcalls) call warning_maxcalls()
            end block
        else
            count = this%maxcalls
        end if

        associate (s => this%s)
            call s%reset()

            do k = 1, this%repetitions
                call clock(start)
                block
                    integer :: m
                    do m = 1, count
                        call mtd%invoke(a1, a2)
                    end do
                end block
                call clock(finish)
                call s%update(start, finish)
                s%n = s%n + 1
            end do
            call s%finalize(count)
        end associate

        call summary(this)
    end subroutine

    subroutine benchmark_serialize(this, str)
        class(benchmark_runner), intent(in), target   :: this
        character(:), allocatable, intent(out) :: str
        !private
        class(benchmark_runner), pointer :: runner => null()
        namelist / config / runner
        allocate(character(100) :: str)
            
        runner => this
                 
        write(str, nml=config)
            
        str = trim(str)
        nullify(runner)
    end subroutine
        
    subroutine benchmark_deserialize(this, str)
        class(benchmark_runner), allocatable, intent(inout)   :: this
        character(*), intent(in) :: str
        !private
        class(benchmark_runner) :: runner
        namelist / config / runner
                 
        read(str, nml=config)
        allocate(this, source=runner)
    end subroutine
end module
