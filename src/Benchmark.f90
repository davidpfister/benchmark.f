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
    use benchmark_method
    use benchmark_workflow
    use benchmark_steps
    use benchmark_warning
#ifdef _OPENMP
#include <openmp.use>
#endif

    implicit none

    private
    
    type(workflow), allocatable :: benchflow

    type, public :: runner
        private
        integer, public     :: repetitions = 10 !< number of repetitions
        integer, public     :: maxcalls = 10000000 !< maximum number of function calls
        real(r8), public    :: mintime = 100.0_r8 !< minimum sampling time in ms to collect data
        real(r8), public    :: maxtime = 100000.0_r8 !< maximum sampling time in ms to collect data
        logical, public     :: skip_warmup = .false. !< option to skip warm up phase
        logical, public     :: skip_dryrun = .false. !< option to skip dry run phase
        integer, public     :: sampling_window = 10 !< option to adjust the size of the sampling window
        integer, public     :: ssd_threshold = 0.05_r8 !< acceptance threshold for the steady-state detection
        !private
        integer :: count = 0
        character(80) :: name
        type(stats) :: s
    contains
        procedure, pass(this), public :: load => benchmark_load
        procedure, pass(this), private :: benchmark_serialize_to_unit
        procedure, pass(this), private :: benchmark_serialize_to_string
        generic, public :: serialize => benchmark_serialize_to_unit, &
                                          benchmark_serialize_to_string
        procedure, nopass, private :: benchmark_deserialize_from_unit
        procedure, nopass, private :: benchmark_deserialize_from_string
        generic, public :: deserialize => benchmark_deserialize_from_unit, &
                                          benchmark_deserialize_from_string
        procedure, pass(this), private :: benchmark_void
        procedure, pass(this), private :: benchmark_a1
        procedure, pass(this), private :: benchmark_a2
        generic, public :: run => benchmark_void, &
                                  benchmark_a1, &
                                  benchmark_a2
    end type

contains

    subroutine benchmark_load(this, path)
        class(runner), intent(inout) :: this
        character(*), intent(in) :: path
        !private
        logical :: exists
        integer :: lu, ios

        inquire(file=trim(path), exist = exists)
        if (exists) then
            open (newunit=lu, file=trim(path), status='old', action='read', iostat = ios)
            if (ios == 0) then
                call this%deserialize(this, lu)
                close(lu)
            end if
        end if
    end subroutine

    subroutine summary(this)
        class(runner), intent(in) :: this

        if (len_trim(this%name) > 0) then
            write (*, '(A,F15.2,A,F15.2)') trim(this%name), 1000_r8 * this%s%mean, ' us  +/- ', 1000_r8 * this%s%stddev
        else
            write (*, '(I20,F15.2,A,F15.2)') this%count, 1000_r8 * this%s%mean, ' us  +/- ', 1000_r8 * this%s%stddev
        end if
    end subroutine

    subroutine benchmark_void(this, f, name)
        class(runner), intent(inout) :: this
        procedure() :: f
        character(*), intent(in), optional :: name
        !private
        integer :: k, count
        type(workflow), pointer :: p
        real(r8) :: start, finish
        type(method(0)) :: mtd
        
        if (present(name)) this%name = name
        
        if (.not. allocated(benchflow)) then
            call steps_initialize(benchflow)
            call benchflow%run()
        end if
        
        p => benchflow%add(start_dryrun)
        call p%run()

        this%count = this%count + 1
        mtd = method(f)
        
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
        class(runner), intent(inout) :: this
        procedure() :: f
        class(*), intent(in) :: a1
        character(*), optional :: name
        !private
        integer :: j, k, count
        type(workflow), pointer :: p
        real(r8) :: start, finish
        type(stats) :: s
        type(method(1)) :: mtd
        
        if (.not. allocated(benchflow)) then
            call steps_initialize(benchflow)
            call benchflow%run()
        end if
        
        p => benchflow%add(start_dryrun)
        call p%run()
        
        this%count = this%count + 1
        mtd = method(f)
        if (present(name)) this%name = name

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
        class(runner), intent(inout) :: this
        procedure() :: f
        class(*), intent(in) :: a1, a2
        character(*), optional :: name
        !private
        integer :: j, k, count
        type(workflow), pointer :: p
        real(r8) :: start, finish
        type(stats) :: s
        type(method(2)) :: mtd
        
        if (.not. allocated(benchflow)) then
            call steps_initialize(benchflow)
            call benchflow%run()
        end if
        
        p => benchflow%add(start_dryrun)
        call p%run()
        
        this%count = this%count + 1
        mtd = method(f, a1, a2)
        if (present(name)) this%name = name

        count = 0

        !warm up
        if (.not. this%skip_warmup) then
            block
                call clock(start)
                finish = start
                do while (count < this%maxcalls .and. finish - start < this%mintime)
                    call mtd%invoke()
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
        deallocate(benchflow)
    end subroutine

    subroutine benchmark_serialize_to_string(this, str)
        class(runner), intent(in), target   :: this
        character(:), allocatable, intent(out) :: str
        !private
        type(runner), pointer :: bench => null()
        namelist / config / bench
        allocate(character(100) :: str)
            
        bench => this
                 
        write(str, nml=config)
            
        str = trim(str)
        nullify(bench)
    end subroutine
    
    subroutine benchmark_serialize_to_unit(this, lu)
        class(runner), intent(in), target   :: this
        integer, intent(in) :: lu
        !private
        type(runner), pointer :: bench => null()
        namelist / config / bench
            
        bench => this
                 
        write(unit=lu, nml=config)
        nullify(bench)
    end subroutine
        
    subroutine benchmark_deserialize_from_string(that, str)
        type(runner), intent(inout)   :: that
        character(*), intent(in) :: str
        !private
        type(runner) :: bench
        namelist / config / bench
                 
        read(str, nml=config)
        that = bench
    end subroutine
    
    subroutine benchmark_deserialize_from_unit(that, lu)
        type(runner), intent(inout)   :: that
        integer, intent(in) :: lu
        !private
        type(runner) :: bench
        namelist / config / bench
                 
        read(nml=config, unit=lu)
        that = bench
    end subroutine
end module
