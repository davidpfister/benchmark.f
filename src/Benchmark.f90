!'██████╗ ███████╗███╗   ██╗ ██████╗██╗  ██╗███╗   ███╗ █████╗ ██████╗ ██╗  ██╗'
!'██╔══██╗██╔════╝████╗  ██║██╔════╝██║  ██║████╗ ████║██╔══██╗██╔══██╗██║ ██╔╝'
!'██████╔╝█████╗  ██╔██╗ ██║██║     ███████║██╔████╔██║███████║██████╔╝█████╔╝ '
!'██╔══██╗██╔══╝  ██║╚██╗██║██║     ██╔══██║██║╚██╔╝██║██╔══██║██╔══██╗██╔═██╗ '
!'██████╔╝███████╗██║ ╚████║╚██████╗██║  ██║██║ ╚═╝ ██║██║  ██║██║  ██║██║  ██╗'
!'╚═════╝ ╚══════╝╚═╝  ╚═══╝ ╚═════╝╚═╝  ╚═╝╚═╝     ╚═╝╚═╝  ╚═╝╚═╝  ╚═╝╚═╝  ╚═╝'
module benchmark
    use, intrinsic :: iso_c_binding
    use benchmark_kinds
    use benchmark_method
    use benchmark_workflow
    use benchmark_steps
    use benchmark_options
#ifdef _OPENMP
#include <openmp.use>
#endif

    implicit none

    private
    
    type(workflow), allocatable, target :: root
    class(workflow), pointer             :: current

    type, extends(runner_options), public :: runner
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
        procedure, pass(this), private :: benchmark_a3
        procedure, pass(this), private :: benchmark_a4
        procedure, pass(this), private :: benchmark_a5
        procedure, pass(this), private :: benchmark_a6
        procedure, pass(this), private :: benchmark_a7
        generic, public :: run => benchmark_void, &
                                  benchmark_a1, &
                                  benchmark_a2, &
                                  benchmark_a3, &
                                  benchmark_a4, &
                                  benchmark_a5, &
                                  benchmark_a6, &
                                  benchmark_a7
        final :: finalize
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

    subroutine benchmark_void(this, f, name)
        class(runner), intent(inout) :: this
        procedure() :: f
        character(*), intent(in), optional :: name
        !private
        type(method(0)) :: mtd

        if (present(name)) this%name = name
        
        if (.not. allocated(root)) then
            call steps_initialize(root)
            current => root%run()
        end if
        
        mtd = method(f)
        
        call current%add(benchmark_step(this, mtd))
        current => current%run()
        
    end subroutine

    subroutine benchmark_a1(this, a1, f, name)
        class(runner), intent(inout) :: this
        procedure() :: f
        class(*), intent(in) :: a1
        character(*), optional :: name
        !private
        type(method(1)) :: mtd
        
        if (present(name)) this%name = name
        
        if (.not. allocated(root)) then
            call steps_initialize(root)
            current => root%run()
        end if
        mtd = method(f, a1)
        
        call current%add(benchmark_step(this, mtd))
        current => current%run()
    end subroutine
    
    subroutine benchmark_a2(this, a1, a2, f, name)
        class(runner), intent(inout) :: this
        procedure() :: f
        class(*), intent(in) :: a1, a2
        character(*), optional :: name
        !private
        type(method(2)) :: mtd
        
        if (present(name)) this%name = name
        
        if (.not. allocated(root)) then
            call steps_initialize(root)
            current => root%run()
        end if
        if (present(name)) this%name = name
        mtd = method(f, a1, a2)
        
        call current%add(benchmark_step(this, mtd))
        current => current%run()
    end subroutine
    
    subroutine benchmark_a3(this, a1, a2, a3, f, name)
        class(runner), intent(inout) :: this
        procedure() :: f
        class(*), intent(in) :: a1, a2, a3
        character(*), optional :: name
        !private
        type(method(3)) :: mtd
        
        if (present(name)) this%name = name
        
        if (.not. allocated(root)) then
            call steps_initialize(root)
            current => root%run()
        end if
        if (present(name)) this%name = name
        mtd = method(f, a1, a2, a3)
        
        call current%add(benchmark_step(this, mtd))
        current => current%run()
    end subroutine
    
    subroutine benchmark_a4(this, a1, a2, a3, a4, f, name)
        class(runner), intent(inout) :: this
        procedure() :: f
        class(*), intent(in) :: a1, a2, a3, a4
        character(*), optional :: name
        !private
        type(method(4)) :: mtd
        
        if (present(name)) this%name = name
        
        if (.not. allocated(root)) then
            call steps_initialize(root)
            current => root%run()
        end if
        if (present(name)) this%name = name
        mtd = method(f, a1, a2, a3, a4)
        
        call current%add(benchmark_step(this, mtd))
        current => current%run()
    end subroutine
    
    subroutine benchmark_a5(this, a1, a2, a3, a4, a5, f, name)
        class(runner), intent(inout) :: this
        procedure() :: f
        class(*), intent(in) :: a1, a2, a3, a4, a5
        character(*), optional :: name
        !private
        type(method(5)) :: mtd
        
        if (present(name)) this%name = name
        
        if (.not. allocated(root)) then
            call steps_initialize(root)
            current => root%run()
        end if
        if (present(name)) this%name = name
        mtd = method(f, a1, a2, a3, a4, a5)
        
        call current%add(benchmark_step(this, mtd))
        current => current%run()
    end subroutine
    
    subroutine benchmark_a6(this, a1, a2, a3, a4, a5, a6, f, name)
        class(runner), intent(inout) :: this
        procedure() :: f
        class(*), intent(in) :: a1, a2, a3, a4, a5, a6
        character(*), optional :: name
        !private
        type(method(6)) :: mtd
        
        if (present(name)) this%name = name
        
        if (.not. allocated(root)) then
            call steps_initialize(root)
            current => root%run()
        end if
        if (present(name)) this%name = name
        mtd = method(f, a1, a2, a3, a4, a5, a6)
        
        call current%add(benchmark_step(this, mtd))
        current => current%run()
    end subroutine
    
    subroutine benchmark_a7(this, a1, a2, a3, a4, a5, a6, a7, f, name)
        class(runner), intent(inout) :: this
        procedure() :: f
        class(*), intent(in) :: a1, a2, a3, a4, a5, a6, a7
        character(*), optional :: name
        !private
        type(method(7)) :: mtd
        
        if (present(name)) this%name = name
        
        if (.not. allocated(root)) then
            call steps_initialize(root)
            current => root%run()
        end if
        if (present(name)) this%name = name
        mtd = method(f, a1, a2, a3, a4, a5, a6, a7)
        
        call current%add(benchmark_step(this, mtd))
        current => current%run()
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
    
    subroutine finalize(this)
        type(runner), intent(inout) :: this
        
        nullify(current)
        if (allocated(root)) deallocate(root)
    end subroutine
end module
