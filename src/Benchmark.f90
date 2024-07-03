module benchmark_library
    use, intrinsic :: iso_c_binding
    use benchmark_kinds
    use benchmark_method
    use benchmark_workflow
    use benchmark_steps
    use benchmark_options
    use benchmark_string
#ifdef _OPENMP
    use omp_lib
#endif

    implicit none

    private
    
    type(workflow), allocatable, target  :: root
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

    subroutine benchmark_void(this, f)
        class(runner), intent(inout)        :: this
        procedure()                         :: f
        !private
        type(method) :: mtd
        type(string) :: names(0)
        
        names = parse_names(this, size(names))
        
        if (.not. allocated(root)) then
            call steps_initialize(root, this)
            current => root%run()
        end if
        
        mtd = method(f)
        
        call current%add(benchmark_run(this, mtd))
        current => current%run()
        
    end subroutine

    subroutine benchmark_a1(this, a1, f)
        class(runner), intent(inout)        :: this
        class(*), intent(in)                :: a1
        procedure()                         :: f
        !private
        type(method) :: mtd
        type(string) :: names(1)
        
        names = parse_names(this, size(names))
        
        if (.not. allocated(root)) then
            call steps_initialize(root, this)
            current => root%run()
        end if
        mtd = method(f, arg(a1, names(1)))
        
        call current%add(benchmark_run(this, mtd))
        current => current%run()
    end subroutine
    
    subroutine benchmark_a2(this, a1, a2, f)
        class(runner), intent(inout)        :: this
        class(*), intent(in)                :: a1, a2
        procedure()                         :: f
        !private
        type(method) :: mtd
        type(string) :: names(2)

        names = parse_names(this, size(names))
        
        if (.not. allocated(root)) then
            call steps_initialize(root, this)
            current => root%run()
        end if

        mtd = method(f, arg(a1, names(1)), arg(a2, names(2)))
    
        call current%add(benchmark_run(this, mtd))
        current => current%run()
    end subroutine
    
    subroutine benchmark_a3(this, a1, a2, a3, f)
        class(runner), intent(inout)        :: this
        class(*), intent(in)                :: a1, a2, a3
        procedure()                         :: f  
        !private
        type(method) :: mtd
        type(string) :: names(3)
        
        names = parse_names(this, size(names))
        
        if (.not. allocated(root)) then
            call steps_initialize(root, this)
            current => root%run()
        end if

        mtd = method(f, arg(a1, names(1)), arg(a2, names(2)), arg(a3, names(3)))
        
        call current%add(benchmark_run(this, mtd))
        current => current%run()
    end subroutine
    
    subroutine benchmark_a4(this, a1, a2, a3, a4, f)
        class(runner), intent(inout)        :: this
        class(*), intent(in)                :: a1, a2, a3, a4
        procedure()                         :: f
        !private
        type(method) :: mtd
        type(string) :: names(4)
        
        names = parse_names(this, size(names))
        
        if (.not. allocated(root)) then
            call steps_initialize(root, this)
            current => root%run()
        end if

        mtd = method(f, arg(a1, names(1)), arg(a2, names(2)), arg(a3, names(3)), &
                        arg(a4, names(4)))
        
        call current%add(benchmark_run(this, mtd))
        current => current%run()
    end subroutine
    
    subroutine benchmark_a5(this, a1, a2, a3, a4, a5, f)
        class(runner), intent(inout)        :: this
        class(*), intent(in)                :: a1, a2, a3, a4, a5
        procedure()                         :: f
        !private
        type(method) :: mtd
        type(string) :: names(5)
        
        names = parse_names(this, size(names))
        
        if (.not. allocated(root)) then
            call steps_initialize(root, this)
            current => root%run()
        end if

        mtd = method(f, arg(a1, names(1)), arg(a2, names(2)), arg(a3, names(3)), &
                        arg(a4, names(4)), arg(a5, names(5)))
        
        call current%add(benchmark_run(this, mtd))
        current => current%run()
    end subroutine
    
    subroutine benchmark_a6(this, a1, a2, a3, a4, a5, a6, f)
        class(runner), intent(inout)        :: this
        class(*), intent(in)                :: a1, a2, a3, a4, a5, a6
        procedure()                         :: f
        !private
        type(method) :: mtd
        type(string) :: names(6)
        
        names = parse_names(this, size(names))
        
        if (.not. allocated(root)) then
            call steps_initialize(root, this)
            current => root%run()
        end if

        mtd = method(f, arg(a1, names(1)), arg(a2, names(2)), arg(a3, names(3)), &
                        arg(a4, names(4)), arg(a5, names(5)), arg(a6, names(6)))
        
        call current%add(benchmark_run(this, mtd))
        current => current%run()
    end subroutine
    
    subroutine benchmark_a7(this, a1, a2, a3, a4, a5, a6, a7, f)
        class(runner), intent(inout)        :: this
        class(*), intent(in)                :: a1, a2, a3, a4, a5, a6, a7
        procedure()                         :: f
        !private
        type(method) :: mtd
        type(string) :: names(7)
        
        names = parse_names(this, size(names))
        
        if (.not. allocated(root)) then
            call steps_initialize(root, this)
            current => root%run()
        end if

        mtd = method(f, arg(a1, names(1)), arg(a2, names(2)), arg(a3, names(3)), &
                        arg(a4, names(4)), arg(a5, names(5)), arg(a6, names(6)), arg(a7, names(7)))
        
        call current%add(benchmark_run(this, mtd))
        current => current%run()
    end subroutine

    subroutine benchmark_serialize_to_string(this, str)
        class(runner), intent(in), target       :: this
        character(:), allocatable, intent(out)  :: str
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
        integer, intent(in)                 :: lu
        !private
        type(runner), pointer :: bench => null()
        namelist / config / bench
            
        bench => this
                 
        write(unit=lu, nml=config)
        nullify(bench)
    end subroutine
        
    subroutine benchmark_deserialize_from_string(that, str)
        type(runner), intent(inout)     :: that
        character(*), intent(in)        :: str
        !private
        type(runner) :: bench
        namelist / config / bench
                 
        read(str, nml=config)
        that = bench
    end subroutine
    
    subroutine benchmark_deserialize_from_unit(that, lu)
        type(runner), intent(inout) :: that
        integer, intent(in)         :: lu
        !private
        type(runner) :: bench
        namelist / config / bench
                 
        read(nml=config, unit=lu)
        that = bench
    end subroutine
    
    function parse_names(this, n, name) result(names)
        class(runner), intent(inout)        :: this
        integer, intent(in)                 :: n
        character(*), intent(in), optional  :: name
        !private
        type(string) :: names(n)
        character(:), allocatable :: func
        character(:), allocatable :: substr
        integer :: i, j
        
        if (present(name)) then
            this%name = name
        else
            if (len_trim(this%name) == 0) return
            i = index(this%name, ',', back = .true.)
            if (i > 0) then
                func = this%name(i+1: len_trim(this%name)-1)
            else
                i = index(this%name, '(', back = .false.)
                if (i > 0) then
                    this%name = this%name(i+1: len_trim(this%name)-1)
                    return
                end if
            end if
            j = n
            do while (i > 0)
                substr = this%name(:i-1)
                i = index(substr, ',', back = .true.)
                if (i > 0)  then
                    names(j) = substr(i+1:)
                    j = j - 1
                else
                    exit
                end if
            end do
            i = index(this%name, '(', back = .false.)
            if (i > 0) then
                names(1) = this%name(i+1:index(this%name, ',', back = .false.)-1)
                this%name = func
                return
            end if
        end if
    end function
    
    subroutine finalize(this)
        type(runner), intent(inout) :: this
        
        nullify(current)
        if (allocated(root)) deallocate(root)
    end subroutine
end module
