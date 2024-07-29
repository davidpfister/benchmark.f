!> @ingroup group_all group_benchmark
!> @author davidpfister
module benchmark_library
    use, intrinsic :: iso_c_binding
    use benchmark_kinds
    use benchmark_method
    use benchmark_workflow
    use benchmark_steps
    use benchmark_options
    use benchmark_string
    use benchmark_output_unit
    use benchmark_warning
#ifdef _OPENMP
    use omp_lib
#endif

    implicit none; private
    
    type(workflow), allocatable, target     :: root
    class(workflow), pointer                :: current

    type, extends(runner_options), public   :: runner
        type(iproperty), public             :: unit
        procedure(), nopass, pointer        :: caller => null()
    contains
        procedure, pass(this), public       :: load => benchmark_load
        procedure, pass(this), public       :: save => benchmark_save
        procedure, pass(this), private      :: benchmark_serialize_to_unit
        procedure, pass(this), private      :: benchmark_serialize_to_string
        generic, public :: serialize => benchmark_serialize_to_unit, &
                                          benchmark_serialize_to_string
        procedure, pass(this), private      :: benchmark_deserialize_from_unit
        procedure, pass(this), private      :: benchmark_deserialize_from_string
        generic, public :: deserialize => benchmark_deserialize_from_unit, &
                                          benchmark_deserialize_from_string
        procedure, pass(this), public       :: set_caller
        procedure, pass(this), private      :: benchmark_void
        procedure, pass(this), private      :: benchmark_a1
        procedure, pass(this), private      :: benchmark_a2
        procedure, pass(this), private      :: benchmark_a3
        procedure, pass(this), private      :: benchmark_a4
        procedure, pass(this), private      :: benchmark_a5
        procedure, pass(this), private      :: benchmark_a6
        procedure, pass(this), private      :: benchmark_a7
        generic, public :: run => benchmark_void, &
                                  benchmark_a1, &
                                  benchmark_a2, &
                                  benchmark_a3, &
                                  benchmark_a4, &
                                  benchmark_a5, &
                                  benchmark_a6, &
                                  benchmark_a7
        procedure, pass(this), public       :: dispose
        final :: finalize
    end type

contains

    subroutine benchmark_load(this, path)
        class(runner), intent(inout)    :: this
        character(*), intent(in)        :: path
        !private
        logical :: exists
        integer :: lu, ios

        inquire(file=trim(path), exist = exists)
        if (exists) then
            open (newunit=lu, file=trim(path), status='old', action='read', delim='apostrophe', iostat = ios)
            if (ios == 0) then
                call this%deserialize(lu)
                close(lu)
            end if
        end if
    end subroutine
    
    subroutine benchmark_save(this, path)
        class(runner), intent(inout)    :: this
        character(*), intent(in)        :: path
        !private
        integer :: ios, lu

        open (newunit=lu, file=trim(path), delim='apostrophe', iostat = ios)
        if (ios == 0) then
            call this%serialize(lu)
            close(lu)
        end if
    end subroutine
    
    subroutine set_caller(this, caller)
        class(runner), intent(inout) :: this
        procedure() :: caller
        
        this%caller => caller
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
        if (associated(this%caller)) mtd%caller => this%caller
        
        call current%add(benchmarker(this, mtd))
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
        if (associated(this%caller)) mtd%caller => this%caller
        
        call current%add(benchmarker(this, mtd))
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
         if (associated(this%caller)) mtd%caller => this%caller
    
        call current%add(benchmarker(this, mtd))
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
         if (associated(this%caller)) mtd%caller => this%caller
        
        call current%add(benchmarker(this, mtd))
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
         if (associated(this%caller)) mtd%caller => this%caller
        
        call current%add(benchmarker(this, mtd))
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
         if (associated(this%caller)) mtd%caller => this%caller
        
        call current%add(benchmarker(this, mtd))
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
         if (associated(this%caller)) mtd%caller => this%caller
        
        call current%add(benchmarker(this, mtd))
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
         if (associated(this%caller)) mtd%caller => this%caller

        call current%add(benchmarker(this, mtd))
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
        type(runner_options), pointer :: bench => null()
        namelist / config / bench
            
        bench => this%runner_options
                 
        write(unit=lu, nml=config)
        nullify(bench)
    end subroutine
        
    subroutine benchmark_deserialize_from_string(this, str)
        class(runner), intent(inout)    :: this
        character(*), intent(in)        :: str
        !private
        type(runner_options) :: bench
        integer :: ierr
        namelist / config / bench
                 
        read(str, nml=config, iostat= ierr)
        this%runner_options = bench
    end subroutine
    
    subroutine benchmark_deserialize_from_unit(this, lu)
        class(runner), intent(out)   :: this
        integer, intent(in)          :: lu
        !private
        type(runner_options) :: bench
        character(len=1000) :: line
        integer :: ierr
        namelist / config / bench
                 
        read(nml=config, unit=lu, iostat=ierr)
        if (ierr /= 0) then
            backspace(lu)
            read(lu, '(A)') line
            write(*, '(A)') 'Invalid namelist at line '//trim(line)
        end if
        this%runner_options = bench
    end subroutine
    
    function parse_names(this, n, name) result(names)
        class(runner), intent(inout)        :: this
        integer, intent(in)                 :: n
        character(*), intent(in), optional  :: name
        !private
        type(string) :: names(n)
        character(:), allocatable :: func
        character(:), allocatable :: substr
        integer :: i, j, k, l
        
        if (present(name)) then
            this%name = name
        else
            l = len_trim(this%name)
            if (len_trim(this%name) == 0) return
            i = index(this%name, ',', back = .true.)
            if (i > 0) then
                k = index(this%name, ')', back = .true.)
                func = this%name(i+1:merge(k-1, l, k/=0))
            else
                i = index(this%name, '(', back = .false.)
                k = index(this%name, ')', back = .true.)
                this%name = this%name(i+1:merge(k-1, l, k/=0))
                return
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
            else
                i = index(this%name, ' ', back = .false.)
                names(1) = this%name(i+1:index(this%name, ',', back = .false.)-1)
                this%name = func
                return
            end if
        end if
    end function
    
    subroutine dispose(this)
        class(runner), intent(inout) :: this
        
        call finalize(this)
    end subroutine
    
    subroutine finalize(this)
        type(runner), intent(inout) :: this
        
        if (display_maxcall_warning) call warning_maxcalls()
        
        nullify(current)
        if (allocated(root)) deallocate(root)
    end subroutine
end module
