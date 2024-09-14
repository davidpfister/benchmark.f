!> @defgroup group_benchmark_library benchmark_library
!> @brief Benchmark library module
module benchmark_library
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

    !> @class runner
    !! @ingroup group_benchmark_library
    !> @brief Provides properties and instance methods for the execution 
    !!        a benchmarking run. The @ref runner class extends the base
    !!        class @link benchmark_options::runner_options runner_options @endlink.
    !! <h2>Examples</h2>
    !! The following examples demonstrate some of the main members of the @ref runner. 
    !! @n
    !! The first example shows a simple benchmark of the methods `foo` and `bar`
    !! passing two Dummy arguments. This example make use of the preprocessing macros
    !! found in the include file `benchmark.inc`. 
    !! @n
    !! @snippet snippet.f90 benchmark_ex1
    !! @n
    !! The second example demonstrates how the benchmarking options can be 
    !! serialized to disk and loaded for later use. The options are saved 
    !! as namelist on disk. 
    !! @n
    !! @snippet snippet.f90 benchmark_ex2
    !! <h2>Remarks</h2>
    !! @note This class is best used together with the include file `benchmark.inc`. 
    type, extends(runner_options), public   :: runner
        private
        type(output), public                :: unit !< Output unit. By default the standard output is used.
        procedure(), nopass, pointer        :: caller => null() !< Caller wrapper procedure
    contains
        private
        procedure, pass(this), private      :: benchmark_serialize_to_unit
        procedure, pass(this), private      :: benchmark_deserialize_from_unit
!> @cond
#ifdef __INTEL_COMPILER
        procedure, pass(this), private      :: benchmark_serialize_to_string
        generic, private :: serialize => benchmark_serialize_to_unit, &
                                        benchmark_serialize_to_string
#else
        generic, private :: serialize => benchmark_serialize_to_unit
#endif
        
#ifdef __INTEL_COMPILER
        procedure, pass(this), private      :: benchmark_deserialize_from_string
        generic, private :: deserialize => benchmark_deserialize_from_unit, &
                                          benchmark_deserialize_from_string
#else
        generic, private :: deserialize => benchmark_deserialize_from_unit
#endif
!> @endcond
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
        procedure, pass(this), public       :: write => benchmark_write
        procedure, pass(this), public       :: read => benchmark_read 
        procedure, pass(this), public       :: dispose
        final :: finalize
    end type

contains

    !> @brief Read runner options from namelist
    !! 
    !! @b Example
    !!
    !! @code{.f90}
    !! use benchmark_kinds
    !! use benchmark_library
    !!    
    !! type(runner) :: br
    !!
    !! call br2%read('benchmark.nml')
    !! @endcode
    !!
    !! @b Remarks
    subroutine benchmark_read(this, path)
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
    
    !> @brief Write runner options to namelist
    !! 
    !! @b Example
    !!
    !! @code{.f90}
    !! use benchmark_kinds
    !! use benchmark_library
    !!    
    !! type(runner) :: br
    !! integer :: lu
    !!
    !! br%maxcalls = 10
    !! br%csv_unit = -10
    !! br%mintime = 50_r8
    !! br%maxtime = 100_r8
    !! br%offset = 20.0_r8
    !! br%sampling_window = 10
    !! br%ssd_threshold = 0.025_r8
    !!
    !! call br%write('benchmark.nml')
    !! @endcode
    !!
    !! The output file is a namelist. It looks as follows
    !! ```
    !! &CONFIG
    !!  BENCH%MAXCALLS=10         ,
    !!  BENCH%CSV_UNIT=-10        ,
    !!  BENCH%MINTIME=  50.000000000000000     ,
    !!  BENCH%MAXTIME=  100.00000000000000     ,
    !!  BENCH%OFFSET=  20.000000000000000     ,
    !!  BENCH%SAMPLING_WINDOW=10         ,
    !!  BENCH%SSD_THRESHOLD=  2.5000000000000001E-002,
    !!  BENCH%SKIP_PRELUDE=F,
    !!  BENCH%COUNT=0          ,
    !!  BENCH%NAME=',
    !!  /
    !! ```
    !!
    !! @b Remarks
    subroutine benchmark_write(this, path)
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
    
    !> @brief Set the function caller to wrap function call.
    !! 
    !! @param[in] this The type bound to the method
    !! @param caller Function wrapper
    !!
    !! @b Examples
    !! The following example demonstrate how to use the `caller` wrapper. 
    !! By doing so, one can enforce a specific interface to the procedure 
    !! argument. This is especially handy when benchmarking functions or 
    !! requiring `inout` and `out` intents
    !! ```fortran
    !! type(runner) :: br
    !!
    !! call br%set_caller(foo)
    !! ```
    !! Where `foo` is defined as follows.
    !! ```fortran
    !! subroutine foo(f, a)
    !!     procedure(foo_x) :: f
    !!     type(string), intent(in) :: a
    !!    
    !!     block
    !!         character(len(a)) :: res
    !!            
    !!         res = f(a%chars)
    !!     end block
    !! end subroutine
    !! ```
    !! With the following interface
    !! ```fortran
    !! interface
    !!     pure function foo_x(str) result(res)
    !!         character(*), intent(in)   :: str 
    !!         character(len(str)) :: res
    !!     end function
    !! end interface
    !! ```
    !! @b Remarks
    subroutine set_caller(this, caller)
        class(runner), intent(inout) :: this
        procedure() :: caller
        
        this%caller => caller
    end subroutine 
    
    !> @brief Bound procedure to run the benchmark
    !! 
    !! @param[in] this The type bound to the method
    !! @param f The procedure to benchmark
    !!
    !! @b Examples
    !! ```fortran
    !! type(runner) :: br
    !!
    !! call br%run()
    !! ```
    !! @b Remarks
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

    !> @brief Bound procedure to run the benchmark
    !! 
    !! @param[in] this The type bound to the method
    !! @param[in] a1 Dummy argument
    !! @param f The procedure to benchmark
    !!
    !! @b Examples
    !! ```fortran
    !! type(runner) :: br
    !!
    !! call br%run('a1')
    !! ```
    !! @b Remarks
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
    
    !> @brief Bound procedure to run the benchmark
    !! 
    !! @param[in] this The type bound to the method
    !! @param[in] a1 Dummy argument
    !! @param[in] a2 Dummy argument
    !! @param f The procedure to benchmark
    !!
    !! @b Examples
    !! ```fortran
    !! type(runner) :: br
    !!
    !! call br%run('a1', 'a2')
    !! ```
    !! @b Remarks
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
    
    !> @brief Bound procedure to run the benchmark
    !! 
    !! @param[in] this The type bound to the method
    !! @param[in] a1 Dummy argument
    !! @param[in] a2 Dummy argument
    !! @param[in] a3 Dummy argument
    !! @param f The procedure to benchmark
    !!
    !! @b Examples
    !! ```fortran
    !! type(runner) :: br
    !!
    !! call br%run('a1', 'a2', 'a3')
    !! ```
    !! @b Remarks
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
    
    !> @brief Bound procedure to run the benchmark
    !! 
    !! @param[in] this The type bound to the method
    !! @param[in] a1 Dummy argument
    !! @param[in] a2 Dummy argument
    !! @param[in] a3 Dummy argument
    !! @param[in] a4 Dummy argument
    !! @param f The procedure to benchmark
    !!
    !! @b Examples
    !! ```fortran
    !! type(runner) :: br
    !!
    !! call br%run('a1', 'a2', 'a3', 'a4')
    !! ```
    !! @b Remarks
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
    
    !> @brief Bound procedure to run the benchmark
    !! 
    !! @param[in] this The type bound to the method
    !! @param[in] a1 Dummy argument
    !! @param[in] a2 Dummy argument
    !! @param[in] a3 Dummy argument
    !! @param[in] a4 Dummy argument
    !! @param[in] a5 Dummy argument
    !! @param f The procedure to benchmark
    !!
    !! @b Examples
    !! ```fortran
    !! type(runner) :: br
    !!
    !! call br%run('a1', 'a2', 'a3', 'a4', 'a5')
    !! ```
    !! @b Remarks
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
    
    !> @brief Bound procedure to run the benchmark
    !! 
    !! @param[in] this The type bound to the method
    !! @param[in] a1 Dummy argument
    !! @param[in] a2 Dummy argument
    !! @param[in] a3 Dummy argument
    !! @param[in] a4 Dummy argument
    !! @param[in] a5 Dummy argument
    !! @param[in] a6 Dummy argument
    !! @param f The procedure to benchmark
    !!
    !! @b Examples
    !! ```fortran
    !! type(runner) :: br
    !!
    !! call br%run('a1', 'a2', 'a3', 'a4', 'a5', 'a6')
    !! ```
    !! @b Remarks
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
    
    !> @brief Bound procedure to run the benchmark
    !! 
    !! @param[in] this The type bound to the method
    !! @param[in] a1 Dummy argument
    !! @param[in] a2 Dummy argument
    !! @param[in] a3 Dummy argument
    !! @param[in] a4 Dummy argument
    !! @param[in] a5 Dummy argument
    !! @param[in] a6 Dummy argument
    !! @param[in] a7 Dummy argument
    !! @param f The procedure to benchmark
    !!
    !! @b Examples
    !! ```fortran
    !! type(runner) :: br
    !!
    !! call br%run('a1', 'a2', 'a3', 'a4', 'a5', 'a6', 'a7')
    !! ```
    !!
    !! @b Remarks
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

#ifdef __INTEL_COMPILER
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
#endif
   
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

#ifdef __INTEL_COMPILER
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
#endif

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
    
    !> @private
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
    
    !> @brief Dispose resources associated with 
    !!        The type bound to the method.
    !! @param[inout] this The type bound to the method
    !!
    !! @b Remarks
    subroutine dispose(this)
        class(runner), intent(inout) :: this
        
        call finalize(this)
    end subroutine
    
    !> @private
    subroutine finalize(this)
        type(runner), intent(inout) :: this
        
        if (display_maxcall_warning) call warning_maxcalls()
        this%unit = stdout
        nullify(current)
        if (allocated(root)) deallocate(root)
    end subroutine
end module
