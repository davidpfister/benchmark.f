
!> @defgroup group_method benchmark_method
!> @brief Method module
!> @cond
#include <concat.inc>
#define ADD_ARGUMENT(n) \
that%args(n) = MACRO_CAT2(a,n)
!> @endcond
module benchmark_method
    use iso_c_binding
    use benchmark_kinds
    use benchmark_argument
    
    implicit none
    
    public :: method, arg
    
    private

    !> @class method
    !! @ingroup group_method
    !! @brief Provides properties and instance methods for the @ref method class. 
    !!        This class is a wrapper for any procedure (subroutine or function).
    !!
    !! @note The procedures are stored a generic compunds `procedure`. The explicit
    !!       interface can be enforced through the use of the @link benchmark_method::method::caller caller @endlink
    !!       property.
    !!
    !! <h2>Examples</h2>
    !! The following examples demonstrate some of the main members of the @ref method. 
    !! @n
    !! This example shows how to set a `caller` explicitely. In the present 
    !! case, the caller was necessary to specify the procedure interface 
    !! explicitely. This is necessary when benchmarking a function rather 
    !! than a subroutine.
    !! @n
    !! @snippet snippet.f90 method_caller
    !! <h2>Remarks</h2>
    !! At the moment, the @ref method class handles procedures using up to 7 dummy arguments.
    !! <h2>Constructors</h2>
    !! Initializes a new instance of the @ref method class
    !!
    !! <h3>method(integer, procedure)</h3>
    !! @verbatim type(method) function method(integer nargs, procedure f)@endverbatim
    !! 
    !! @param[in] nargs The number of dummy arguments
    !! @param f The procedure
    !! 
    !! @b Examples
    !! @code{.f90}
    !! type(method) :: m
    !!  m = method(2, foo)
    !! @endcode
    !!
    !! <h3>method(procedure, procedure)</h3>
    !! @verbatim type(method) function method(procedure f, procedure caller)@endverbatim
    !! 
    !! @param f The procedure pointer
    !! @param caller The caller wrapper
    !! 
    !! @b Examples
    !! @code{.f90}
    !! type(method) :: m
    !!  m = method(foo)
    !! @endcode
    !!
    !! <h3>method(procedure, class(*), procedure)</h3>
    !! @verbatim type(method) function method(procedure f, class(*) a1, procedure caller)@endverbatim
    !! 
    !! @param f The procedure pointer
    !! @param[in] a1 The method argument
    !! @param caller (optional) The caller wrapper
    !! 
    !! @b Examples
    !! @code{.f90}
    !! type(method) :: m
    !!  m = method(foo, arg(1, 'a1'))
    !! @endcode
    !!
    !! <h3>method(procedure, class(*), class(*), procedure)</h3>
    !! @verbatim type(method) function method(procedure f, class(*) a1, class(*) a2, procedure caller)@endverbatim
    !! 
    !! @param f The procedure pointer
    !! @param[in] a1 The method argument
    !! @param[in] a2 The method argument
    !! @param caller (optional) The caller wrapper
    !!
    !! <h3>method(procedure, class(*), class(*), class(*), procedure)</h3>
    !! @verbatim type(method) function method(procedure f, class(*) a1, class(*) a2, class(*) a3, procedure caller)@endverbatim
    !! 
    !! @param f The procedure pointer
    !! @param[in] a1 The method argument
    !! @param[in] a2 The method argument
    !! @param[in] a3 The method argument
    !! @param caller (optional) The caller wrapper
    !!
    !! <h3>method(procedure, class(*), class(*), class(*), class(*), procedure)</h3>
    !! @verbatim type(method) function method(procedure f, class(*) a1, class(*) a2, class(*) a3, class(*) a4, procedure caller)@endverbatim
    !! 
    !! @param f The procedure pointer
    !! @param[in] a1 The method argument
    !! @param[in] a2 The method argument
    !! @param[in] a3 The method argument
    !! @param[in] a4 The method argument
    !! @param caller (optional) The caller wrapper
    !!
    !! <h3>method(procedure, class(*), class(*), class(*), class(*), class(*), procedure)</h3>
    !! @verbatim type(method) function method(procedure f, class(*) a1, class(*) a2, class(*) a3, class(*) a4, class(*) a5, procedure caller)@endverbatim
    !! 
    !! @param f The procedure pointer
    !! @param[in] a1 The method argument
    !! @param[in] a2 The method argument
    !! @param[in] a3 The method argument
    !! @param[in] a4 The method argument
    !! @param[in] a5 The method argument
    !! @param caller (optional) The caller wrapper
    !!
    !! <h3>method(procedure, class(*), class(*), class(*), class(*), class(*), class(*), procedure)</h3>
    !! @verbatim type(method) function method(procedure f, class(*) a1, class(*) a2, class(*) a3, class(*) a4, class(*) a5, class(*) a6, procedure caller)@endverbatim
    !! 
    !! @param f The procedure pointer
    !! @param[in] a1 The method argument
    !! @param[in] a2 The method argument
    !! @param[in] a3 The method argument
    !! @param[in] a4 The method argument
    !! @param[in] a5 The method argument
    !! @param[in] a6 The method argument
    !! @param caller (optional) The caller wrapper
    !!
    !! <h3>method(procedure, class(*), class(*), class(*), class(*), class(*), class(*), class(*), procedure)</h3>
    !! @verbatim type(method) function method(procedure f, class(*) a1, class(*) a2, class(*) a3, class(*) a4, class(*) a5, class(*) a6, class(*) a7, procedure caller)@endverbatim
    !! 
    !! @param f The procedure pointer
    !! @param[in] a1 The method argument
    !! @param[in] a2 The method argument
    !! @param[in] a3 The method argument
    !! @param[in] a4 The method argument
    !! @param[in] a5 The method argument
    !! @param[in] a6 The method argument
    !! @param[in] a7 The method argument
    !! @param caller (optional) The caller wrapper
    !!
    !! @b Remarks
    type :: method
        private
        integer, public                         :: nargs !< Number of dummy arguments
        procedure(), nopass, pointer            :: f => null() !< Procedure pointer
        procedure(), nopass, pointer, public    :: caller => null() !< Wrapper to call the procedure pointer. This provides a way to set the procedure interface explicitely.
        type(arg), allocatable, public          :: args(:) !< Array of method @link benchmark_argument::arg arguments @endlink
    contains
        procedure, pass(this), private :: invoke_a0
        procedure, pass(this), private :: invoke_a1
        procedure, pass(this), private :: invoke_a2
        procedure, pass(this), private :: invoke_a3
        procedure, pass(this), private :: invoke_a4
        procedure, pass(this), private :: invoke_a5
        procedure, pass(this), private :: invoke_a6
        procedure, pass(this), private :: invoke_a7
        generic, public :: invoke => invoke_a0, &
                                     invoke_a1, &
                                     invoke_a2, &
                                     invoke_a3, &
                                     invoke_a4, &
                                     invoke_a5, &
                                     invoke_a6, &
                                     invoke_a7
        procedure, pass(lhs), private :: method_assign_method
        generic, public :: assignment(=) => method_assign_method
        procedure, pass(this), public :: dispose
        final :: finalize
    end type
    
    interface method
    !> @cond
        module procedure :: method_create, &
                            method_create_0, &
                            method_create_1, &
                            method_create_2, &
                            method_create_3, &
                            method_create_4, &
                            method_create_5, &
                            method_create_6, &
                            method_create_7
    !> @endcond
    end interface
 
    contains

    function method_create(nargs, f) result(that)
        integer, intent(in)             :: nargs
        procedure()                     :: f
        type(method)                    :: that        
        
        that%f => f
        that%nargs = nargs
        allocate(that%args(nargs))
    end function
    
    function method_create_0(f, caller) result(that)
        procedure()                     :: f
        procedure(), optional           :: caller
        type(method)                    :: that        
        
        that%f => f
        that%nargs = 0
        allocate(that%args(0))
        if (present(caller)) then
            that%caller => caller
        end if
    end function
    
    function method_create_1(f, a1, caller) result(that)
        procedure()                     :: f
        class(*), intent(in)            :: a1
        procedure(), optional           :: caller
        type(method)                    :: that
        
        that%f => f
        that%nargs = 1
        allocate(that%args(1))
        if (present(caller)) then
            that%caller => caller
        end if
        ADD_ARGUMENT(1)
    end function
    
    function method_create_2(f, a1, a2, caller) result(that)
        procedure()                     :: f
        class(*), intent(in)            :: a1, a2
        procedure(), optional           :: caller
        type(method)                    :: that
        
        that%f => f
        that%nargs = 2
        allocate(that%args(2))
        if (present(caller)) then
            that%caller => caller
        end if
        ADD_ARGUMENT(1)
        ADD_ARGUMENT(2)
    end function
    
    function method_create_3(f, a1, a2, a3, caller) result(that)
        procedure()                     :: f
        class(*), intent(in)            :: a1, a2, a3
        procedure(), optional           :: caller
        type(method)                    :: that
        
        that%f => f
        that%nargs = 3
        allocate(that%args(3))
        if (present(caller)) then
            that%caller => caller
        end if
        ADD_ARGUMENT(1)
        ADD_ARGUMENT(2)
        ADD_ARGUMENT(3)
    end function
    
    function method_create_4(f, a1, a2, a3, a4, caller) result(that)
        procedure()                     :: f
        class(*), intent(in)            :: a1, a2, a3, a4
        procedure(), optional           :: caller
        type(method)                    :: that
        
        that%f => f
        that%nargs = 4
        allocate(that%args(4))
        if (present(caller)) then
            that%caller => caller
        end if
        ADD_ARGUMENT(1)
        ADD_ARGUMENT(2)
        ADD_ARGUMENT(3)
        ADD_ARGUMENT(4)
    end function
    
    function method_create_5(f, a1, a2, a3, a4, a5, caller) result(that)
        procedure()                     :: f
        class(*), intent(in)            :: a1, a2, a3, a4, a5
        procedure(), optional           :: caller
        type(method)                    :: that
        
        that%f => f
        that%nargs = 5
        allocate(that%args(5))
        if (present(caller)) then
            that%caller => caller
        end if
        ADD_ARGUMENT(1)
        ADD_ARGUMENT(2)
        ADD_ARGUMENT(3)
        ADD_ARGUMENT(4)
        ADD_ARGUMENT(5)
    end function
    
    function method_create_6(f, a1, a2, a3, a4, a5, a6, caller) result(that)
        procedure()                     :: f
        class(*), intent(in)            :: a1, a2, a3, a4, a5, a6
        procedure(), optional           :: caller
        type(method)                    :: that
        
        that%f => f
        that%nargs = 6
        allocate(that%args(6))
        if (present(caller)) then
            that%caller => caller
        end if
        ADD_ARGUMENT(1)
        ADD_ARGUMENT(2)
        ADD_ARGUMENT(3)
        ADD_ARGUMENT(4)
        ADD_ARGUMENT(5)
        ADD_ARGUMENT(6)
    end function
    
    function method_create_7(f, a1, a2, a3, a4, a5, a6, a7, caller) result(that)
        procedure()                     :: f
        class(*), intent(in)            :: a1, a2, a3, a4, a5, a6, a7
        procedure(), optional           :: caller
        type(method)                    :: that
        
        that%f => f
        that%nargs = 7
        allocate(that%args(7))
        if (present(caller)) then
            that%caller => caller
        end if
        ADD_ARGUMENT(1)
        ADD_ARGUMENT(2)
        ADD_ARGUMENT(3)
        ADD_ARGUMENT(4)
        ADD_ARGUMENT(5)
        ADD_ARGUMENT(6)
        ADD_ARGUMENT(7)
    end function

    !> @brief Bound procedure to invoke a method
    !! 
    !! @param[in] this The type bound to the method
    !!
    !! @b Examples
    !! @code{.f90}
    !! type(method) :: mtd
    !!
    !! mtd = method(f)
    !! call mtd%invoke()
    !! @endcode
    !! @b Remarks
    !! 
    !! @note This function is either used with a 0-arguments
    !! method, or with any method whose arguments have been set in the 
    !! constructor.
    !!
    !! @b Remarks
    subroutine invoke_a0(this)
        class(method), intent(inout)    :: this
        select case (this%nargs)
        case (0)
            if (associated(this%caller)) then
                call this%caller(this%f)
            else
                call this%f()
            end if
        case (1)
            if (.not. allocated(this%args(1)%value)) stop -1
            associate(arg1 => this%args(1)%value)
                if (associated(this%caller)) then
                    call this%caller(this%f, arg1)
                else
                    call this%f(arg1)
                end if
            end associate
        case (2)
            if (.not. (allocated(this%args(1)%value) .and. &
                       allocated(this%args(2)%value))) stop -1
            associate(arg1 => this%args(1)%value, &
                      arg2 => this%args(2)%value)
                if (associated(this%caller)) then
                    call this%caller(this%f, arg1, arg2)
                else
                    call this%f(arg1, arg2)
                end if
            end associate
        case (3)
            if (.not. (allocated(this%args(1)%value) .and. &
                       allocated(this%args(2)%value) .and. &
                       allocated(this%args(3)%value))) stop -1
            associate(arg1 => this%args(1)%value, &
                      arg2 => this%args(2)%value, &
                      arg3 => this%args(3)%value)
                if (associated(this%caller)) then
                    call this%caller(this%f, arg1, arg2, arg3)
                else
                    call this%f(arg1, arg2, arg3)
                end if
            end associate
        case (4)
            if (.not. (allocated(this%args(1)%value) .and. &
                       allocated(this%args(2)%value) .and. &
                       allocated(this%args(3)%value) .and. &
                       allocated(this%args(4)%value))) stop -1
            associate(arg1 => this%args(1)%value, &
                      arg2 => this%args(2)%value, &
                      arg3 => this%args(3)%value, &
                      arg4 => this%args(4)%value)
                if (associated(this%caller)) then
                    call this%caller(this%f, arg1, arg2, arg3, arg4)
                else
                    call this%f(arg1, arg2, arg3, arg4)
                end if
            end associate
        case (5)
            if (.not. (allocated(this%args(1)%value) .and. &
                       allocated(this%args(2)%value) .and. &
                       allocated(this%args(3)%value) .and. &
                       allocated(this%args(4)%value) .and. &
                       allocated(this%args(5)%value))) stop -1
            associate(arg1 => this%args(1)%value, &
                      arg2 => this%args(2)%value, &
                      arg3 => this%args(3)%value, &
                      arg4 => this%args(4)%value, &
                      arg5 => this%args(5)%value)
                if (associated(this%caller)) then
                    call this%caller(this%f, arg1, arg2, arg3, arg4, arg5)
                else
                    call this%f(arg1, arg2, arg3, arg4, arg5)
                end if
            end associate
        case (6)
            if (.not. (allocated(this%args(1)%value) .and. &
                       allocated(this%args(2)%value) .and. &
                       allocated(this%args(3)%value) .and. &
                       allocated(this%args(4)%value) .and. &
                       allocated(this%args(5)%value) .and. &
                       allocated(this%args(6)%value))) stop -1
            associate(arg1 => this%args(1)%value, &
                      arg2 => this%args(2)%value, &
                      arg3 => this%args(3)%value, &
                      arg4 => this%args(4)%value, &
                      arg5 => this%args(5)%value, &
                      arg6 => this%args(6)%value)
                if (associated(this%caller)) then
                    call this%caller(this%f, arg1, arg2, arg3, arg4, arg5, arg6)
                else
                    call this%f(arg1, arg2, arg3, arg4, arg5, arg6)
                end if
            end associate
        case (7)
            if (.not. (allocated(this%args(1)%value) .and. &
                       allocated(this%args(2)%value) .and. &
                       allocated(this%args(3)%value) .and. &
                       allocated(this%args(4)%value) .and. &
                       allocated(this%args(5)%value) .and. &
                       allocated(this%args(6)%value) .and. &
                       allocated(this%args(7)%value))) stop -1
            associate(arg1 => this%args(1)%value, &
                      arg2 => this%args(2)%value, &
                      arg3 => this%args(3)%value, &
                      arg4 => this%args(4)%value, &
                      arg5 => this%args(5)%value, &
                      arg6 => this%args(6)%value, &
                      arg7 => this%args(7)%value)
                if (associated(this%caller)) then
                    call this%caller(this%f, arg1, arg2, arg3, arg4, arg5, arg6, arg7)
                else
                    call this%f(arg1, arg2, arg3, arg4, arg5, arg6, arg7)
                end if
            end associate
        case default
            stop -1
        end select
    end subroutine

    !> @brief Bound procedure to invoke a method
    !! 
    !! @param[in] this The type bound to the method
    !! @param[in] a1 dummy argument
    !!
    !! @b Examples
    !!
    !! The first example demonstrate how to invoke the method 
    !! by passing the arguments to the `invoke` procedure.
    !! @code{.f90}
    !! type(method) :: mtd
    !!
    !! mtd = method(1, f)
    !! call mtd%invoke(arg(a1, 'a1'))
    !! @endcode
    !! Alternatively, on can also set the arguments directly from 
    !! the method constructor.
    !! @code{.f90}
    !! type(method) :: mtd
    !!
    !! mtd = method(f, arg(a1, 'a1'))
    !! call mtd%invoke()
    !! @endcode
    !!
    !! @b Remarks
    subroutine invoke_a1(this, a1)
        class(method), intent(inout)    :: this
        class(*), intent(in)            :: a1

        if (this%nargs /= 1) stop -1
        
        this%args(1) = a1
        associate(arg1 => a1)
            if (associated(this%caller)) then
                call this%caller(this%f, arg1)
            else
                call this%f(arg1)
            end if
        end associate
    end subroutine
    
    !> @brief Bound procedure to invoke a method
    !! 
    !! @param[in] this The type bound to the method
    !! @param[in] a1 dummy argument
    !! @param[in] a2 dummy argument
    !!
    !! @b Examples
    !!
    !! The first example demonstrate how to invoke the method 
    !! by passing the arguments to the `invoke` procedure.
    !! @code{.f90}
    !! type(method) :: mtd
    !!
    !! mtd = method(2, f)
    !! call mtd%invoke(arg(a1, 'a1'), arg(a2, 'a2'))
    !! @endcode
    !! Alternatively, on can also set the arguments directly from 
    !! the method constructor.
    !! @code{.f90}
    !! type(method) :: mtd
    !!
    !! mtd = method(f, arg(a1, 'a1'), arg(a2, 'a2'))
    !! call mtd%invoke()
    !! @endcode
    !!
    !! @b Remarks
    subroutine invoke_a2(this, a1, a2)
        class(method), intent(inout)    :: this
        class(*), intent(in)            :: a1, a2

        if (this%nargs /= 2) stop -1
        
        this%args(1) = a1
        this%args(2) = a2
        associate(arg1 => a1, arg2 => a2)
            if (associated(this%caller)) then
                call this%caller(this%f, arg1, arg2)
            else
                call this%f(arg1, arg2)
            end if
        end associate
    end subroutine
    
    !> @brief Bound procedure to invoke a method
    !! 
    !! @param[in] this The type bound to the method
    !! @param[in] a1 dummy argument
    !! @param[in] a2 dummy argument
    !! @param[in] a3 dummy argument
    !!
    !! @b Examples
    !!
    !! The first example demonstrate how to invoke the method 
    !! by passing the arguments to the `invoke` procedure.
    !! @code{.f90}
    !! type(method) :: mtd
    !!
    !! mtd = method(3, f)
    !! call mtd%invoke(arg(a1, 'a1'), arg(a2, 'a2'), arg(a3, 'a3'))
    !! @endcode
    !! Alternatively, on can also set the arguments directly from 
    !! the method constructor.
    !! @code{.f90}
    !! type(method) :: mtd
    !!
    !! mtd = method(f, arg(a1, 'a1'), arg(a2, 'a2'), arg(a3, 'a3'))
    !! call mtd%invoke()
    !! @endcode
    !!
    !! @b Remarks
    subroutine invoke_a3(this, a1, a2, a3)
        class(method), intent(inout)    :: this
        class(*), intent(in)            :: a1, a2, a3

        if (this%nargs /= 3) stop -1
        
        this%args(1) = a1
        this%args(2) = a2
        this%args(3) = a3
        associate(arg1 => a1, &
                  arg2 => a2, &
                  arg3 => a3)
            if (associated(this%caller)) then
                call this%caller(this%f, arg1, arg2, arg3)
            else
                call this%f(arg1, arg2, arg3)
            end if
        end associate
    end subroutine
    
    !> @brief Bound procedure to invoke a method
    !! 
    !! @param[in] this The type bound to the method
    !! @param[in] a1 dummy argument
    !! @param[in] a2 dummy argument
    !! @param[in] a3 dummy argument
    !! @param[in] a4 dummy argument
    !!
    !! @b Examples
    !!
    !! The first example demonstrate how to invoke the method 
    !! by passing the arguments to the `invoke` procedure.
    !! @code{.f90}
    !! type(method) :: mtd
    !!
    !! mtd = method(4, f)
    !! call mtd%invoke(arg(a1, 'a1'), arg(a2, 'a2'), arg(a3, 'a3'), &
    !!                 arg(a4, 'a4'))
    !! @endcode
    !! Alternatively, on can also set the arguments directly from 
    !! the method constructor.
    !! @code{.f90}
    !! type(method) :: mtd
    !!
    !! mtd = method(f, arg(a1, 'a1'), arg(a2, 'a2'), arg(a3, 'a3'), &
    !!                 arg(a4, 'a4'))
    !! call mtd%invoke()
    !! @endcode
    !!
    !! @b Remarks
    subroutine invoke_a4(this, a1, a2, a3, a4)
        class(method), intent(inout)    :: this
        class(*), intent(in)            :: a1, a2, a3, a4

        if (this%nargs /= 4) stop -1
        
        this%args(1) = a1
        this%args(2) = a2
        this%args(3) = a3
        this%args(4) = a4
        associate(arg1 => a1, &
                  arg2 => a2, &
                  arg3 => a3, &
                  arg4 => a4)
            if (associated(this%caller)) then
                call this%caller(this%f, arg1, arg2, arg3, arg4)
            else
                call this%f(arg1, arg2, arg3, arg4)
            end if
        end associate
    end subroutine
    
    !> @brief Bound procedure to invoke a method
    !! 
    !! @param[in] this The type bound to the method
    !! @param[in] a1 dummy argument
    !! @param[in] a2 dummy argument
    !! @param[in] a3 dummy argument
    !! @param[in] a4 dummy argument
    !! @param[in] a5 dummy argument
    !!
    !! @b Examples
    !!
    !! The first example demonstrate how to invoke the method 
    !! by passing the arguments to the `invoke` procedure.
    !! @code{.f90}
    !! type(method) :: mtd
    !!
    !! mtd = method(5, f)
    !! call mtd%invoke(arg(a1, 'a1'), arg(a2, 'a2'), arg(a3, 'a3'), &
    !!                 arg(a4, 'a4'), arg(a5, 'a5'))
    !! @endcode
    !! Alternatively, on can also set the arguments directly from 
    !! the method constructor.
    !! @code{.f90}
    !! type(method) :: mtd
    !!
    !! mtd = method(f, arg(a1, 'a1'), arg(a2, 'a2'), arg(a3, 'a3'), &
    !!                 arg(a4, 'a4'), arg(a5, 'a5'))
    !! call mtd%invoke()
    !! @endcode
    !!
    !! @b Remarks
    subroutine invoke_a5(this, a1, a2, a3, a4, a5)
        class(method), intent(inout)    :: this
        class(*), intent(in)            :: a1, a2, a3, a4, a5

        if (this%nargs /= 5) stop -1
        
        this%args(1) = a1
        this%args(2) = a2
        this%args(3) = a3
        this%args(4) = a4
        this%args(5) = a5
        associate(arg1 => a1, &
                  arg2 => a2, &
                  arg3 => a3, &
                  arg4 => a4, &
                  arg5 => a5)
            if (associated(this%caller)) then
                call this%caller(this%f, arg1, arg2, arg3, arg4, arg5)
            else
                call this%f(arg1, arg2, arg3, arg4, arg5)
            end if
        end associate
    end subroutine
    
    !> @brief Bound procedure to invoke a method
    !! 
    !! @param[in] this The type bound to the method
    !! @param[in] a1 dummy argument
    !! @param[in] a2 dummy argument
    !! @param[in] a3 dummy argument
    !! @param[in] a4 dummy argument
    !! @param[in] a5 dummy argument
    !! @param[in] a6 dummy argument
    !!
    !! @b Examples
    !!
    !! The first example demonstrate how to invoke the method 
    !! by passing the arguments to the `invoke` procedure.
    !! @code{.f90}
    !! type(method) :: mtd
    !!
    !! mtd = method(6, f)
    !! call mtd%invoke(arg(a1, 'a1'), arg(a2, 'a2'), arg(a3, 'a3'), &
    !!                 arg(a4, 'a4'), arg(a5, 'a5'), arg(a6, 'a6'))
    !! @endcode
    !! Alternatively, on can also set the arguments directly from 
    !! the method constructor.
    !! @code{.f90}
    !! type(method) :: mtd
    !!
    !! mtd = method(f, arg(a1, 'a1'), arg(a2, 'a2'), arg(a3, 'a3'), &
    !!                 arg(a4, 'a4'), arg(a5, 'a5'), arg(a6, 'a6'))
    !! call mtd%invoke()
    !! @endcode
    !!
    !! @b Remarks
    subroutine invoke_a6(this, a1, a2, a3, a4, a5, a6)
        class(method), intent(inout)    :: this
        class(*), intent(in)            :: a1, a2, a3, a4, a5, a6

        if (this%nargs /= 6) stop -1
        
        this%args(1) = a1
        this%args(2) = a2
        this%args(3) = a3
        this%args(4) = a4
        this%args(5) = a5
        this%args(6) = a6
        associate(arg1 => a1, &
                  arg2 => a2, &
                  arg3 => a3, &
                  arg4 => a4, &
                  arg5 => a5, &
                  arg6 => a6)
            if (associated(this%caller)) then
                call this%caller(this%f, arg1, arg2, arg3, arg4, arg5, arg6)
            else
                call this%f(arg1, arg2, arg3, arg4, arg5, arg6)
            end if
        end associate
    end subroutine
    
    !> @brief Bound procedure to invoke a method
    !! 
    !! @param[in] this The type bound to the method
    !! @param[in] a1 dummy argument
    !! @param[in] a2 dummy argument
    !! @param[in] a3 dummy argument
    !! @param[in] a4 dummy argument
    !! @param[in] a5 dummy argument
    !! @param[in] a6 dummy argument
    !! @param[in] a7 dummy argument
    !!
    !! @b Examples
    !!
    !! The first example demonstrate how to invoke the method 
    !! by passing the arguments to the `invoke` procedure.
    !! @code{.f90}
    !! type(method) :: mtd
    !!
    !! mtd = method(7, f)
    !! call mtd%invoke(arg(a1, 'a1'), arg(a2, 'a2'), arg(a3, 'a3'), &
    !!                 arg(a4, 'a4'), arg(a5, 'a5'), arg(a6, 'a6'), arg(a7, 'a7'))
    !! @endcode
    !! Alternatively, on can also set the arguments directly from 
    !! the method constructor.
    !! @code{.f90}
    !! type(method) :: mtd
    !!
    !! mtd = method(f, arg(a1, 'a1'), arg(a2, 'a2'), arg(a3, 'a3'), &
    !!                 arg(a4, 'a4'), arg(a5, 'a5'), arg(a6, 'a6'), arg(a7, 'a7'))
    !! call mtd%invoke()
    !! @endcode
    !!
    !! @b Remarks
    subroutine invoke_a7(this, a1, a2, a3, a4, a5, a6, a7)
        class(method), intent(inout)    :: this
        class(*), intent(in)            :: a1, a2, a3, a4, a5, a6, a7

        if (this%nargs /= 7) stop -1
        
        this%args(1) = a1
        this%args(2) = a2
        this%args(3) = a3
        this%args(4) = a4
        this%args(5) = a5
        this%args(6) = a6
        this%args(7) = a7
        associate(arg1 => a1, &
                  arg2 => a2, &
                  arg3 => a3, &
                  arg4 => a4, &
                  arg5 => a5, &
                  arg6 => a6, &
                  arg7 => a7)
            if (associated(this%caller)) then
                call this%caller(this%f, arg1, arg2, arg3, arg4, arg5, arg6, arg7)
            else
                call this%f(arg1, arg2, arg3, arg4, arg5, arg6, arg7)
            end if
        end associate
    end subroutine
    
    !> @brief Assignment overloading. Assign a method to another 
    !!        method
    !! @param[inout] lhs The target method
    !! @param[in] rhs The source method
    !!
    !! @b Remarks
    subroutine method_assign_method(lhs, rhs)
        class(method), intent(inout)  :: lhs
        type(method), intent(in)      :: rhs
        !private
        integer :: i

        nullify(lhs%f)
        nullify(lhs%caller)
        lhs%f => rhs%f
        if (associated(rhs%caller)) then
            lhs%caller => rhs%caller
        end if
        lhs%nargs = rhs%nargs
        if (allocated(lhs%args)) deallocate(lhs%args)
        allocate(lhs%args(rhs%nargs), source = rhs%args)
        !
    end subroutine

    !> @brief Dispose resources associated with 
    !!        the bound type.
    !! @param[inout] this The type bound to the method
    !!
    !! @b Remarks
    subroutine dispose(this)
        class(method), intent(inout) :: this
        
        call finalize(this)
    end subroutine

    !> @private
    recursive subroutine finalize(this)
        type(method), intent(inout) :: this
        
        if (associated(this%f)) nullify(this%f)
        if (associated(this%caller)) nullify(this%caller)
    end subroutine
    
end module