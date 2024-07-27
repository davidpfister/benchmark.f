
!> @ingroup group_method group_all
!> @author davidpfister
!> @file
!> @cond
#include <concat.inc>
#define ADD_ARGUMENT(n) \
that%args(n) = MACRO_CAT2(a,n)
!> @endcond
module benchmark_method
    use iso_c_binding
    use benchmark_kinds
    use benchmark_method_argument
    
    implicit none
    
    public :: method, arg
    
    private

    type :: method
        private
        integer, public                 :: nargs
        procedure(), nopass, pointer    :: f => null()
        procedure(), nopass, pointer    :: caller => null()
        type(arg), allocatable, public  :: args(:)
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
    end type
    
    interface method
        module procedure :: method_create, & 
                            method_create_0, &
                            method_create_1, &
                            method_create_2, &
                            method_create_3, &
                            method_create_4, &
                            method_create_5, &
                            method_create_6, &
                            method_create_7
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
    
end module