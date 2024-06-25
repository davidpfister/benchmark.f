module benchmark_method
    use iso_c_binding
    use benchmark_kinds
    use benchmark_caller
    use benchmark_method_argument
    
    implicit none
    
    public method
    private
    
       
    type :: method(nargs)
        integer, len :: nargs
        procedure(), nopass, pointer    :: f => null()
        type(argument)                  :: args(nargs)
    contains
        procedure, pass(this), private :: invoke_a0
        procedure, pass(this), private :: invoke_a1
        procedure, pass(this), private :: invoke_a2
        procedure, pass(this), private :: invoke_a3
        procedure, pass(this), private :: invoke_a4
        procedure, pass(this), private :: invoke_a5
        procedure, pass(this), private :: invoke_a6
        procedure, pass(this), private :: invoke_a7
        procedure, pass(this), private :: invoke_a0_with_caller
        procedure, pass(this), private :: invoke_a1_with_caller
        procedure, pass(this), private :: invoke_a2_with_caller
        procedure, pass(this), private :: invoke_a3_with_caller
        procedure, pass(this), private :: invoke_a4_with_caller
        procedure, pass(this), private :: invoke_a5_with_caller
        procedure, pass(this), private :: invoke_a6_with_caller
        procedure, pass(this), private :: invoke_a7_with_caller
        generic, public :: invoke => invoke_a0, &
                                     invoke_a1, &
                                     invoke_a2, &
                                     invoke_a3, &
                                     invoke_a4, &
                                     invoke_a5, &
                                     invoke_a6, &
                                     invoke_a7, &
                                     invoke_a0_with_caller, &
                                     invoke_a1_with_caller, &
                                     invoke_a2_with_caller, &
                                     invoke_a3_with_caller, &
                                     invoke_a4_with_caller, &
                                     invoke_a5_with_caller, &
                                     invoke_a6_with_caller, &
                                     invoke_a7_with_caller
        procedure, pass(lhs), private :: method_assign_method
        generic, public :: assignment(=) => method_assign_method
    end type
    
    interface method
        module procedure method_create_0, &
                        method_create_1, &
                        method_create_2, &
                        method_create_3, &
                        method_create_4, &
                        method_create_5, &
                        method_create_6, &
                        method_create_7
    end interface
 
    contains
    
    function method_create_0(f) result(that)
        procedure()                     :: f
        type(method(0))                 :: that        
        
        that%f => f
    end function
    
    function method_create_1(f, a1) result(that)
        procedure()                     :: f
        class(*), intent(in)            :: a1
        type(method(1))                 :: that
        
        that%f => f
        allocate(that%args(1)%value, source = a1)
    end function
    
    function method_create_2(f, a1, a2) result(that)
        procedure()                     :: f
        class(*), intent(in)            :: a1
        class(*), intent(in)            :: a2
        type(method(2))                 :: that
        
        that%f => f
        allocate(that%args(1)%value, source = a1)
        allocate(that%args(2)%value, source = a2)
    end function
    
    function method_create_3(f, a1, a2, a3) result(that)
        procedure()                     :: f
        class(*), intent(in)            :: a1
        class(*), intent(in)            :: a2
        class(*), intent(in)            :: a3
        type(method(3))                 :: that
        
        that%f => f
        allocate(that%args(1)%value, source = a1)
        allocate(that%args(2)%value, source = a2)
        allocate(that%args(3)%value, source = a3)
    end function
    
    function method_create_4(f, a1, a2, a3, a4) result(that)
        procedure()                     :: f
        class(*), intent(in)            :: a1
        class(*), intent(in)            :: a2
        class(*), intent(in)            :: a3
        class(*), intent(in)            :: a4
        type(method(4))                 :: that
        
        that%f => f
        allocate(that%args(1)%value, source = a1)
        allocate(that%args(2)%value, source = a2)
        allocate(that%args(3)%value, source = a3)
        allocate(that%args(4)%value, source = a4)
    end function
    
    function method_create_5(f, a1, a2, a3, a4, a5) result(that)
        procedure()                     :: f
        class(*), intent(in)            :: a1
        class(*), intent(in)            :: a2
        class(*), intent(in)            :: a3
        class(*), intent(in)            :: a4
        class(*), intent(in)            :: a5
        type(method(5))                 :: that
        
        that%f => f
        allocate(that%args(1)%value, source = a1)
        allocate(that%args(2)%value, source = a2)
        allocate(that%args(3)%value, source = a3)
        allocate(that%args(4)%value, source = a4)
        allocate(that%args(5)%value, source = a5)
    end function
    
    function method_create_6(f, a1, a2, a3, a4, a5, a6) result(that)
        procedure()                     :: f
        class(*), intent(in)            :: a1
        class(*), intent(in)            :: a2
        class(*), intent(in)            :: a3
        class(*), intent(in)            :: a4
        class(*), intent(in)            :: a5
        class(*), intent(in)            :: a6
        type(method(6))                 :: that
        
        that%f => f
        allocate(that%args(1)%value, source = a1)
        allocate(that%args(2)%value, source = a2)
        allocate(that%args(3)%value, source = a3)
        allocate(that%args(4)%value, source = a4)
        allocate(that%args(5)%value, source = a5)
        allocate(that%args(6)%value, source = a6)
    end function
    
    function method_create_7(f, a1, a2, a3, a4, a5, a6, a7) result(that)
        procedure()                     :: f
        class(*), intent(in)            :: a1
        class(*), intent(in)            :: a2
        class(*), intent(in)            :: a3
        class(*), intent(in)            :: a4
        class(*), intent(in)            :: a5
        class(*), intent(in)            :: a6
        class(*), intent(in)            :: a7
        type(method(7))                 :: that
        
        that%f => f
        allocate(that%args(1)%value, source = a1)
        allocate(that%args(2)%value, source = a2)
        allocate(that%args(3)%value, source = a3)
        allocate(that%args(4)%value, source = a4)
        allocate(that%args(5)%value, source = a5)
        allocate(that%args(6)%value, source = a6)
        allocate(that%args(7)%value, source = a7)
    end function
    
    subroutine invoke_a0(this)
        class(method(*)), intent(inout) :: this
        select case (this%nargs)
        case (0)
            call this%f()
        case (1)
            if (.not. allocated(this%args(1)%value)) stop -1
            associate(arg1 => this%args(1)%value)
                call this%f(arg1)
            end associate
        case (2)
            if (.not. (allocated(this%args(1)%value) .and. &
                       allocated(this%args(2)%value))) stop -1
            associate(arg1 => this%args(1)%value, &
                      arg2 => this%args(2)%value)
                call this%f(arg1, arg2)
            end associate
        case (3)
            if (.not. (allocated(this%args(1)%value) .and. &
                       allocated(this%args(2)%value) .and. &
                       allocated(this%args(3)%value))) stop -1
            associate(arg1 => this%args(1)%value, &
                      arg2 => this%args(2)%value, &
                      arg3 => this%args(3)%value)
                call this%f(arg1, arg2, arg3)
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
                call this%f(arg1, arg2, arg3, arg4)
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
                call this%f(arg1, arg2, arg3, arg4, arg5)
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
                call this%f(arg1, arg2, arg3, arg4, arg5, arg6)
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
                call this%f(arg1, arg2, arg3, arg4, arg5, arg6, arg7)
            end associate
        case default
            stop -1
        end select
    end subroutine
    
    subroutine invoke_a0_with_caller(this, caller)
        class(method(*)), intent(inout) :: this
        procedure(caller_a0_x) :: caller
        !private
        procedure(), pointer :: f => null()
        
        if (this%nargs /= 0) stop -1
        f => this%f
        call caller(f)
        nullify(f)
    end subroutine
    
    subroutine invoke_a1(this, a1)
        class(method(*)), intent(inout) :: this
        class(*), intent(in) :: a1

        if (this%nargs /= 1) stop -1
        
        this%args(1) = a1
        associate(arg1 => a1)
            call this%f(arg1)
        end associate
    end subroutine
    
    subroutine invoke_a1_with_caller(this, a1, caller)
        class(method(*)), intent(inout) :: this
        class(*), intent(in) :: a1
        procedure(caller_a1_x) :: caller
        !private
        procedure(), pointer :: f => null()

        if (this%nargs /= 1) stop -1  
        this%args(1) = a1
        f => this%f
        call caller(f, a1)
        nullify(f)
    end subroutine
    
    subroutine invoke_a2(this, a1, a2)
        class(method(*)), intent(inout) :: this
        class(*), intent(in) :: a1
        class(*), intent(in) :: a2

        if (this%nargs /= 2) stop -1
        
        this%args(1) = a1
        this%args(2) = a2
        associate(arg1 => a1, arg2 => a2)
            call this%f(arg1, arg2)
        end associate
    end subroutine
    
    subroutine invoke_a2_with_caller(this, a1, a2, caller)
        class(method(*)), intent(inout) :: this
        class(*), intent(in) :: a1
        class(*), intent(in) :: a2
        procedure(caller_a2_x) :: caller
        !private
        procedure(), pointer :: f => null()

        if (this%nargs /= 2) stop -1
        this%args(1) = a1
        this%args(2) = a2
        f => this%f
        call caller(f, a1, a2)
        nullify(f)
    end subroutine
    
    subroutine invoke_a3(this, a1, a2, a3)
        class(method(*)), intent(inout) :: this
        class(*), intent(in) :: a1
        class(*), intent(in) :: a2
        class(*), intent(in) :: a3

        if (this%nargs /= 3) stop -1
        
        this%args(1) = a1
        this%args(2) = a2
        this%args(3) = a3
        associate(arg1 => a1, &
                  arg2 => a2, &
                  arg3 => a3)
            call this%f(arg1, arg2, arg3)
        end associate
    end subroutine
    
    subroutine invoke_a3_with_caller(this, a1, a2, a3, caller)
        class(method(*)), intent(inout) :: this
        class(*), intent(in) :: a1
        class(*), intent(in) :: a2
        class(*), intent(in) :: a3
        procedure(caller_a3_x) :: caller
        !private
        procedure(), pointer :: f => null()
        
        if (this%nargs /= 3) stop -1
        
        this%args(1) = a1
        this%args(2) = a2
        this%args(3) = a3
        f => this%f
        call caller(f, a1, a2, a3)
        nullify(f)
    end subroutine
    
    subroutine invoke_a4(this, a1, a2, a3, a4)
        class(method(*)), intent(inout) :: this
        class(*), intent(in) :: a1, a2, a3, a4

        if (this%nargs /= 4) stop -1
        
        this%args(1) = a1
        this%args(2) = a2
        this%args(3) = a3
        this%args(4) = a4
        associate(arg1 => a1, &
                  arg2 => a2, &
                  arg3 => a3, &
                  arg4 => a4)
            call this%f(arg1, arg2, arg3, &
                        arg4)
        end associate
    end subroutine
    
    subroutine invoke_a4_with_caller(this, a1, a2, a3, a4, caller)
        class(method(*)), intent(inout) :: this
        class(*), intent(in) :: a1
        class(*), intent(in) :: a2
        class(*), intent(in) :: a3
        class(*), intent(in) :: a4
        procedure(caller_a4_x) :: caller
        !private
        procedure(), pointer :: f => null()
        
        if (this%nargs /= 4) stop -1
        
        this%args(1) = a1
        this%args(2) = a2
        this%args(3) = a3
        this%args(4) = a4
        f => this%f
        call caller(f, a1, a2, a3, a4)
        nullify(f)
    end subroutine
    
    subroutine invoke_a5(this, a1, a2, a3, a4, a5)
        class(method(*)), intent(inout) :: this
        class(*), intent(in) :: a1, a2, a3, a4, a5

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
            call this%f(arg1, arg2, arg3, &
                        arg4, arg5)
        end associate
    end subroutine
    
    subroutine invoke_a5_with_caller(this, a1, a2, a3, a4, a5, caller)
        class(method(*)), intent(inout) :: this
        class(*), intent(in) :: a1
        class(*), intent(in) :: a2
        class(*), intent(in) :: a3
        class(*), intent(in) :: a4
        class(*), intent(in) :: a5
        procedure(caller_a5_x) :: caller
        !private
        procedure(), pointer :: f => null()
        
        if (this%nargs /= 5) stop -1
        
        this%args(1) = a1
        this%args(2) = a2
        this%args(3) = a3
        this%args(4) = a4
        this%args(5) = a5
        f => this%f
        call caller(f, a1, a2, a3, a4, a5)
        nullify(f)
    end subroutine
    
    subroutine invoke_a6(this, a1, a2, a3, a4, a5, a6)
        class(method(*)), intent(inout) :: this
        class(*), intent(in) :: a1, a2, a3, a4, a5, a6

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
            call this%f(arg1, arg2, arg3, &
                        arg4, arg5, arg6)
        end associate
    end subroutine
    
    subroutine invoke_a6_with_caller(this, a1, a2, a3, a4, a5, a6, caller)
        class(method(*)), intent(inout) :: this
        class(*), intent(in) :: a1
        class(*), intent(in) :: a2
        class(*), intent(in) :: a3
        class(*), intent(in) :: a4
        class(*), intent(in) :: a5
        class(*), intent(in) :: a6
        procedure(caller_a6_x) :: caller
        !private
        procedure(), pointer :: f => null()
        
        if (this%nargs /= 6) stop -1
        
        this%args(1) = a1
        this%args(2) = a2
        this%args(3) = a3
        this%args(4) = a4
        this%args(5) = a5
        this%args(6) = a6
        f => this%f
        call caller(f, a1, a2, a3, a4, a5, a6)
        nullify(f)
    end subroutine
    
    subroutine invoke_a7(this, a1, a2, a3, a4, a5, a6, a7)
        class(method(*)), intent(inout) :: this
        class(*), intent(in) :: a1, a2, a3, a4, a5, a6, a7

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
            call this%f(arg1, arg2, arg3, &
                        arg4, arg5, arg6, &
                        arg7)
        end associate
    end subroutine
    
    subroutine invoke_a7_with_caller(this, a1, a2, a3, a4, a5, a6, a7, caller)
        class(method(*)), intent(inout) :: this
        class(*), intent(in) :: a1
        class(*), intent(in) :: a2
        class(*), intent(in) :: a3
        class(*), intent(in) :: a4
        class(*), intent(in) :: a5
        class(*), intent(in) :: a6
        class(*), intent(in) :: a7
        procedure(caller_a7_x) :: caller
        !private
        procedure(), pointer :: f => null()

        if (this%nargs /= 7) stop -1
        
        this%args(1) = a1
        this%args(2) = a2
        this%args(3) = a3
        this%args(4) = a4
        this%args(5) = a5
        this%args(6) = a6
        this%args(7) = a7
        f => this%f
        call caller(f, a1, a2, a3, a4, a5, a6, a7)
        nullify(f)
    end subroutine
    
    subroutine method_assign_method(lhs, rhs)
        type(method(*)), intent(inout)  :: lhs
        type(method(*)), intent(in)     :: rhs
        !private
        integer :: i
        
        nullify(lhs%f)
        lhs%f => rhs%f
        if (lhs%nargs == rhs%nargs) then
            do i = 1, lhs%nargs
                if (allocated(lhs%args(i)%value)) deallocate(lhs%args(i)%value)
                allocate(lhs%args(i)%value, source = rhs%args(i)%value)
            end do
        else
            error stop 'Assignment not supported'
        end if
    end subroutine
end module