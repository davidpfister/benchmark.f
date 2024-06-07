module benchmark_method
    use iso_c_binding
    use benchmark_kinds
    use benchmark_caller
    
    implicit none
    
    public method
    private
       
    type :: method(nargs)
        integer, len :: nargs
        procedure(), nopass, pointer :: f => null()
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
    end type
       
    interface method
        module procedure :: method_new
    end interface
 
    contains
    
    subroutine method_new(f, m)
        type(method(*)), intent(inout) :: m
        procedure() :: f        
        
        m%f => f
    end subroutine
       
    subroutine invoke_a0(this)
        class(method(*)), intent(inout) :: this
        if (this%nargs /= 0) stop -1
        call this%f()
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
        f => this%f
        call caller(f, a1)
        nullify(f)
    end subroutine
    
    subroutine invoke_a2(this, a1, a2)
        class(method(*)), intent(inout) :: this
        class(*), intent(in) :: a1
        class(*), intent(in) :: a2

        if (this%nargs /= 2) stop -1
        
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
        f => this%f
        call caller(f, a1, a2, a3)
        nullify(f)
    end subroutine
    
    subroutine invoke_a4(this, a1, a2, a3, a4)
        class(method(*)), intent(inout) :: this
        class(*), intent(in) :: a1, a2, a3, a4

        if (this%nargs /= 4) stop -1
        
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
        f => this%f
        call caller(f, a1, a2, a3, a4)
        nullify(f)
    end subroutine
    
    subroutine invoke_a5(this, a1, a2, a3, a4, a5)
        class(method(*)), intent(inout) :: this
        class(*), intent(in) :: a1, a2, a3, a4, a5

        if (this%nargs /= 5) stop -1
        
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
        f => this%f
        call caller(f, a1, a2, a3, a4, a5)
        nullify(f)
    end subroutine
    
    subroutine invoke_a6(this, a1, a2, a3, a4, a5, a6)
        class(method(*)), intent(inout) :: this
        class(*), intent(in) :: a1, a2, a3, a4, a5, a6

        if (this%nargs /= 6) stop -1
        
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
        f => this%f
        call caller(f, a1, a2, a3, a4, a5, a6)
        nullify(f)
    end subroutine
    
    subroutine invoke_a7(this, a1, a2, a3, a4, a5, a6, a7)
        class(method(*)), intent(inout) :: this
        class(*), intent(in) :: a1, a2, a3, a4, a5, a6, a7

        if (this%nargs /= 7) stop -1
        
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
        f => this%f
        call caller(f, a1, a2, a3, a4, a5, a6, a7)
        nullify(f)
    end subroutine
end module