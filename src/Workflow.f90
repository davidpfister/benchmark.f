module benchmark_workflow

    implicit none
    
    type workflow
        procedure(), nopass, pointer :: action => null()
        type(workflow), pointer :: next => null()
    contains
        procedure, pass(this), public :: add => workflow_add
        procedure, pass(this), public :: run => workflow_run
        final :: dispose
    end type
    
    interface workflow
        module procedure :: workflow_new
    end interface
    
    interface
        subroutine void()
        end subroutine
    end interface
    
    contains
    
    type(workflow) function workflow_new(a) result(w)
        procedure(void) :: a

        w%action => a
    end function
    
    function workflow_add(this, a) result(p)
        class(workflow), intent(inout), target :: this
        procedure(void) :: a
        !private
        type(workflow), pointer :: p 
        p => null()
        
        p => this
        if (.not. associated(p%action)) then 
            p%action => a
            return 
        end if
        
        do while (associated(p%next))
            p => p%next
        end do
        allocate(p%next)
        p => p%next
        p%action => a 
    end function
    
    subroutine workflow_run(this)
        class(workflow), intent(inout), target :: this
        !private
        class(workflow), pointer :: p
        p => this
        do while (associated(p))
            if (associated(p%action)) call p%action()
            p => p%next
        end do
    end subroutine
    
    recursive subroutine dispose(this)
        type(workflow), intent(inout) :: this
        
        if (associated(this%next)) then
            call dispose(this%next)
            deallocate(this%next)
            nullify(this%next)
        end if
    end subroutine
    
end module