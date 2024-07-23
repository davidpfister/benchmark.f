!> @ingroup group_all group_workflow
!> @author davidpfister
module benchmark_workflow

    implicit none
    
    private
    
    type, public :: workflow
        character(:), allocatable :: header
        procedure(work), nopass, pointer :: action => null()
        class(workflow), pointer :: next => null()
    contains
        procedure, pass(this), private :: workflow_add
        procedure, pass(this), private :: workflow_add_action
        generic, public :: add => workflow_add, workflow_add_action
        procedure, pass(this), public :: run => workflow_run
        final :: dispose
    end type
    
    interface workflow
        module procedure :: workflow_new
    end interface
    
    interface
        subroutine work(step)
            import
            class(workflow), intent(inout) :: step
        end subroutine
    end interface
    
    contains
    
    type(workflow) function workflow_new(a) result(w)
        procedure(work) :: a

        w%action => a
    end function
    
    subroutine workflow_add(this, w)
        class(workflow), intent(inout), target :: this
        class(workflow), intent(in)             :: w
        !private
        class(workflow), pointer :: p 
        p => null()

        p => this        
        do while (associated(p%next))
            p => p%next
        end do
        allocate(p%next, source = w)
        p => p%next
    end subroutine
    
    subroutine workflow_add_action(this, a)
        class(workflow), intent(inout), target :: this
        procedure(work) :: a
        !private
        class(workflow), pointer :: p 
        p => null()

        p => this       
        do while (associated(p%next))
            p => p%next
        end do
        allocate(p%next)
        p => p%next
        p%action => a 
    end subroutine
    
    function workflow_run(this) result(p)
        class(workflow), intent(inout), target :: this
        !private
        class(workflow), pointer :: p
        p => this%next
        do while (.true.)
            if (associated(p)) then
                if (associated(p%action)) call p%action(p)
                if (associated(p%next)) then
                    p => p%next
                else
                    exit
                end if
            else
                exit
            end if
        end do
    end function
    
    recursive subroutine dispose(this)
        type(workflow), intent(inout) :: this
        
        if (associated(this%next)) then
            call dispose(this%next)
            if (allocated(this%header)) deallocate(this%header)
            nullify(this%next)
        end if
    end subroutine
    
end module