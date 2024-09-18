!> @defgroup group_workflow benchmark_workflow
!! @brief Workflow module
module benchmark_workflow

    implicit none; private
    
    !> @class workflow
    !! @ingroup group_workflow
    !! @brief Provides properties and instance methods for the creation, 
    !!        modification, execution and deletion of a @ref workflow. This 
    !!        object is used to execute the various @link group_steps steps @endlink
    !!        involved when benchmarking.
    !! @verbatim type, public :: workflow @endverbatim
    !! <h2>Examples</h2>
    !! The following example demonstrates some of the main members of the 
    !! @link benchmark_workflow::workflow workflow @endlink class.
    !! @n
    !! The steps are first loaded into the workflow using the method 
    !! @link benchmark_workflow::workflow::add add @endlink.
    !! @n
    !! It is then executed in the order the steps were added by invoking
    !! the function @link benchmark_workflow::workflow::run run @endlink. 
    !! The function @link benchmark_workflow::workflow::run run @endlink
    !! returns a pointer to the next available step, i.e. the position of
    !! the next step in the entire workflow.
    !! @snippet snippet.f90 workflow_example
    !! <h2>Remarks</h2>
    !! Use the @link benchmark_workflow::workflow workflow @endlink class
    !! for situation where one has a clear series of steps to be performed sequentially
    !! @note This class does not support parallelization and may never will. 
    !! @par
    !! <h2>Constructors</h2>
    !! Initializes a new instance of the @link benchmark_workflow::workflow workflow @endlink class
    !! <h3>workflow(procedure(work))</h3>
    !! @verbatim type(workflow) function workflow(procedure(work) a)@endverbatim
    !! 
    !! @param[in] a The action to be performed
    !! 
    !! @b Examples
    !! @code{.f90}
    !! !foo() is a subroutine of type subroutine work(class(workflow))
    !!  wf = workflow(foo())
    !! @endcode
    !! @b Remarks
    type, public :: workflow
        private
        character(:), allocatable, public :: header !< Header is the first node of the linked list of actions
        procedure(work), nopass, pointer, public :: action => null()    !< Action to be performed
        class(workflow), pointer, public :: next => null() !< Next action in the linked list
    contains
        procedure, pass(this), private :: workflow_add
        procedure, pass(this), private :: workflow_add_action
        generic, public :: add => workflow_add, workflow_add_action
        procedure, pass(this), public :: run => workflow_run
        procedure, pass(this), public       :: dispose
        final :: finalize
    end type
    
    interface workflow
        !> @cond
        module procedure :: workflow_new
        !> @endcond
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

    !> @brief Add a step to an existing workflow, described by a
    !!        workflow object.
    !! @param[inout] this The type bound to the method
    !! @param[in] w The workflow to be added
    !!
    !! @b Remarks
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
    
    !> @brief Add a step to an existing workflow, described by an
    !!        action object. The action is a s procedure implementing
    !!        the @link benchmark_workflow::work work@endlink interface
    !! @param[inout] this The type bound to the method
    !! @param[in] a The action to be added
    !!
    !! @b Remarks
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

    !> @brief Add a step to an existing workflow, described by an
    !!        action object. The action is a s procedure implementing
    !!        the @link benchmark_workflow::work work@endlink interface
    !! @param[inout] this The type bound to the method
    !! @return A pointer of type class(@ref workflow), pointing
    !!         to the next available step
    !!
    !! @b Remarks
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

    !> @brief Dispose resources associated with 
    !!        the bound type.
    !! @param[inout] this The type bound to the method
    !!
    !! @b Remarks
    subroutine dispose(this)
        class(workflow), intent(inout) :: this
        
        call finalize(this)
    end subroutine

    !> @private
    recursive subroutine finalize(this)
        type(workflow), intent(inout) :: this
        
        if (associated(this%next)) then
            call dispose(this%next)
            if (allocated(this%header)) deallocate(this%header)
            this%next => null()
        end if
    end subroutine
end module