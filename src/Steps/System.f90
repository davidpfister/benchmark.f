!> @file
!! @defgroup group_steps_system System
!! Retrieve system information at run time
!! @ingroup group_steps
module benchmark_steps_system
    use benchmark_systeminfo
    use benchmark_workflow, only: workflow
    use benchmark_output_unit

    implicit none; private

    public :: system

    !> @brief system constructor
    !! @ingroup group_steps_system
    interface system
        module procedure :: system_new
    end interface

contains

    type(workflow) function system_new() result(step)
        step%header = '                                     -- SYSTEM INFO --                                               '
        step%action => step_run
    end function

    subroutine step_run(step)
        class(workflow), intent(inout) :: step

        write(output_unit, '(A)') new_line('A'), step%header
        call get_systeminfo()
    end subroutine
end module
