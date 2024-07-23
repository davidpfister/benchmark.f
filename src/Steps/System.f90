!> @ingroup group_all group_steps
!> @author davidpfister
!> @brief Retrieve system infortion at run time
!> @details This steps retrieves system information at run time. 
!>          The method was inspired by the paramonte library (https://github.com/cdslaborg/paramonte/blob/main/src/fortran/main/pm_sysInfo.F90)
!>          from AmirShahmoradi. 
!>          It has been simplified and adapted to the needs
module benchmark_steps_system
    use benchmark_systeminfo
    use benchmark_workflow, only: workflow
    use benchmark_output_unit
    
    implicit none
    
    private
    
    public :: system
       
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
        
        write (output_unit, '(A)') new_line('A'), step%header
        call get_systeminfo()
    end subroutine
end module