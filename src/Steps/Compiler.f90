!> @ingroup group_all group_steps
!> @author davidpfister
!> @brief Retrieve compiler infortion at run time
!> @details This steps retrieves compiler information as the compiler's name
!>          and version, as well as compilation options. It uses the intrinsic 
!>          functions `compiler_options` and `compiler_version` from the 
!>          module `iso_fortran_env`
module benchmark_steps_compiler
    use benchmark_compilerinfo
    use benchmark_workflow, only: workflow
    use benchmark_output_unit
    
    implicit none
    
    private
    
    public :: compiler
       
    interface compiler
        module procedure :: compiler_new
    end interface
    
    contains
    
    type(workflow) function compiler_new() result(step)
        step%header = '                                     -- COMPILER INFO --                                               '
        step%action => step_run
    end function
    
    subroutine step_run(step)
        class(workflow), intent(inout) :: step
        
        write (output_unit, '(A)') new_line('A'), step%header
        call get_compilerinfo()
    end subroutine
end module