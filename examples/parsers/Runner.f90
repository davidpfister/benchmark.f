module parser_runner
    use parser_factory
    use parameters
    
    private 
    
    public :: runner_test, &
              current_parser
    
    type, public :: parser
        character(:), allocatable :: name
        integer :: index
        class(parser_x), allocatable :: interpretor
    contains 
        procedure, pass(this), public :: initialize
    end type
    
    type(parser) :: current_parser
    type(factory) :: fact
        
    contains 
    
    subroutine initialize(this, name)
        class(parser), intent(inout) :: this
        character(*), intent(in) :: name
        
        this%name = name 
        this%index = 0
        this%interpretor = fact%build(name)
        if (.not. allocated(current_parser%interpretor)) return
        call this%interpretor%initialize()
    end subroutine
    
    subroutine runner_test(eq)
#ifndef __INTEL_COMPILER
        class(*), intent(in) :: eq
#else
        character(200), intent(in) :: eq
#endif
        !private
        real(r8) :: res
        if (.not. allocated(current_parser%interpretor)) return
#ifndef __INTEL_COMPILER
        select type(eq)
        type is (character(*))
#endif
        associate(x => current_parser%interpretor)
            res = x%compute(trim(eq))
#ifdef _DEBUG
            if (abs(results(current_parser%index)- res > 0.001_r8)) then 
                pause
            end if
#endif
        end associate
#ifndef __INTEL_COMPILER
        end select
#endif
    end subroutine
end module