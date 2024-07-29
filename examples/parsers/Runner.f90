module parser_runner
    use parser_factory
    use parameters
    
    private 
    
    public :: runner_test, &
              current_parser, &
              parsers
    
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
        call this%interpretor%initialize()
    end subroutine
    
    subroutine runner_test(eq)
        character(200), intent(in) :: eq
        !private
        real(r8) :: res

        associate(x => current_parser%interpretor)
            res = x%compute(trim(eq))
            if (abs(results(current_parser%index)- res > 0.001_r8)) then 
                pause
            end if
        end associate
    end subroutine
end module