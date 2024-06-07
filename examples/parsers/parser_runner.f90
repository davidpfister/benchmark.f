module parser_runner
    use parser_factory
    use parameters
    
    private 
    
    public :: runner_test, &
              runner, &
              parsers
    
    type, public :: parser
        character(:), allocatable :: name
        integer :: index
        class(parser_x), allocatable :: interpretor
    contains 
        procedure, pass(this), public :: initialize
    end type
    
    type(parser) :: runner
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
    
    subroutine runner_test(i)
        integer(i4), intent(in) :: i
        !private
        real(r8) :: res

        associate(x => runner%interpretor)
            res = x%compute(i)
            if (abs(results(runner%index)- res > 1.0d-3)) then 
                pause
            end if
        end associate
    end subroutine
end module