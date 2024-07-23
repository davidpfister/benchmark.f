#include <benchmark.inc>
program poisson
    use rhofunc
    use benchmark_library
    
    implicit none
    
    type(runner) :: br

    open(unit=15, file = 'report.csv')
    br%csv_unit = 15
    !calling using preprocessor macro
    benchmark(br, run(1.0d-6, 30, poisson_naive))
    benchmark(br, run(1.0d-6, 30, poisson_optimized))
    
    pause
    stop
    
    !calling using the derived type and the 'full' name
    br%name = '(1.0d-6, 30, test_poisson)'
    call br%run(1.0d-6, 30, poisson_optimized)
    
    !calling using the derived type and the 'full' name without brackets
    br%name = '1.0d-6, 30, test_poisson'
    call br%run(1.0d-6, 30, poisson_optimized)
    
    !calling using the derived type and partial name
    br%name = '30, test_poisson'
    call br%run(1.0d-6, 30, poisson_optimized)
    
    !calling using the derived type and derived type
    br%name = '1.0d-6,, test_poisson'
    call br%run(1.0d-6, 30, poisson_optimized)
    
    !calling using the derived type and only the function name
    br%name = 'test_poisson'
    call br%run(1.0d-6, 30, poisson_optimized)
    
    !calling using the derived type and and empty name
    br%name = ''
    call br%run(1.0d-6, 30, poisson_optimized)

end program
