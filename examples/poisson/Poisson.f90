#include <benchmark.inc>
program poisson
    use rhofunc
    use benchmark_library
    
    implicit none
    
    type(runner) :: br
    !
    !open(unit=15, file = 'report.csv')
    !br%csv_unit = 15
    !calling using preprocessor macro
    benchmark(br, run(1.0d-6, 30, poisson_naive))
    
    benchmark(br, run(1.0d-6, 30, poisson_optimized))

end program
