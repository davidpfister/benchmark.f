#include <benchmark.inc>
program poisson
    use rhofunc
    use benchmark_library
    
    implicit none
    
    block
        type(runner) :: br

        benchmark(br, run(1.0d-6, 30, poisson_naive))  
        benchmark(br, run(1.0d-6, 30, poisson_optimized))
    end block
    read(*,*)
end program
