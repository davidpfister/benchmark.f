#include <benchmark.inc>
program allocation
    use benchmark_kinds
    use benchmark_library
    use allocation_library
    
    implicit none
    
    block
        type(runner) :: br
    
        benchmark(br, run(50000, array_preallocation))
        benchmark(br, run(100000, array_preallocation))
    
        benchmark(br, run(50000, array_constructor))
        benchmark(br, run(100000, array_constructor))

        benchmark(br, run(50000, naive_reallocation))
        benchmark(br, run(100000, naive_reallocation))
    
        benchmark(br, run(100, 50000, buffer_reallocation))
        benchmark(br, run(100, 100000, buffer_reallocation))
    
        benchmark(br, run(1000, 50000, buffer_reallocation))
        benchmark(br, run(1000, 100000, buffer_reallocation))
    end block
    read(*,*)
end program
            
