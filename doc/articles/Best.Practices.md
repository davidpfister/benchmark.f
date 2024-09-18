# Best Practices {#best_practices}

[TOC]

There are good practices to keep in mind when benchmarking a 
Fortran code designed to avoid the most common problems and help to ensure 
reliability, reproducibility and usefulness of the results. 
The list below provides some tips to benchmark Fortran code:

## Isolate the code

This means separating the code from the main program, 
perhaps in terms of its execution and in terms of its interactions. 
When isolating code for benchmarking, consider creating small, 
self-contained examples that focus on the specific 
functionality you want to measure. 

## Isolate the environment

This means executing a benchmark program that is optimized for execution 
and ensure the lowest possible overhead from the environment. It is always 
advisable to run the code from the command line and not from an IDE, or notebook
like environment.
Certain editors can add overhead that can dramatically slow down the execution of the program.
They may also automatically add tracing and debugging capabilities. 

In addition, it is good practice to disable program output and logging.
Disk IO are usually slow as compared to other operations.

## Use a High-Precision Clock

The system has more than one clock available. In Fortran, one can use `cpu_time`, `date_and_time` and `system_clock`
`system_clock` with 64-bits integer usually gives the best precision. 

```fortran
block
    integer(i8) :: clock_max, clock_rate, clock_reading

    call system_clock(clock_reading, clock_rate, clock_max)
    ctime = 1000_r8 * real(clock_reading, r8) / real(clock_rate, r8)
end block
```

## Warmup the Target Code

It is a good practice to warm up the target code before the target code is benchmarked.
This ensures that the execution time is not longer than it might otherwise be and ensures 
that the code is able to run at the full speed. Warm up can be performed by executing the code 
directly before benchmarking.

## Repeat Benchmarks and Report a Summary Statistic

Benchmark results will vary. The reason is because of the natural variation in running a program on a system. 
It will take slightly more or less time each time it runs because of operating 
system activity, CPU load, memory usage, external dependencies, caching, etc.

This adds random noise to the benchmark measure which can be leveraged by repeating a benchmark many times 
and computing statistics on the results. Common statistical measures include the average, minimum, and maximum, outliers. 