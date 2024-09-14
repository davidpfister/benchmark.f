# Best Practices {#best_practices}

[TOC]

There are good practices to keep in mind when benchmarking a Fortran code designed to avoid the most common problems and help to ensure 
reliability, reproducibility and usefulness of the results. 
The list below provides some tips to benchmark Fortran code:

## Isolate the code

This means separating it from the main program, perhaps in terms of its execution and in terms of its interactions. 
When isolating code for benchmarking, consider creating small, self-contained examples that focus on the specific 
functionality you want to measure. 

## Isolate Benchmark Environment

We must ensure that when we execute a benchmark program it is optimized for execution and gives the lowest overhead.
Run the code in isolation in the simplest possible manner, such as from the command line.

*   Consider not benchmarking from an IDE.
*   Consider not benchmarking from a text editor.
*   Consider not benchmarking within a notebook like Jupyter.
*   Consider not benchmarking from the interactive interpreter.

Certain editors can add overhead that can dramatically slow down the execution of the program.
They may also automatically add tracing and debugging capabilities. They may also capture input and output 
and make other subtle changes to the program that influence its execution speed.

In addition, it is good practice to disable program output and logging.
Disk IO are usually slow as compared to other operations.

*   Consider conditioning all print/write statements with preprocessor macros
*   Consider disabling log while benchmarking.

## Use a High-Precision Clock

The system has more than one clock available.

Different clocks have different capabilities, most notably the precision of the clock.

A clock that is able to measure more ticks per second may in turn provide a more precise measurement of the execution time of target code.
To increase the resolution of the code one can also consider running the same code multiple times. 

## Warmup the Target Code

It is a good practice to warm up the target code before the target code is benchmarked.
This ensures that the execution time is not longer than it might otherwise be and ensures 
that the code is able to run at the full speed capable of the interpreter. We can warm up 
the target code by executing it directly before benchmarking.

## Repeat Benchmarks and Report a Summary Statistic

Benchmark results will vary. In fact, each time a single benchmark run is performed the result will be different 
and this can almost be guaranteed. The reason is because of the natural variation in running a program on a system. 
It will take slightly more or less time each time it is run. This is the case for many reasons, such as operating 
system activity, CPU load, memory usage, external dependencies, caching, and so on.

This adds random noise to the benchmark measure and misleading benchmark results in turn. We can 
counter the statistical noise inherent in benchmark scores by repeating a benchmark many times and collecting many 
measures. This provides a distribution of samples of benchmark measures. From this sample, we can report a single 
summary statistic. Common examples include the mean (average), median, minimum, and maximum. The larger the sample 
of measures, the more stable the measure.