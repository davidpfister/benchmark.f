 ## Fortran Benchmarking Best Practices {#best_practices}

[Toggle](#)

*   [9 Fortran Benchmarking Best Practices](#9_Fortran_Benchmarking_Best_Practices "9 Fortran Benchmarking Best Practices")
*   [Isolate Benchmark Code](#Isolate_Benchmark_Code "Isolate Benchmark Code")
*   [Isolate Benchmark Environment](#Isolate_Benchmark_Environment "Isolate Benchmark Environment")
*   [Use a High-Precision Clock](#Use_a_High-Precision_Clock "Use a High-Precision Clock")
*   [Use a Non-Adjustable and Monotonic Clock](#Use_a_Non-Adjustable_and_Monotonic_Clock "Use a Non-Adjustable and Monotonic Clock")
*   [Repeat Benchmarks and Report a Summary Statistic](#Repeat_Benchmarks_and_Report_a_Summary_Statistic "Repeat Benchmarks and Report a Summary Statistic")
*   [Warmup the Target Code](#Warmup_the_Target_Code "Warmup the Target Code")
*   [Present Results with Appropriate Precision and Units](#Present_Results_with_Appropriate_Precision_and_Units "Present Results with Appropriate Precision and Units")
*   [Disable the Fortran Garbage Collector](#Disable_the_Fortran_Garbage_Collector "Disable the Fortran Garbage Collector")
*   [Disable Program Output](#Disable_Program_Output "Disable Program Output")
*   [Further Reading](#Further_Reading "Further Reading")
*   [Takeaways](#Takeaways "Takeaways")

### Cheat sheet for reliable benchmarking

There are good practices that can implemented when benchmarking code in Fortran that will avoid the most common problems and help to ensure that benchmark results are stable and useful.

The focus here is on benchmarking the execution time of the target code, perhaps the most common type of benchmarking.

The short list below provides tips to keep in mind when benchmarking the execution time of Fortran code:

1.  Isolate the code.
2.  Isolate the environment.
3.  Use a good clock.
4.  Warm up the machine first.
5.  Repeat benchmark until reaching steady state.
6.  Present results with statistics and appropriate precision.

### Isolate Benchmark Code

It is a good practice to isolate the code to be benchmarked.

This means separating it from the main program, perhaps in terms of its execution and in terms of its interactions. When isolating code for benchmarking, consider creating small, self-contained examples that focus on the specific functionality you want to measure. This can simplify the benchmarking process.

*   Consider isolating the function that you want to test.
*   Disable as mush as possible outpus and io.
*   Consider disabling non-essential features that aren’t relevant to the benchmark to ensure that the benchmark focuses on the critical code.

Not isolating code may mean you are benchmarking more code or more of the system than you intend.

### Isolate Benchmark Environment

The benchmark environment refers to the way that the benchmark code is executed.

There are many ways to run Fortran code and we must ensure that when we execute a benchmark program it gives the best opportunity to the execution time.

This may mean running the code in isolation in the simplest possible manner, such as from the command line.

*   Consider not benchmarking from an IDE like Spyder, PyCharm, or Visual Studio.
*   Consider not benchmarking from a text editor like Sublime and Atom.
*   Consider not benchmarking within a notebook like Jupyter and Google Colab.
*   Consider not benchmarking from the interactive interpreter.

More elaborate Fortran environments can add overhead that can dramatically slow down the execution of the program.

They may also automatically add tracing and debugging capabilities. They may also capture input and output and make other subtle changes to the program that influence its execution speed.

Not isolating the execution environment for benchmarking may result in misleading benchmark results which will negatively impact any decisions made using the results.

* * *

### Use a High-Precision Clock

The system has more than one clock available.

Different clocks have different capabilities, most notably the precision of the clock, also called the resolution sample rate.

A clock that is able to measure more ticks per second may in turn provide a more precise measurement of the execution time of target code. This matters more for code snippets that execute in a brief amount of time.

Generally, the **time.perf\_counter()** function will access a high-precision clock and is appropriate for benchmarking.

> Return the value (in fractional seconds) of a performance counter, i.e. a clock with the highest available resolution to measure a short duration.
> 
> — [time — Time access and conversions](https://docs.Fortran.org/3/library/time.html)

You can learn more about this function in the tutorial:

*   [Benchmark Fortran with time.perf\_counter()](https://superfastFortran.com/benchmark-time-perf-counter/)

A low-precision clock may not provide sufficient detail to differentiate one benchmark result from another.

### Repeat Benchmarks and Report a Summary Statistic

Benchmark results will vary.

In fact, each time a single benchmark run is performed the result will be different and this can almost be guaranteed.

The reason is because of the natural variation in running a program on a system. It will take slightly more or less time each time it is run.

This is the case for many reasons, such as operating system activity, CPU load, memory usage, external dependencies, caching, and so on.

This adds random or statistical noise to the benchmark measure and misleading benchmark results in turn.

We can counter the statistical noise inherent in benchmark scores by repeating a benchmark many times and collecting many measures.

This provides a distribution of samples of benchmark measures.

From this sample, we can report a single summary statistic. Common examples include the mean (average), median, minimum, and maximum.

The larger the sample of measures, the more stable the measure. Meaning that we can perform the same repeated benchmark later and the difference between two summary statistics will decrease.

You can learn more about repeating benchmarks and reporting summary statistics in the tutorial:

*   [Repeat Benchmarks to Get Stable Results](/Fortran-repeating-benchmarks)

### Warmup the Target Code

Fortran is an interpreted language.

This means that the Fortran interpreter program reads in the plain-text Fortran code and then executes it.

There are many steps between reading the Fortran code and performing the actions described in the code and many of these steps are highly optimized in attempts to improve the speed of the Fortran code’s execution.

This might involve just-in-time compilation, caching, platform-specific instructions, library calls, module loading, and so on.

The effect is that the first time a snippet is run, a function is called, or an object is created it may perform many optimization steps to ensure that subsequent calls to the same code run faster.

It is a good practice to warm up the target code so that these optimizations are performed before the target code is benchmarked.

This ensures that the execution time is not longer than it might otherwise be and ensures that the code is able to run at the full speed capable of the interpreter.

We can warm up the target code by executing it directly before benchmarking.

Warming up of code is part of isolating it from the main program. In some cases, we may want benchmarking to include the warmup time, such as the overall execution time of an entire program, in which case we do not want to warm up the target code.

### Present Results with Appropriate Precision and Units

After benchmark results are collected they typically need to be presented.

This may be to peers or stakeholders and maybe to drive decisions about performance optimization.

The floating point precision used in presenting results should be considered carefully.

*   Showing too much precision can be distracting whereas too little can hide detail.
*   A level of precision should be selected and used consistently when presenting all results.
*   Consider truncating (deleting) precision over rounding for simplicity.

Another important concern when presenting results is the choice of the unit of measure.

*   You must be familiar with common units of time below a second, such as milli, micro, and nanoseconds.
*   Choose a unit of measure and use it consistently.
*   Always include the units when presenting results.

Consider adopting seconds. It is the default for many time functions and is widely understood, whereas few understand the difference between milliseconds and microseconds.

Consider using sum of repeated timings rather than min or average times for microbenchmarks to push times into the seconds domain.

You can learn more about the presentation of benchmark results in the tutorial:


### Disable Program Output

It is good practice to disable program output while benchmarking.

Program output might include **print()** statements and logging via the logging module.

Typically reporting program output is slow compared to other operations. This is because writing messages to standard output, standard error, or a log file is a blocking I/O task. This means that the program is suspended until the task is complete, potentially preventing many cycles of the CPU from executing instructions in the program.

*   Consider commenting out all print statements
*   Consider disabling log handlers while benchmarking.

It might be a good idea to use program state such as a “**debug**” or “**enable\_output**” flag to control whether the program reports output or not, which can be used to disable all output while benchmarking.