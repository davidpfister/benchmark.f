# Poisson Equation {#example_poisson}

This example was directly taken from [fortran-lang](https://github.com/fortran-lang/benchmarks/tree/main/poisson2d).
It proposes to benchmark different implementations of a solver for the 2D Poisson equation.
The solver uses a simple Jacobi iteration methods. For details, see, "Computational Physics" by Mark Newman, Chap 9.

The benchmark consists in testing two different implementations in Fortran for a given tolerance (10^-6) and a 
given number of grid point (30).

```fortran 
    benchmark(br, run(1.0d-6, 30, poisson_naive))  
    benchmark(br, run(1.0d-6, 30, poisson_optimized))
```

The results are as follows: 
<center>
|              Method Name                      |          Mean          |    Standard Deviation  |
|:----------------------------------------------|:-----------------------|:-----------------------|
|poisson_naive(1.0d-6,30)                       |              161.750 ms|           +/- 11.252 ms|
|poisson_optimized(1.0d-6,30)                   |               93.025 ms|            +/- 5.981 ms|
</center>