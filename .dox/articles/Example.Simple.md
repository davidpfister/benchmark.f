# Pi Calculation {#example_simple}

This simple example implements the calculation of `pi` using the Gregory-Leibniz series
@f[
    \frac{pi}{4}=1-\frac{1}{3}+\frac{1}{5}-\frac{1}{7}+\frac{1}{9}-...=\sum_{k=0}^{\infty}\frac{(-1)^k}{2k+1}
@f]

The sum is computed for various number of terms from 10000000 to 200000000
```fortran
    benchmark(br, run(10000000, compute_pi))
    benchmark(br, run(100000000, compute_pi))
    benchmark(br, run(200000000, compute_pi))
```

The results are as follows: 
<center>
|              Method Name                      |          Mean          |    Standard Deviation  |
|:----------------------------------------------|:-----------------------|:-----------------------|
|compute_pi(10000000)                           |               86.675 ms|            +/- 5.592 ms|
|compute_pi(100000000)                          |              849.100 ms|           +/- 25.177 ms|
|compute_pi(200000000)                          |                1.708  s|            +/- 0.047  s|
</center>