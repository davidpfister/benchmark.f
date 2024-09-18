# Allocation {#example_allocation}

This example test different ways to allocate an array of integer from 0 to n. 
Four different implementations are tested: 

Preallocation 
```fortran 
allocate(array(n))
do i = 1, n
    array(i) = i
end do
```

Array contructor
```fortran
array = [(i, i = 1,n)]
```

Dynamic reallocation 
```fortran
allocate(array(0))
do i = 1, n
    array = [array, i]
end do
```

And a method using buffers based on the following [example](https://degenerateconic.com/dynamically-sizing-arrays.html)

The results are as follows: 

<center>
|              Method Name                      |          Mean          |    Standard Deviation  |
|:----------------------------------------------|:-----------------------|:-----------------------|
|array_preallocation(50000)                     |              114.112 us|            +/- 5.940 us|
|array_preallocation(100000)                    |              220.971 us|            +/- 6.818 us|
|array_constructor(50000)                       |               99.854 us|            +/- 4.982 us|
|array_constructor(100000)                      |              192.367 us|            +/- 2.191 us|
|naive_reallocation(50000)                      |                3.483  s|            +/- 0.227  s|
|naive_reallocation(100000)                     |               13.755  s|            +/- 1.131  s|
|buffer_reallocation(100,50000)                 |                8.004 ms|            +/- 0.170 ms|
|buffer_reallocation(100,100000)                |               30.195 ms|            +/- 0.631 ms|
|buffer_reallocation(1000,50000)                |                1.302 ms|            +/- 0.029 ms|
|buffer_reallocation(1000,100000)               |                4.468 us|            +/- 0.059 ms|
</center>