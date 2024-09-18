# Compiler Differences {#compiler_differences}

While developing this library I experienced some differences between compilers (i.e. ifort and gfortran). 
Not in terms of performances, which is kind of expected, but in terms of implementation of some modern Fortran 
features. The *benchmark.f* library uses some dark corner of the Fortran language to get some kind of generic 
programming. As a consequence, it seams that different compilers have various interpretation of these edge cases in the standard. 
The developers of the f18 fortran compiler already compiled a [list](https://github.com/klausler/fortran-wringer-tests) of features that, even-though standard-conformant, 
are not portable due to different interpretation of the standard. Some of the code used here is part of this list and I will do my best to provide workaround to make it work on both tested compilers.

## Implicit procedure pointer

Since Fortran 2003, it is possible to declare procedure pointers as function argument. The declaration is usually something like this 
```fortran 
procedure(interface), intent(in) :: ptr
```
It is also possible to omit the interface in the procedure declaration statement. `procedure()` with no type or interface name is 
supposed to work as a dummy procedure or procedure pointer that can handle either a function or a subroutine, so long as it isn't 
referenced. Many compilers assume that it means a subroutine, or just can't deal with it. gfortran is one of them.
 
While this works fine with ifort, gfortran does not accept the use of `function`s as argument. 
Therefore, when using gfortran, you are limited to benchmarking `subroutine`s. 

## Associate and unlimited polymorphism

Procedure arguments are stored for later use as unlimited polymorphic variables (`class(*)`). 
When used in combination with the `associate` construct ifort and gfortran behave differently. 

While ifort is capable of implicitly casting the variable into the desired type, gfortran doesn't and
there one is forced to use the `select type` construct. 

For instance, one can try this simple function call: 

```fortran
    class(*), intent(in)            :: a1

    associate(arg1 => a1)
        call func(arg1)
    end associate
```

with the function 
```fortran
subroutine func(n)
    integer, intent(in) :: n
    print*, n
end subroutine
```

The argument `n` is the desired integer value when the code is compiled with ifort, while gfortran returns a random value(maybe the address of the variable?)

## Writing namelist to internal file

gfortran does not support writing `namelist` to internal files. As such the option is only available when compiled 
with ifort. Preprocessor macros and conditional compilation are used to provide this option to ifort
```fortran 
#ifdef __INTEL_COMPILER
    subroutine benchmark_serialize_to_string(this, str)
        class(runner), intent(in), target       :: this
        character(:), allocatable, intent(out)  :: str
        !private
        type(runner), pointer :: bench => null()
        namelist / config / bench
        allocate(character(100) :: str)
            
        bench => this
                 
        write(str, nml=config)
            
        str = trim(str)
        nullify(bench)
    end subroutine
#endif
``` 

## Example

Here is an example that illustrate how the same benchmark can be written for both compilers: 

The test program is the same, irrespective of the compiler used.
```fortran
#include <benchmark.inc>
program poisson
    use rhofunc
    use benchmark_library
    
    implicit none
    
    block
        type(runner) :: br

        benchmark(br, run(1.0d-6, 30, poisson_optimized))
    end block
    read(*,*)
end program
```

### gfortran

```fortran
    pure subroutine poisson_optimized(treshold, m)
        class(*), intent(in) :: treshold
        class(*), intent(in)  :: m
        cast(treshold, real(rp))
            cast(m, integer)
                block
                    !private
                    integer :: i,j, iter
                    real(rp) :: delta, phiprime(m,m), phi(m,m), a2, rhoarr(m,m)
                    real(rp), parameter :: quart = 0.25_rp

                    delta = 1.0_rp
                    iter = 0
                    phiprime(:,:) = 0.0_rp
                    phi(:,:) = 0.0_rp
                    do i=1, m
                        do j=1, m
                            rhoarr(i,j) = rho(i*a,j*a)
                        end do
                    end do
                
                    do while (delta > treshold )
                        iter = iter + 1
                        a2 = a**2
                        do i=2, m-1
                            do j=2, m-1
                                phiprime(i,j) = (phi(i+1,j) + phi(i-1,j) + phi(i,j+1) + phi(i,j-1))*quart &
                                + a2*quart/epsilon0*rhoarr(i,j)
                            end do
                        end do
                        delta = maxval(abs(phiprime - phi))
                        phi = phiprime 
                    end do
                end block
            endcast
        endcast
    end subroutine
    
    pure real(rp) function rho(x,y)
        real(rp), intent(in) :: x,y
        if (x > 0.6_rp .and. x < 0.8_rp .and. y > 0.6_rp .and. y < 0.8_rp) then
            rho = 1.0_rp
        else if (x > 0.2_rp .and. x < 0.4_rp .and. y > 0.2_rp .and. y < 0.4_rp) then
            rho = -1.0_rp
        else
            rho = 0.0_rp
        end if
    end function
```

### ifort

```fortran
    pure subroutine poisson_optimized(treshold, m)
        real(rp), intent(in) :: treshold
        integer, intent(in)  :: m
        !private
        integer :: i,j, iter
        real(rp) :: delta, phiprime(m,m), phi(m,m), a2, rhoarr(m,m)
        real(rp), parameter :: quart = 0.25_rp

        delta = 1.0_rp
        iter = 0
        phiprime(:,:) = 0.0_rp
        phi(:,:) = 0.0_rp
        do i=1, m
            do j=1, m
                rhoarr(i,j) = rho(i*a,j*a)
            end do
        end do
    
        do while (delta > treshold )
            iter = iter + 1
            a2 = a**2
            do i=2, m-1
                do j=2, m-1
                    phiprime(i,j) = (phi(i+1,j) + phi(i-1,j) + phi(i,j+1) + phi(i,j-1))*quart &
                    + a2*quart/epsilon0*rhoarr(i,j)
                end do
            end do
            delta = maxval(abs(phiprime - phi))
            phi = phiprime 
        end do
    end subroutine
    
    pure real(rp) function rho(x,y)
        real(rp), intent(in) :: x,y
        if (x > 0.6_rp .and. x < 0.8_rp .and. y > 0.6_rp .and. y < 0.8_rp) then
            rho = 1.0_rp
        else if (x > 0.2_rp .and. x < 0.4_rp .and. y > 0.2_rp .and. y < 0.4_rp) then
            rho = -1.0_rp
        else
            rho = 0.0_rp
        end if
    end function
```