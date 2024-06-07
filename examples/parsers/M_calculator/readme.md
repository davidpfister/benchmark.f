[![](docs/images/calc_small.ico)](https://urbanjost.github.io/M_calculator/docs/fpm-ford/index.html)
# [M_calculator](https://urbanjost.github.io/M_calculator/docs/M_calculator.3.html)

## parse mathematical expressions

   The M_calculator(3f) module contains a basic expression parser and
   related convenience routines.

   Expressions are similar to Fortran77 syntax except
   + powers are processed from left to right so use parenthesis
   + variable names are case-sensitive
   + string variable names start with a dollar-sign,
   + all numeric values are assumed to be DOUBLEPRECISION.

   Note that although suitable for many uses, this is not a byte-code
   interpreter and is designed for parsing basic unique expressions,
   not for high volume repetition of the computations on a large volume
   of data.

   The original dates back a long time, but it still works.

## Example ![example](docs/images/demo.gif)

```fortran
program demo_dnum0
use M_calculator, only: dnum0
implicit none
doubleprecision              :: x, y, z, answer
integer                      :: i
character(len=:), allocatable :: strings(:)

   strings = [character(len=128) :: &
   '20/3.4                       ', &
   'CI=10*sin(3.1416/4)          ', &
   'a=CI**2+sqrt(90)             ', &
   'x=a/CI-atan(a)               ', &
   'y=x+CI/333                   ', &
   '                             ']

   do i = 1, size(strings)
      answer = dnum0(strings(i))
      write(*,'(*(g0))')answer,' = ',strings(i)
   enddo

   x = dnum0('x')
   y = dnum0('y')
   z = dnum0('CI')
   write (*, *) x, y, z

end program demo_dnum0
```
Results:
```text
 > 5.8823529411764710 = 20/3.4
 > 7.0710807985947346 = CI=10*sin(3.1416/4)
 > 59.487016640760288 = a=CI**2+sqrt(90)
 > 6.8587316018816180 = x=a/CI-atan(a)
 > 6.8799660787542747 = y=x+CI/333
 > 0.0000000000000000 =
 >    6.8587316018816180        6.8799660787542747        7.0710807985947346
```
## Documentation   ![docs](docs/images/docs.gif)

 - The [user documentation](docs/M_calculator.3.md) describes the many functions available.

 - The code was run through [ford(1)](https://politicalphysicist.github.io/ford-fortran-documentation.html)
   to produce a [developers' document](https://urbanjost.github.io/M_calculator/docs/fpm-ford/index.html).

 - [github action status](docs/STATUS.md)

## Build with gmake(1) ![gmake](docs/images/gnu.gif)
```bash
    git clone https://github.com/urbanjost/M_calculator.git
    cd M_calculator/src
    # change Makefile if not using one of the listed compilers

    # for gfortran
    make clean
    make F90=gfortran gfortran

    # for ifort
    make clean
    make F90=ifort ifort

    # for nvfortran
    make clean
    make F90=nvfortran nvfortran
```
   This will compile the M_calculator module and build all the example
   programs from the document pages in the PROGRAMS/ sub-directory.

## Build with  fpm(1) ![fpm](docs/images/fpm_logo.gif)
   #### (registered at the [fpm(1) registry](https://github.com/fortran-lang/fpm-registry) )

   Alternatively, download the github repository and build it with
   fpm ( as described at [Fortran Package Manager](https://github.com/fortran-lang/fpm) )

```bash
    git clone https://github.com/urbanjost/M_calculator.git
    cd M_calculator
    fpm build
    fpm test
```
   or just list it as a dependency in your fpm.toml project file.
```toml
    [dependencies]
    calculator        = { git = "https://github.com/urbanjost/M_calculator.git" }
```
[![M_calculator](docs/images/calcs.gif)](https://github.com/urbanjost/M_calculator/blob/master/docs/M_calculator.3.md)
--
[Find M_calculator on github](https://github.com/urbanjost/M_calculator.git)

