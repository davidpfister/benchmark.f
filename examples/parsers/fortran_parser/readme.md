*********************************************************************

This software is provided AS IS with NO WARRANTY.

*********************************************************************

Hi Fortran Programmers

OK, the function interpreter is now finished (at least to a level that
satisfies me).

PLEASE, this code comes AS IS without any warranty of any kind.  I have
developed this code enough to satisfy my own needs and no body elses. 
If you find a use for it, well and good.  If you manage to improve on
it, please me know and I will put the improvements in my own code.

The interpreter does 61 functions 

    cos(x)
    sin(x)
    tan(x)
    exp(x)
    log(x)
    log10(x)
    logn(x,y)
    sqrt(x)
    cbrt(x)
    acos(x)
    asin(x)
    atan(x)
    cosh(x)
    sinh(x)
    tanh(x)
    anint(x)
    aint(x)
    abs(x)
    delta(x)
    step(x)
    hat(x)
    min(x,y)
    max(x,y)
    besj0(x)
    besj1(x)
    besjn(n,x)
    besy0(x)
    besy1(x)
    besyn(n,x)
    besi0(x)
    besi1(x)
    besin(n,x)
    besk0(x)
    besk1(x)
    beskn(n,x)
    erf(x)
    erfc(x)
    ierf(x)
    ierfc(x)
    gamma(x)
    lgamma(x)
    csch(x)
    sech(x)
    coth(x)
    if(conditional, true, false)
    gauss(x)
    sinc(x)
    fresc(x)
    fress(x)
    expi(x)
    sini(x)
    cosi(x)
    logi(x)
    elle(x)
    ellk(x)
    ielle(x, phi)
    iellf(x, phi)
    modulo(x,y)
    mod(x,y)
    floor(x)
    ceiling(y)

16 operators

    +
    -
    *
    /
    //		Integer Divide
    %		Integer modulo
    **
    ^
    >
    >=
    =>
    <
    <=
    =<
    ==
    =
    !=

brackets and correct order of operation.

Numbers are all assumed to be double precision real and are entered in
the usual fortran double precision way (or any way that read(*,*) num
will interpret)

# = integer

	#
	.#
	#.#
	#.#d#
	#.#d-#
	#.#d+#
	#.#e#
	#.#e-#
	#.#e+#
	#.#D#
	#.#D-#
	#.#D+#
	#.#E#
	#.#E-#
	#.#E+#

etc...

The functions are simply strings with any number of possible variables



****  THE CODE NOW HAS ERROR DETECTION  ****

Yes, I got around to adding it at the request of a user of my code.
Basically, the code will try to detect when an illegal function is
entered.  It is quite basic, but seems to work quite well.

Basically, when calling the function, you have the option of
including an error field ( character(len=#)::error ).  If no error
occurs, this error field will contain OK.  If an error is detected
then error will contain the first # characters of the error explanation.

It will still return a valid number (that obtained previously) but you
can at least have some primitive idea of what went wrong.

********************************************



****  THE CODE NOW DOES ERROR LIMIT COMPUTATIONS  ****

The code now allows the computation of error calculations of the form

	D = delta (error)
	d = partial derivative

	Df = df/dx Dx + df/dy Dy

You input the variables using a square bracket

	x[0.1]

would give an error of 0.1 on the variable x.

******************************************************



Anyway, the code is not documented, though it should be really easy to
follow.



KNOWN PROBLEMS

No known problems.


If you have any problems, let me know.

Stu.

## To use

The code has been broken up into fairly simple pieces, which should allow
great flexibility and speed.

```fortran
subroutine s_createfn
subroutine s_evaluatefn
subroutine s_evaluateerr
subroutine s_destroyfn
```

The Fortran90 Interface blocks would look like

```fortran
interface


	subroutine s_createfn(func, vars, funcnumber, error)
		character(len = *), intent(in) :: func, vars
		integer, intent(out) :: funcnumber
		character(len=*), intent(out), optional :: error
	end subroutine


	subroutine s_evaluatefn(funcnumber, varvals, number, error)
		integer, intent(in) :: funcnumber
		real(fdp), dimension(:), intent(in) :: varvals
		real(fdp), intent(out) :: number
		character(len=*), intent(out), optional :: error
	end subroutine


	subroutine s_evaluateerr(funcnumber, varvals, number, error)
		integer, intent(in) :: funcnumber
		real(fdp), dimension(:), intent(in) :: varvals
		real(fdp), intent(out) :: number
		character(len=*), intent(out), optional :: error
	end subroutine


	subroutine s_destroyfn(funcnumber)
		integer, intent(in) :: funcnumber
	end subroutine


	end interface
```

where

	fdp = 8		Double precision float

So, as an example you could do

```	fortran
read(*,'(a)') f1

	call s_createfn(f1, 'x[0.1] y[0.01] z[0.02]', funcnum1, error)

	write(*,*) trim(error)

	call s_evaluatefn(funcnum1, (/1.0d0, 2.0d0, 3.0d0/), r, error)

	write(*,*) r

	call s_evaluateerr(funcnum1, (/1.0d0, 2.0d0, 3.0d0/), e, error)

	write(*,*) e

	call s_destroyfn(funcnum1)
    ```



Once you have created a function and have a function number for it, that function will remained stored in memory in an optimised fashon until you destroy it or exit the program.

The basic idea is that you load in a function from some input (STDIN or text file) and call s_createfn which basically parses the text function into an internal representation for fast execution.  Then, whenever you need that function computed for various values of the input variables, you call s_evaluatefn.  Once you have finished, you call s_destroyfn and exit the program.



