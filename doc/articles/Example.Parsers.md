# Equation Parsers {#example_parsers}

|Name|Link|
|:---|:---|
|BiF-lib|[https://code.usgs.gov/fortran/bif](https://code.usgs.gov/fortran/bif)|
|evaluate|[https://gbenthien.net/strings/index.html](https://gbenthien.net/strings/index.html)|
|feq-parse|[https://github.com/fluidnumerics/feq-parse](https://github.com/fluidnumerics/feq-parse)|
|fortran_function_parser|[https://github.com/jacobwilliams/fortran_function_parser](https://github.com/jacobwilliams/fortran_function_parser)|
|fortran_parser|[https://github.com/sdm900/fortran_parser](https://github.com/sdm900/fortran_parser)|
|FortranParser|[https://github.com/jacopo-chevallard/FortranParser](https://github.com/jacopo-chevallard/FortranParser)|
|fparser|[http://fparser.sourceforge.net](http://fparser.sourceforge.net)|
|Fortran-Expression-Evaluator|[https://github.com/ivomarb/Fortran-Expression-Evaluator](https://github.com/ivomarb/Fortran-Expression-Evaluator)|
|M_calculator|[https://urbanjost.github.io/M_calculator](https://urbanjost.github.io/M_calculator)|
|Symengine|[https://github.com/symengine/symengine.f90](https://github.com/symengine/symengine.f90)|

## Expressions 

@f[
'a+b*x1' 
'(a*x**b)/(c+x**b)'
'(a*x)/(b+(x*(1+x/c)))'
'a+b*exp(c*x)+d*exp(e*x)'
'a+b*(exp(c*x) - 1)/c'
'a+b*log(x)+c*log(x)**2'
'a-log(1+b*exp(-c*x))'
'(a+b*x)/(c+x)'
'a+b*exp(-(c*x))'
'a+b*sin(2*3.14*x/c+d)'
'a+b*sin(2*4.14*x/c+d)**2'
'1-exp(-a*x)'
'a+b*x1+c*x2'
'a+b*log(x1)+c*log(x2)'
'a*x1**b*x2**c'
'cosh(log(abs(y*z+x**2+x1**x2)))+a*d*(exp(c*f)+154.3)'
'a+b*log(x1)+c*x2+d*x2**2'
'atan(sinh(log(abs(exp(z/x)*sqrt(y+a**c+f*e)))))'
'a+b/x1+c*log(x2)+d*log(x2)**2+e*log(x2)**3'
'atan(sinh(log(abs(exp(z/x)*sqrt(y+a**c+f*e)))))*cos(log(abs(sqrt(y+a**c+f*e))))'
'a+b*log(x1)+c*log(x1)**2+d/x2+e/x2**2'
'(x+a)/(b+c*(x+a)+d*(x+a)**2)'
'a+b*log(x1)+c*log(x1)**2+d*log(x1)**3+e/x'
@f]

## Prerequisite

For convenience all fortran equation parsers have been added to the repo. 
However, symengine only contains the Fortran wrapper and the [C++ library](https://github.com/symengine/symengine)
needs to be installed on the computer.

## Notes

Some parsers have been modified slightly to pass this test. For instance, support to some intrinsic functions have been added.
I also contributed to [feq-parse](https://github.com/fluidnumerics/feq-parse) and [Symengine](https://github.com/symengine/symengine.f90)
to fix some of the bugs I could find during the execution of this benchmark.