# Equation Parsers {#example_parsers}

The following examples benchmarks equations parsers. 
Several codes are available for parsing text equations and evaluating the results. Most of then have 
been compiled here. The source code are embedded in this project for the sake of simplicity.
The different parsers are reported here.

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

The parser from [Bif](https://code.usgs.gov/fortran/bif) has been excluded since it cannot handle all equations.

## Expressions 

The parsers have been tested against the following expressions
@f[
    a+b*x1
@f] 
@f[
    (a*x**b)/(c+x**b)
@f]    
@f[
    (a*x)/(b+(x*(1+x/c)))
@f]
@f[
    a+b*exp(c*x)+d*exp(e*x)
@f]    
@f[
    a+b*(exp(c*x) - 1)/c
@f]
@f[
    a+b*log(x)+c*log(x)**2
@f]    
@f[
    a-log(1+b*exp(-c*x))
@f]    
@f[
    (a+b*x)/(c+x)
@f]
@f[
    a+b*exp(-(c*x))
@f]
@f[
    a+b*sin(2*3.14*x/c+d)
@f]    
@f[
    a+b*sin(2*4.14*x/c+d)**2
@f]
@f[
    1-exp(-a*x)
@f]
@f[
    a+b*x1+c*x2
@f]
@f[
    a+b*log(x1)+c*log(x2)
@f]
@f[
    a*x1**b*x2**c
@f]
@f[
    cosh(log(abs(y*z+x**2+x1**x2)))+a*d*(exp(c*f)+154.3)
@f]
@f[
    a+b*log(x1)+c*x2+d*x2**2
@f]
@f[
    atan(sinh(log(abs(exp(z/x)*sqrt(y+a**c+f*e)))))
@f]
@f[
    a+b/x1+c*log(x2)+d*log(x2)**2+e*log(x2)**3
@f]
@f[
    atan(sinh(log(abs(exp(z/x)*sqrt(y+a**c+f*e)))))*cos(log(abs(sqrt(y+a**c+f*e))))
@f]
@f[
    a+b*log(x1)+c*log(x1)**2+d/x2+e/x2**2
@f]
@f[
    (x+a)/(b+c*(x+a)+d*(x+a)**2)
@f]
@f[
    a+b*log(x1)+c*log(x1)**2+d*log(x1)**3+e/x
@f]

## Prerequisite

For convenience all fortran equation parsers have been added to the repo. 
However, symengine test folder only contains the Fortran wrapper and 
the [C++ library](https://github.com/symengine/symengine)
needs to be installed on the computer. Linking to the symengine library might be tedious so 
it is excluded in standard build. To be included in the project, simply define the preprocessor macro `_INCLUDE_SYMENGINE`

## Notes

Some parsers have been modified slightly to pass this test. For instance, support to some intrinsic functions have been added.
I also contributed to [feq-parse](https://github.com/fluidnumerics/feq-parse) and [Symengine](https://github.com/symengine/symengine.f90)
to fix some of the bugs I could find during the execution of this benchmark.

The results are as follows: 

<center>
|              Method Name                      |          Mean          |    Standard Deviation  |
|:----------------------------------------------|:-----------------------|:-----------------------|
|evaluate(a+b*x1)                               |                2.586 us|            +/- 0.634 us|
|evaluate((a*x**b)/(c+x**b))                    |                4.916 us|            +/- 1.338 us|
|evaluate((a*x)/(b+(x*(1+x/c))))                |                5.606 us|            +/- 1.311 us|
|evaluate(a+b*exp(c*x)+d*exp(e*x))              |                6.440 us|            +/- 1.688 us|
|evaluate(a+b*(exp(c*x) - 1)/c)                 |                5.831 us|            +/- 1.549 us|
|evaluate(a+b*log(x)+c*log(x)**2)               |                6.119 us|            +/- 0.982 us|
|evaluate(a-log(1+b*exp(-c*x)))                 |                5.307 us|            +/- 0.983 us|
|evaluate((a+b*x)/(c+x))                        |                4.104 us|            +/- 0.619 us|
|evaluate(a+b*exp(-(c*x)))                      |                4.156 us|            +/- 0.932 us|
|evaluate(a+b*sin(2*3.14*x/c+d))                |                6.578 us|            +/- 1.550 us|
|evaluate(a+b*sin(2*4.14*x/c+d)**2)             |                8.833 us|            +/- 1.161 us|
|evaluate(1-exp(-a*x))                          |                3.559 us|            +/- 0.511 us|
|evaluate(a+b*x1+c*x2)                          |                4.198 us|            +/- 0.730 us|
|evaluate(a+b*log(x1)+c*log(x2))                |               14.703 us|            +/- 3.097 us|
|evaluate(a*x1**b*x2**c)                        |                6.359 us|            +/- 1.274 us|
|evaluate(cosh(log(abs(y*z+x**2+x1**x2)))+a*d*(e|               20.949 us|            +/- 6.246 us|
|evaluate(a+b*log(x1)+c*x2+d*x2**2)             |               13.285 us|            +/- 4.749 us|
|evaluate(atan(sinh(log(abs(exp(z/x)*sqrt(y+a**c|               14.542 us|            +/- 3.054 us|
|evaluate(a+b/x1+c*log(x2)+d*log(x2)**2+e*log(x2|               19.209 us|            +/- 2.870 us|
|evaluate(atan(sinh(log(abs(exp(z/x)*sqrt(y+a**c|               40.904 us|            +/- 3.973 us|
|evaluate(a+b*log(x1)+c*log(x1)**2+d/x2+e/x2**2)|               24.728 us|            +/- 2.900 us|
|evaluate((x+a)/(b+c*(x+a)+d*(x+a)**2))         |               19.994 us|            +/- 2.680 us|
|evaluate(a+b*log(x1)+c*log(x1)**2+d*log(x1)**3+|               18.486 us|            +/- 6.060 us|
|feq-parse(a+b*x1)                              |               19.805 us|            +/- 1.686 us|
|feq-parse((a*x**b)/(c+x**b))                   |               29.082 us|            +/- 5.884 us|
|feq-parse((a*x)/(b+(x*(1+x/c))))               |               23.555 us|            +/- 2.652 us|
|feq-parse(a+b*exp(c*x)+d*exp(e*x))             |               26.553 us|            +/- 6.269 us|
|feq-parse(a+b*(exp(c*x) - 1)/c)                |               24.299 us|            +/- 1.400 us|
|feq-parse(a+b*log(x)+c*log(x)**2)              |               28.809 us|            +/- 6.347 us|
|feq-parse(a-log(1+b*exp(-c*x)))                |               45.469 us|            +/- 7.019 us|
|feq-parse((a+b*x)/(c+x))                       |               26.969 us|            +/- 4.053 us|
|feq-parse(a+b*exp(-(c*x)))                     |               25.241 us|            +/- 3.578 us|
|feq-parse(a+b*sin(2*3.14*x/c+d))               |               31.317 us|            +/- 5.705 us|
|feq-parse(a+b*sin(2*4.14*x/c+d)**2)            |               34.406 us|            +/- 4.805 us|
|feq-parse(1-exp(-a*x))                         |               32.159 us|            +/- 6.998 us|
|feq-parse(a+b*x1+c*x2)                         |               34.427 us|            +/- 9.085 us|
|feq-parse(a+b*log(x1)+c*log(x2))               |               41.527 us|            +/- 6.458 us|
|feq-parse(a*x1**b*x2**c)                       |               44.218 us|            +/- 5.837 us|
|feq-parse(cosh(log(abs(y*z+x**2+x1**x2)))+a*d*(|               65.645 us|            +/- 9.550 us|
|feq-parse(a+b*log(x1)+c*x2+d*x2**2)            |               57.594 us|            +/- 8.763 us|
|feq-parse(atan(sinh(log(abs(exp(z/x)*sqrt(y+a**|               46.070 us|           +/- 10.899 us|
|feq-parse(a+b/x1+c*log(x2)+d*log(x2)**2+e*log(x|               56.017 us|           +/- 10.354 us|
|feq-parse(atan(sinh(log(abs(exp(z/x)*sqrt(y+a**|               72.657 us|           +/- 16.329 us|
|feq-parse(a+b*log(x1)+c*log(x1)**2+d/x2+e/x2**2|               63.608 us|           +/- 14.936 us|
|feq-parse((x+a)/(b+c*(x+a)+d*(x+a)**2))        |               55.256 us|            +/- 5.214 us|
|feq-parse(a+b*log(x1)+c*log(x1)**2+d*log(x1)**3|               43.900 us|            +/- 9.802 us|
|function_parser(a+b*x1)                        |               95.763 us|           +/- 12.425 us|
|function_parser((a*x**b)/(c+x**b))             |              186.814 us|           +/- 31.616 us|
|function_parser((a*x)/(b+(x*(1+x/c))))         |              160.901 us|           +/- 16.074 us|
|function_parser(a+b*exp(c*x)+d*exp(e*x))       |              168.758 us|           +/- 29.687 us|
|function_parser(a+b*(exp(c*x) - 1)/c)          |              105.625 us|           +/- 12.056 us|
|function_parser(a+b*log(x)+c*log(x)**2)        |              108.502 us|            +/- 9.634 us|
|function_parser(a-log(1+b*exp(-c*x)))          |              105.747 us|           +/- 14.172 us|
|function_parser((a+b*x)/(c+x))                 |              107.241 us|           +/- 23.449 us|
|function_parser(a+b*exp(-(c*x)))               |               87.101 us|            +/- 8.915 us|
|function_parser(a+b*sin(2*3.14*x/c+d))         |              106.555 us|            +/- 9.720 us|
|function_parser(a+b*sin(2*4.14*x/c+d)**2)      |              119.256 us|           +/- 14.663 us|
|function_parser(1-exp(-a*x))                   |               48.107 us|            +/- 5.244 us|
|function_parser(a+b*x1+c*x2)                   |              115.224 us|           +/- 13.981 us|
|function_parser(a+b*log(x1)+c*log(x2))         |              107.293 us|            +/- 8.260 us|
|function_parser(a*x1**b*x2**c)                 |               98.038 us|            +/- 7.644 us|
|function_parser(cosh(log(abs(y*z+x**2+x1**x2)))|              230.301 us|           +/- 22.109 us|
|function_parser(a+b*log(x1)+c*x2+d*x2**2)      |              173.947 us|           +/- 14.479 us|
|function_parser(atan(sinh(log(abs(exp(z/x)*sqrt|              145.872 us|           +/- 13.497 us|
|function_parser(a+b/x1+c*log(x2)+d*log(x2)**2+e|              221.897 us|           +/- 20.909 us|
|function_parser(atan(sinh(log(abs(exp(z/x)*sqrt|              252.734 us|           +/- 30.734 us|
|function_parser(a+b*log(x1)+c*log(x1)**2+d/x2+e|              238.554 us|           +/- 26.266 us|
|function_parser((x+a)/(b+c*(x+a)+d*(x+a)**2))  |              199.317 us|           +/- 19.768 us|
|function_parser(a+b*log(x1)+c*log(x1)**2+d*log(|              226.340 us|           +/- 25.530 us|
|interpreter(a+b*x1)                            |              127.306 us|           +/- 44.878 us|
|interpreter((a*x**b)/(c+x**b))                 |              237.543 us|           +/- 27.593 us|
|interpreter((a*x)/(b+(x*(1+x/c))))             |              335.762 us|           +/- 25.505 us|
|interpreter(a+b*exp(c*x)+d*exp(e*x))           |              389.391 us|           +/- 28.436 us|
|interpreter(a+b*(exp(c*x) - 1)/c)              |              456.541 us|           +/- 41.906 us|
|interpreter(a+b*log(x)+c*log(x)**2)            |              581.376 us|           +/- 63.313 us|
|interpreter(a-log(1+b*exp(-c*x)))              |              592.330 us|           +/- 54.073 us|
|interpreter((a+b*x)/(c+x))                     |              590.905 us|           +/- 63.191 us|
|interpreter(a+b*exp(-(c*x)))                   |              530.015 us|           +/- 29.928 us|
|interpreter(a+b*sin(2*3.14*x/c+d))             |              609.687 us|           +/- 42.470 us|
|interpreter(a+b*sin(2*4.14*x/c+d)**2)          |              660.998 us|           +/- 49.281 us|
|interpreter(1-exp(-a*x))                       |              687.381 us|           +/- 68.559 us|
|interpreter(a+b*x1+c*x2)                       |              853.033 us|          +/- 153.841 us|
|interpreter(a+b*log(x1)+c*log(x2))             |              753.876 us|           +/- 38.335 us|
|interpreter(a*x1**b*x2**c)                     |              798.297 us|           +/- 68.121 us|
|interpreter(cosh(log(abs(y*z+x**2+x1**x2)))+a*d|              932.745 us|          +/- 102.121 us|
|interpreter(a+b*log(x1)+c*x2+d*x2**2)          |              867.817 us|           +/- 60.685 us|
|interpreter(atan(sinh(log(abs(exp(z/x)*sqrt(y+a|              887.772 us|           +/- 80.037 us|
|interpreter(a+b/x1+c*log(x2)+d*log(x2)**2+e*log|             1073.134 us|          +/- 155.411 us|
|interpreter(atan(sinh(log(abs(exp(z/x)*sqrt(y+a|             1063.364 us|           +/- 59.056 us|
|interpreter(a+b*log(x1)+c*log(x1)**2+d/x2+e/x2*|             1038.969 us|           +/- 60.645 us|
|interpreter((x+a)/(b+c*(x+a)+d*(x+a)**2))      |             1106.564 us|           +/- 53.600 us|
|interpreter(a+b*log(x1)+c*log(x1)**2+d*log(x1)*|             1147.059 us|           +/- 79.165 us|
|fortranparser(a+b*x1)                          |               37.326 us|            +/- 2.724 us|
|fortranparser((a*x**b)/(c+x**b))               |               74.751 us|            +/- 5.658 us|
|fortranparser((a*x)/(b+(x*(1+x/c))))           |               88.807 us|            +/- 9.312 us|
|fortranparser(a+b*exp(c*x)+d*exp(e*x))         |               90.798 us|            +/- 3.749 us|
|fortranparser(a+b*(exp(c*x) - 1)/c)            |               68.345 us|            +/- 5.355 us|
|fortranparser(a+b*log(x)+c*log(x)**2)          |               63.384 us|            +/- 3.160 us|
|fortranparser(a-log(1+b*exp(-c*x)))            |               60.334 us|            +/- 7.523 us|
|fortranparser((a+b*x)/(c+x))                   |               66.550 us|            +/- 3.968 us|
|fortranparser(a+b*exp(-(c*x)))                 |               57.835 us|            +/- 5.136 us|
|fortranparser(a+b*sin(2*3.14*x/c+d))           |               75.984 us|            +/- 6.177 us|
|fortranparser(a+b*sin(2*4.14*x/c+d)**2)        |               72.763 us|            +/- 5.885 us|
|fortranparser(1-exp(-a*x))                     |               30.156 us|            +/- 1.972 us|
|fortranparser(a+b*x1+c*x2)                     |               66.243 us|            +/- 6.326 us|
|fortranparser(a+b*log(x1)+c*log(x2))           |               71.056 us|            +/- 5.313 us|
|fortranparser(a*x1**b*x2**c)                   |               63.630 us|            +/- 3.908 us|
|fortranparser(cosh(log(abs(y*z+x**2+x1**x2)))+a|              127.277 us|            +/- 9.956 us|
|fortranparser(a+b*log(x1)+c*x2+d*x2**2)        |               98.032 us|            +/- 7.774 us|
|fortranparser(atan(sinh(log(abs(exp(z/x)*sqrt(y|              101.294 us|           +/- 10.502 us|
|fortranparser(a+b/x1+c*log(x2)+d*log(x2)**2+e*l|              122.081 us|            +/- 7.015 us|
|fortranparser(atan(sinh(log(abs(exp(z/x)*sqrt(y|              163.321 us|            +/- 9.234 us|
|fortranparser(a+b*log(x1)+c*log(x1)**2+d/x2+e/x|              128.334 us|           +/- 10.907 us|
|fortranparser((x+a)/(b+c*(x+a)+d*(x+a)**2))    |              112.525 us|            +/- 8.615 us|
|fortranparser(a+b*log(x1)+c*log(x1)**2+d*log(x1|              125.867 us|            +/- 9.543 us|
|fparser(a+b*x1)                                |               23.933 us|            +/- 1.431 us|
|fparser((a*x**b)/(c+x**b))                     |               48.438 us|            +/- 3.610 us|
|fparser((a*x)/(b+(x*(1+x/c))))                 |               64.378 us|            +/- 8.794 us|
|fparser(a+b*exp(c*x)+d*exp(e*x))               |               60.926 us|            +/- 5.516 us|
|fparser(a+b*(exp(c*x) - 1)/c)                  |               42.534 us|            +/- 2.949 us|
|fparser(a+b*log(x)+c*log(x)**2)                |               51.088 us|            +/- 3.273 us|
|fparser(a-log(1+b*exp(-c*x)))                  |               45.760 us|            +/- 5.227 us|
|fparser((a+b*x)/(c+x))                         |               48.931 us|            +/- 5.350 us|
|fparser(a+b*exp(-(c*x)))                       |               36.748 us|            +/- 2.772 us|
|fparser(a+b*sin(2*3.14*x/c+d))                 |               48.002 us|            +/- 2.629 us|
|fparser(a+b*sin(2*4.14*x/c+d)**2)              |               55.607 us|            +/- 4.316 us|
|fparser(1-exp(-a*x))                           |               21.748 us|            +/- 0.888 us|
|fparser(a+b*x1+c*x2)                           |               41.868 us|            +/- 2.373 us|
|fparser(a+b*log(x1)+c*log(x2))                 |               50.649 us|            +/- 3.585 us|
|fparser(a*x1**b*x2**c)                         |               45.405 us|            +/- 3.317 us|
|fparser(cosh(log(abs(y*z+x**2+x1**x2)))+a*d*(ex|              100.150 us|            +/- 7.987 us|
|fparser(a+b*log(x1)+c*x2+d*x2**2)              |               74.419 us|            +/- 7.829 us|
|fparser(atan(sinh(log(abs(exp(z/x)*sqrt(y+a**c+|               74.047 us|            +/- 9.553 us|
|fparser(a+b/x1+c*log(x2)+d*log(x2)**2+e*log(x2)|              101.253 us|           +/- 10.766 us|
|fparser(atan(sinh(log(abs(exp(z/x)*sqrt(y+a**c+|              126.219 us|            +/- 6.691 us|
|fparser(a+b*log(x1)+c*log(x1)**2+d/x2+e/x2**2) |              105.094 us|            +/- 7.583 us|
|fparser((x+a)/(b+c*(x+a)+d*(x+a)**2))          |               85.658 us|            +/- 3.932 us|
|fparser(a+b*log(x1)+c*log(x1)**2+d*log(x1)**3+e|               94.905 us|            +/- 7.838 us|
|fee(a+b*x1)                                    |               20.608 us|            +/- 2.256 us|
|fee((a*x**b)/(c+x**b))                         |               56.533 us|            +/- 7.294 us|
|fee((a*x)/(b+(x*(1+x/c))))                     |               77.968 us|            +/- 7.579 us|
|fee(a+b*exp(c*x)+d*exp(e*x))                   |               71.017 us|            +/- 8.511 us|
|fee(a+b*(exp(c*x) - 1)/c)                      |               62.093 us|            +/- 4.428 us|
|fee(a+b*log(x)+c*log(x)**2)                    |               70.863 us|            +/- 9.153 us|
|fee(a-log(1+b*exp(-c*x)))                      |               64.158 us|            +/- 7.419 us|
|fee((a+b*x)/(c+x))                             |               43.095 us|            +/- 3.622 us|
|fee(a+b*exp(-(c*x)))                           |               49.759 us|            +/- 5.479 us|
|fee(a+b*sin(2*3.14*x/c+d))                     |               77.576 us|           +/- 10.382 us|
|fee(a+b*sin(2*4.14*x/c+d)**2)                  |               75.294 us|            +/- 7.138 us|
|fee(1-exp(-a*x))                               |               33.278 us|            +/- 2.112 us|
|fee(a+b*x1+c*x2)                               |               38.835 us|            +/- 5.524 us|
|fee(a+b*log(x1)+c*log(x2))                     |               72.076 us|            +/- 7.415 us|
|fee(a*x1**b*x2**c)                             |               51.092 us|           +/- 10.376 us|
|fee(cosh(log(abs(y*z+x**2+x1**x2)))+a*d*(exp(c*|              165.111 us|           +/- 24.362 us|
|fee(a+b*log(x1)+c*x2+d*x2**2)                  |               74.979 us|            +/- 8.041 us|
|fee(atan(sinh(log(abs(exp(z/x)*sqrt(y+a**c+f*e)|              134.113 us|           +/- 10.802 us|
|fee(a+b/x1+c*log(x2)+d*log(x2)**2+e*log(x2)**3)|              136.102 us|           +/- 13.856 us|
|fee(atan(sinh(log(abs(exp(z/x)*sqrt(y+a**c+f*e)|              230.267 us|           +/- 19.865 us|
|fee(a+b*log(x1)+c*log(x1)**2+d/x2+e/x2**2)     |              115.970 us|            +/- 9.107 us|
|fee((x+a)/(b+c*(x+a)+d*(x+a)**2))              |               96.020 us|            +/- 9.752 us|
|fee(a+b*log(x1)+c*log(x1)**2+d*log(x1)**3+e/x) |              126.579 us|           +/- 15.118 us|
|M_calculator(a+b*x1)                           |               22.250 us|            +/- 1.097 us|
|M_calculator((a*x**b)/(c+x**b))                |               73.832 us|            +/- 3.858 us|
|M_calculator((a*x)/(b+(x*(1+x/c))))            |              113.156 us|            +/- 5.578 us|
|M_calculator(a+b*exp(c*x)+d*exp(e*x))          |               74.952 us|            +/- 4.739 us|
|M_calculator(a+b*(exp(c*x) - 1)/c)             |               57.328 us|            +/- 3.823 us|
|M_calculator(a+b*log(x)+c*log(x)**2)           |              110.682 us|            +/- 4.549 us|
|M_calculator(a-log(1+b*exp(-c*x)))             |               75.637 us|            +/- 5.134 us|
|M_calculator((a+b*x)/(c+x))                    |               63.145 us|            +/- 3.037 us|
|M_calculator(a+b*exp(-(c*x)))                  |               53.501 us|            +/- 2.572 us|
|M_calculator(a+b*sin(2*3.14*x/c+d))            |               65.256 us|            +/- 2.265 us|
|M_calculator(a+b*sin(2*4.14*x/c+d)**2)         |               71.864 us|            +/- 2.216 us|
|M_calculator(1-exp(-a*x))                      |               65.949 us|            +/- 7.275 us|
|M_calculator(a+b*x1+c*x2)                      |               25.476 us|            +/- 3.356 us|
|M_calculator(a+b*log(x1)+c*log(x2))            |              102.633 us|            +/- 4.149 us|
|M_calculator(a*x1**b*x2**c)                    |               32.832 us|            +/- 1.764 us|
|M_calculator(cosh(log(abs(y*z+x**2+x1**x2)))+a*|              241.518 us|           +/- 27.458 us|
|M_calculator(a+b*log(x1)+c*x2+d*x2**2)         |               72.443 us|            +/- 4.939 us|
|M_calculator(atan(sinh(log(abs(exp(z/x)*sqrt(y+|              280.539 us|           +/- 27.312 us|
|M_calculator(a+b/x1+c*log(x2)+d*log(x2)**2+e*lo|              167.467 us|           +/- 13.551 us|
|M_calculator(atan(sinh(log(abs(exp(z/x)*sqrt(y+|              407.812 us|           +/- 14.286 us|
|M_calculator(a+b*log(x1)+c*log(x1)**2+d/x2+e/x2|              122.673 us|            +/- 8.359 us|
|M_calculator((x+a)/(b+c*(x+a)+d*(x+a)**2))     |              115.527 us|            +/- 4.466 us|
|M_calculator(a+b*log(x1)+c*log(x1)**2+d*log(x1)|              172.530 us|           +/- 18.305 us|
|symengine(a+b*x1)                              |               34.663 us|            +/- 2.696 us|
|symengine((a*x**b)/(c+x**b))                   |               55.079 us|            +/- 6.491 us|
|symengine((a*x)/(b+(x*(1+x/c))))               |               66.036 us|            +/- 4.182 us|
|symengine(a+b*exp(c*x)+d*exp(e*x))             |               90.878 us|            +/- 7.634 us|
|symengine(a+b*(exp(c*x) - 1)/c)                |               75.787 us|            +/- 7.986 us|
|symengine(a+b*log(x)+c*log(x)**2)              |               45.870 us|            +/- 1.810 us|
|symengine(a-log(1+b*exp(-c*x)))                |               66.620 us|            +/- 3.105 us|
|symengine((a+b*x)/(c+x))                       |               56.819 us|            +/- 3.771 us|
|symengine(a+b*exp(-(c*x)))                     |               53.461 us|            +/- 2.995 us|
|symengine(a+b*sin(2*3.14*x/c+d))               |               98.678 us|            +/- 6.910 us|
|symengine(a+b*sin(2*4.14*x/c+d)**2)            |              105.063 us|            +/- 3.653 us|
|symengine(1-exp(-a*x))                         |               34.754 us|            +/- 1.613 us|
|symengine(a+b*x1+c*x2)                         |               49.075 us|            +/- 3.311 us|
|symengine(a+b*log(x1)+c*log(x2))               |               52.973 us|            +/- 2.649 us|
|symengine(a*x1**b*x2**c)                       |               35.157 us|            +/- 2.656 us|
|symengine(cosh(log(abs(y*z+x**2+x1**x2)))+a*d*(|              108.099 us|            +/- 7.260 us|
|symengine(a+b*log(x1)+c*x2+d*x2**2)            |               73.030 us|            +/- 7.216 us|
|symengine(atan(sinh(log(abs(exp(z/x)*sqrt(y+a**|              100.571 us|           +/- 10.144 us|
|symengine(a+b/x1+c*log(x2)+d*log(x2)**2+e*log(x|               95.818 us|            +/- 6.201 us|
|symengine(atan(sinh(log(abs(exp(z/x)*sqrt(y+a**|              133.421 us|            +/- 9.519 us|
|symengine(a+b*log(x1)+c*log(x1)**2+d/x2+e/x2**2|               95.151 us|            +/- 6.110 us|
|symengine((x+a)/(b+c*(x+a)+d*(x+a)**2))        |               83.638 us|            +/- 4.092 us|
|symengine(a+b*log(x1)+c*log(x1)**2+d*log(x1)**3|               93.039 us|            +/- 7.999 us|
|reference(a+b*x1)                              |                0.276 us|            +/- 0.012 us|
|reference((a*x**b)/(c+x**b))                   |                0.278 us|            +/- 0.018 us|
|reference((a*x)/(b+(x*(1+x/c))))               |                0.275 us|            +/- 0.013 us|
|reference(a+b*exp(c*x)+d*exp(e*x))             |                0.299 us|            +/- 0.040 us|
|reference(a+b*(exp(c*x) - 1)/c)                |                0.312 us|            +/- 0.026 us|
|reference(a+b*log(x)+c*log(x)**2)              |                0.280 us|            +/- 0.018 us|
|reference(a-log(1+b*exp(-c*x)))                |                0.277 us|            +/- 0.013 us|
|reference((a+b*x)/(c+x))                       |                0.283 us|            +/- 0.019 us|
|reference(a+b*exp(-(c*x)))                     |                0.274 us|            +/- 0.014 us|
|reference(a+b*sin(2*3.14*x/c+d))               |                0.269 us|            +/- 0.013 us|
|reference(a+b*sin(2*4.14*x/c+d)**2)            |                0.257 us|            +/- 0.010 us|
|reference(1-exp(-a*x))                         |                0.283 us|            +/- 0.014 us|
|reference(a+b*x1+c*x2)                         |                0.277 us|            +/- 0.013 us|
|reference(a+b*log(x1)+c*log(x2))               |                0.295 us|            +/- 0.025 us|
|reference(a*x1**b*x2**c)                       |                0.282 us|            +/- 0.014 us|
|reference(cosh(log(abs(y*z+x**2+x1**x2)))+a*d*(|                0.262 us|            +/- 0.021 us|
|reference(a+b*log(x1)+c*x2+d*x2**2)            |                0.267 us|            +/- 0.020 us|
|reference(atan(sinh(log(abs(exp(z/x)*sqrt(y+a**|                0.255 us|            +/- 0.010 us|
|reference(a+b/x1+c*log(x2)+d*log(x2)**2+e*log(x|                0.257 us|            +/- 0.014 us|
|reference(atan(sinh(log(abs(exp(z/x)*sqrt(y+a**|                0.237 us|            +/- 0.010 us|
|reference(a+b*log(x1)+c*log(x1)**2+d/x2+e/x2**2|                0.307 us|            +/- 0.040 us|
|reference((x+a)/(b+c*(x+a)+d*(x+a)**2))        |                0.271 us|            +/- 0.014 us|
|reference(a+b*log(x1)+c*log(x1)**2+d*log(x1)**3|                0.265 us|            +/- 0.015 us|
</center>