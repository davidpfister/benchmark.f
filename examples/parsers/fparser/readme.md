------------------------------------------------------------------------
Fortran 90 function parser v1.1
------------------------------------------------------------------------

This function parser module is intended for applications where a set of 
mathematical fortran-style expressions is specified at runtime and is 
then evaluated for a large number of variable values. This is done by 
compiling the set of function strings into byte code, which is 
interpreted efficiently for the various variable values. 

The source code is available from:
http://fparser.sourceforge.net

Please send comments, corrections or questions to the author:
Roland Schmehl <roland.schmehl@alumni.uni-karlsruhe.de>

------------------------------------------------------------------------
1. Changes
------------------------------------------------------------------------
What's new in v1.1:       (thanks to Wilton P. Silva and Juha Mäkipelto
                           for the bug reports)

* EXP failed: Corrected typo in alphabet string in subroutine LowCase.
* Expression containing binary operator with precedence over unary 
  minus (e.g. "-x^2") failed and has been corrected in subroutines
  IsBinaryOp and CompileSubstr.
* Leading plus (e.g. "+x") is now understood by correcting subroutines
  CompileSubstr and CheckSyntax
* Multiple operators produce error message in subroutine CheckSyntax

------------------------------------------------------------------------
2. Basic usage
------------------------------------------------------------------------

Step 0 - Module Import
----------------------
In all program units where you want to use the function parser procedures 
and variables you must import the module by:

USE fparser

This command imports only 5 public names: initf, parsef, evalf, EvalErrMsg
and EvalErrType, which are explained in the following. The remainder of the 
module is hidden to the calling program.

Step 1 - Initialization
-----------------------
The parser module has to be initialized for the simultaneous evaluation of 
n functions by calling the module subroutine initp one time in your Fortran 
code:

CALL initf (n)

This allocates i=1,...,n internal data structures used by the byte-compiler 
and subsequently by the bytecode-interpreter.

Step 2 - Function parsing
-------------------------
The i-th function string FuncStr is parsed (checked and compiled) into the 
i-th bytecode by calling the module subroutine parsef:

CALL parsef (i, FuncStr, Var)

The variable names as they appear in the string FuncStr have to be passed 
in the one-dimensional string array Var (zero size of Var is acceptable). 
The number of variables is implicitly passed by the dimension of this array. 
For some notes on the syntax of the function string see below.

Step 3 - Function evaluation
----------------------------
The i-th function value is evaluated for a specific set of variable values 
by calling the module function evalf:

a = evalf (i, Val)

The variable values are passed in the one-dimensional array Val which must 
have the same dimension as array Var. 

------------------------------------------------------------------------
3. Error handling
------------------------------------------------------------------------

An error in the function parsing step leads to a detailed error message 
(Type and position of error) and program termination.

An error during function evaluation returns a function value of 0.0 and
sets the error flag EvalErrType (also imported by the USE statement) to 
a value > 0 (EvalErrType=0 indicates no error). An error message from the 
bytecode-interpreter can be obtained by calling the character function 
EvalErrMsg () without any argument.

------------------------------------------------------------------------
4. Function string syntax
------------------------------------------------------------------------

Although they have to be passed as array elements of the same declared 
length (Fortran 90 restriction), the variable names can be of arbitrary 
actual length for the parser. Parsing for variables is case sensitive. 

The syntax of the function string is similar to the Fortran convention. 
Mathematical Operators recognized are +, -, *, /, ** or alternatively ^, 
whereas symbols for brackets must be (). 

The function parser recognizes the (single argument) Fortran 90 intrinsic 
functions abs, exp, log10, log, sqrt, sinh, cosh, tanh, sin, cos, tan, asin, 
acos, atan. Parsing for intrinsic functions is case INsensitive.

Operations are evaluated in the correct order:

  ()          expressions in brackets first
  -A          unary minus (or plus)
  A**B A^B    exponentiation (A raised to the power B)
  A*B  A/B    multiplication and division
  A+B  A-B    addition and subtraction

The function string can contain integer or real constants. To be recognized
as explicit constants these must conform to the format

[+|-][nnn][.nnn][e|E|d|D[+|-]nnn]

where nnn means any number of digits. The mantissa must contain at least
one digit before or following an optional decimal point. Valid exponent 
identifiers are 'e', 'E', 'd' or 'D'. If they appear they must be followed 
by a valid exponent!

------------------------------------------------------------------------
5. Notes
------------------------------------------------------------------------

* The precision of real numbers can be adapted to the calling program by 
  adjusting the KIND parameter rn in the external module parameters.

* The package contains a sample Makefile and some test programs to 
  demonstrate implementation and performance of the function parser.

------------------------------------------------------------------------
6. Credits
------------------------------------------------------------------------

The function parser concept is based on a C++ class library written by 
Juha Nieminen <warp@iki.fi> available from:

http://warp.povusers.org/FunctionParser/
