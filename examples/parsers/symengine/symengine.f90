module symengine

use iso_c_binding, only: c_int, c_long, c_double, c_ptr, c_null_ptr, c_null_char, c_f_pointer, c_associated
use iso_fortran_env, only: int32, int64, real32, real64
use exceptions
use symengine_interface
use symengine_basic
use functions
use symengine_rational
use symengine_symbol
use constants
use dense_matrix
use sets

implicit none

private
public :: ptr, Basic, SymInteger, Rational, RealDouble, Symbol, parse, exp, log, abs, sqrt, atan2, max, SymComplex
public :: sin, cos, tan, asin, acos, atan, erf, erfc
public :: csc, sec, cot, acsc, asec, acot
public :: sinh, cosh, tanh, asinh, acosh, atanh
public :: csch, sech, coth, acsch, asech, acoth
public :: lambertw, zeta, dirichlet_eta, gamma, loggamma, floor, ceiling
public :: ComplexDouble
public :: pi, e, eulergamma, catalan, goldenratio
public :: SetBasic
public :: DenseMatrix, transpose, ones, zeros, eye
public :: emptyset, universalset, complexes, reals, rationals, integers, set_union, set_intersection
public :: interval, finiteset

contains

function parse(c)
    character(len=*) :: c
    type(Basic) :: parse
    integer(c_long) :: exception
    character(len_trim(c) + 1) :: new_c

    new_c = trim(c) // c_null_char
    parse%ptr = c_basic_new_heap()
    exception = c_basic_parse(parse%ptr, new_c) 
    call handle_exception(exception)
end function

end module
