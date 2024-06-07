module test_basic
    use symengine
    use iso_fortran_env, only: int64, int32
    
    implicit none
    
    contains
    
    subroutine assert_eq(a, b)
        type(Basic) :: a, b
        if (a /= b) then
            print *, a%str(), " /= ", b%str()
            error stop "Not equal"
        else
            print *, "success"
        end if
    end subroutine

    subroutine assert_matrix_eq(a, b)
        type(DenseMatrix) :: a, b
        if (.not. (a == b)) then
            print *, a%str(), " /= ", b%str()
            error stop "Not equal"
        else
            print *, "success"
        end if
    end subroutine

    subroutine assert_str_eq(a, b)
        character(len=*), intent(in) :: a, b
        if (b /= a(1:len(a)-1)) then
            print *, a, " /= ", b
            error stop "Not equal"
        else
            print *, "success"
        end if
    end subroutine

    subroutine assert(a)
        logical :: a
        if (.not. a) then
            error stop "Assertion failed"
        else
            print *, "success"
        end if
    end subroutine
    
    subroutine test1()
        type(Basic) :: x, f, a
        
        x = Symbol('x')
        a = RealDouble(2.0)
        f = parse('x * 12')
        
        f = f%subs(x, a)
        f = f%evalf()
        print *, f%str()
    end subroutine
    
    subroutine test2()

        type(Basic) :: a, b, c, d
        type(DenseMatrix) :: M, N
        type(SetBasic) :: set

        a = SymInteger(12)
        b = Symbol('x')
        c = parse('x * 12')
        call assert_eq(a * b, c)
        call assert_eq(b * 12, c)
        call assert_eq(12 * b, c)
        call assert_eq(b * 12_int64, c)
        call assert_eq(12_int64 * b, c)

        c = parse('x + 12')
        call assert_eq(a + b, c)
        call assert_eq(b + 12, c)
        call assert_eq(12 + b, c)
        call assert_eq(b + 12_int64, c)
        call assert_eq(12_int64 + b, c)

        c = parse('12 - x')
        call assert_eq(a - b, c)
        call assert_eq(12 - b, c)
        call assert_eq(12_int64 - b, c)
        c = parse('x - 12')
        call assert_eq(b - 12, c)
        call assert_eq(b - 12_int64, c)

        c = parse('-x')
        call assert_eq(-b, c)

        c = parse('12 / x')
        call assert_eq(a / b, c)
        call assert_eq(12 / b, c)
        call assert_eq(12_int64 / b, c)
        c = parse('x / 12')
        call assert_eq(b / 12, c)
        call assert_eq(b / 12_int64, c)

        c = parse('12 ** x')
        call assert_eq(a ** b, c)
        call assert_eq(12 ** b, c)
        call assert_eq(12_int64 ** b, c)
        c = parse('x ** 12')
        call assert_eq(b ** 12, c)
        call assert_eq(b ** 12_int64, c)

        c = parse('erf(x)')
        call assert_eq(erf(b), c)
        c = parse('erfc(x)')
        call assert_eq(erfc(b), c)

        c = parse('sin(x)')
        call assert_eq(sin(b), c)
        c = parse('cos(x)')
        call assert_eq(cos(b), c)
        c = parse('tan(x)')
        call assert_eq(tan(b), c)
        c = parse('asin(x)')
        call assert_eq(asin(b), c)
        c = parse('acos(x)')
        call assert_eq(acos(b), c)
        c = parse('atan(x)')
        call assert_eq(atan(b), c)
        c = parse('csc(x)')
        call assert_eq(csc(b), c)
        c = parse('sec(x)')
        call assert_eq(sec(b), c)
        c = parse('cot(x)')
        call assert_eq(cot(b), c)
        c = parse('acsc(x)')
        call assert_eq(acsc(b), c)
        c = parse('asec(x)')
        call assert_eq(asec(b), c)
        c = parse('acot(x)')
        call assert_eq(acot(b), c)
        c = parse('sinh(x)')
        call assert_eq(sinh(b), c)
        c = parse('cosh(x)')
        call assert_eq(cosh(b), c)
        c = parse('tanh(x)')
        call assert_eq(tanh(b), c)
        c = parse('asinh(x)')
        call assert_eq(asinh(b), c)
        c = parse('acosh(x)')
        call assert_eq(acosh(b), c)
        c = parse('atanh(x)')
        call assert_eq(atanh(b), c)
        c = parse('csch(x)')
        call assert_eq(csch(b), c)
        c = parse('sech(x)')
        call assert_eq(sech(b), c)
        c = parse('coth(x)')
        call assert_eq(coth(b), c)
        c = parse('acsch(x)')
        call assert_eq(acsch(b), c)
        c = parse('asech(x)')
        call assert_eq(asech(b), c)
        c = parse('acoth(x)')
        call assert_eq(acoth(b), c)

        c = parse('atan2(x, 12)')
        call assert_eq(atan2(b, a), c)

        c = parse('atan2(x, 12)')
        call assert_eq(atan2(b, 12), c)
        call assert_eq(atan2(b, 12_int64), c)

        c = parse('atan2(12, x)')
        call assert_eq(atan2(12, b), c)
        call assert_eq(atan2(12_int64, b), c)

        c = parse('atan2(x, 12.0)')
        call assert_eq(atan2(b, 12.0), c)
        call assert_eq(atan2(b, 12.0d0), c)

        c = parse('atan2(12.0, x)')
        call assert_eq(atan2(12.0, b), c)
        call assert_eq(atan2(12.0d0, b), c)

        c = parse('exp(x)')
        call assert_eq(exp(b), c)

        c = parse('log(x)')
        call assert_eq(log(b), c)

        c = parse('lambertw(x)')
        call assert_eq(lambertw(b), c)
        c = parse('zeta(x)')
        call assert_eq(zeta(b), c)
        c = parse('dirichlet_eta(x)')
        call assert_eq(dirichlet_eta(b), c)
        c = parse('gamma(x)')
        call assert_eq(gamma(b), c)
        c = parse('loggamma(x)')
        call assert_eq(loggamma(b), c)
        c = parse('abs(x)')
        call assert_eq(abs(b), c)

        c = parse('sqrt(x)')
        call assert_eq(sqrt(b), c)

        a = 12
        b = parse('12')
        call assert_eq(a, b)
        a = 12_int64
        b = parse('12')
        call assert_eq(a, b)
        a = 12.0
        b = parse('12.0')
        call assert_eq(a, b)
        a = 12.0d0
        b = parse('12.0')
        call assert_eq(a, b)
        a = (2.0, 3.0)
        b = parse('2.0 + 3.0*I')
        call assert_eq(a, b)
        a = (2.0d0, 3.0d0)
        b = parse('2.0 + 3.0*I')
        call assert_eq(a, b)

        a = Rational(1, 2)
        b = Rational(3, 4)
        c = Rational(3, 8)
        call assert_eq(a * b, c)

        a = RealDouble(1.0)
        b = RealDouble(2.0)
        c = RealDouble(3.0)
        call assert_eq(a + b, c)

        a = RealDouble(23)
        b = parse('23.0')
        call assert_eq(a, b)
        a = RealDouble(23_int64)
        b = parse('23.0')
        call assert_eq(a, b)

        a = Symbol('x')
        b = parse('x * 2.0')
        call assert_eq(a * 2.0, b)
        call assert_eq(2.0 * a, b)
        call assert_eq(a * 2.0d0, b)
        call assert_eq(2.0d0 * a, b)

        a = Symbol('x')
        b = parse('x + 1.0')
        call assert_eq(a + 1.0, b)
        call assert_eq(1.0 + a, b)
        call assert_eq(a + 1.0d0, b)
        call assert_eq(1.0d0 + a, b)

        a = Symbol('x')
        b = parse('x - 1.0')
        call assert_eq(a - 1.0, b)
        call assert_eq(a - 1.0d0, b)
        b = parse('1.0 - x')
        call assert_eq(1.0 - a, b)
        call assert_eq(1.0d0 - a, b)

        a = Symbol('x')
        b = parse('x / 2.0')
        call assert_eq(a / 2.0, b)
        call assert_eq(a / 2.0d0, b)
        b = parse('2.0 / x')
        call assert_eq(2.0 / a, b)
        call assert_eq(2.0d0 / a, b)

        a = Symbol('x')
        b = parse('x ** 2.0')
        call assert_eq(a ** 2.0, b)
        call assert_eq(a ** 2.0d0, b)
        b = parse('2.0 ** x')
        call assert_eq(2.0 ** a, b)
        call assert_eq(2.0d0 ** a, b)

        a = RealDouble(1.0d0)
        b = RealDouble(2.0d0)
        c = RealDouble(3.0d0)
        call assert_eq(a + b, c)

        a = SymInteger(2)
        c = sqrt(a)
        c = c%evalf()
        d = RealDouble(1.4142135623730951d0)
        call assert_eq(c, d)

        a = SymInteger(2)
        call assert(a == 2)
        call assert(2 == a)
        call assert(2_int64 == a)
        call assert(a == 2_int64)

        a = RealDouble(2.0)
        call assert(2.0 == a)
        call assert(a == 2.0)
        call assert(2.0d0 == a)
        call assert(a == 2.0d0)

        a = ComplexDouble((2.0, 1.0))
        call assert((2.0, 1.0) == a)
        call assert(a == (2.0, 1.0))

        a = ComplexDouble(2.0, 3.0)
        call assert((2.0, 3.0) == a)
        a = ComplexDouble(2.0, 3.0d0)
        call assert((2.0, 3.0) == a)
        a = ComplexDouble(2.0d0, 3.0)
        call assert((2.0, 3.0) == a)
        a = ComplexDouble(2.0d0, 3.0d0)
        call assert((2.0, 3.0) == a)

        a = SymInteger(2)
        call assert(a /= 3)
        call assert(3 /= a)
        call assert(3_int64 /= a)
        call assert(a /= 3_int64)

        a = RealDouble(2.0)
        call assert(3.0 /= a)
        call assert(a /= 3.0)
        call assert(3.0d0 /= a)
        call assert(a /= 3.0d0)

        a = ComplexDouble((2.0, 1.0))
        call assert((3.0, 3.0) /= a)
        call assert(a /= (3.0, 3.0))

        a = ComplexDouble(2.0, 3.0)
        call assert((3.0, 3.0) /= a)
        a = ComplexDouble(2.0, 3.0d0)
        call assert((3.0, 3.0) /= a)
        a = ComplexDouble(2.0d0, 3.0)
        call assert((3.0, 3.0) /= a)
        a = ComplexDouble(2.0d0, 3.0d0)
        call assert((3.0, 3.0) /= a)

        a = pi()
        b = parse("pi")
        call assert_eq(a, b)

        a = e()
        b = parse("e")
        call assert_eq(a, b)

        a = eulergamma()
        b = parse("EulerGamma")
        call assert_eq(a, b)

        a = catalan()
        b = parse("Catalan")
        call assert_eq(a, b)

        a = goldenratio()
        b = parse("GoldenRatio")
        call assert_eq(a, b)

        a = max([ptr(SymInteger(2)), ptr(SymInteger(3))])
        b = parse("3")
        call assert_eq(a, b)

        a = max([ptr(Symbol("x")), ptr(SymInteger(3))])
        b = parse("max(3, x)")
        call assert_eq(a, b)

        a = Symbol("x")
        b = SymInteger(2) * a
        c = b%subs(a, SymInteger(15))
        d = SymInteger(30)
        call assert_eq(c, d)

        a = SymComplex(1, 1)
        b = parse('1 + I')
        call assert_eq(a, b)

        a = SymComplex(1_int64, 1_int64)
        b = parse('1 + I')
        call assert_eq(a, b)
        a = SymComplex(1, 1_int64)
        call assert_eq(a, b)
        a = SymComplex(1_int64, 1)
        call assert_eq(a, b)

        a = ComplexDouble((1.0, 1.0))
        b = parse('1.0 + 1.0*I')
        call assert_eq(a, b)
        a = ComplexDouble((1.0d0, 1.0d0))
        b = parse('1.0 + 1.0*I')
        call assert_eq(a, b)

        a = SymInteger(1)
        b = a + (1.0, 1.0)
        c = parse('2.0 + 1.0*I')
        call assert_eq(b, c)
        b = (2.0, 2.0) + a
        c = parse('3.0 + 2.0*I')
        call assert_eq(b, c)
        a = SymInteger(1)
        b = a + (1.0d0, 1.0d0)
        c = parse('2.0 + 1.0*I')
        call assert_eq(b, c)
        b = (2.0d0, 2.0d0) + a
        c = parse('3.0 + 2.0*I')
        call assert_eq(b, c)

        a = SymInteger(1)
        b = a - (1.0, 1.0)
        c = parse('0.0 - 1.0*I')
        call assert_eq(b, c)
        b = (1.0, 1.0) - a
        c = parse('0.0 + 1.0*I')
        call assert_eq(b, c)
        b = a - (1.0d0, 1.0d0)
        c = parse('0.0 - 1.0*I')
        call assert_eq(b, c)
        b = (1.0d0, 1.0d0) - a
        c = parse('0.0 + 1.0*I')
        call assert_eq(b, c)

        a = SymInteger(2)
        b = a * (2.0, 2.0)
        c = parse('4.0 + 4.0*I')
        call assert_eq(b, c)
        b = (2.0, 2.0) * a
        c = parse('4.0 + 4.0*I')
        call assert_eq(b, c)
        a = SymInteger(2)
        b = a * (2.0d0, 2.0d0)
        c = parse('4.0 + 4.0*I')
        call assert_eq(b, c)
        b = (2.0d0, 2.0d0) * a
        c = parse('4.0 + 4.0*I')
        call assert_eq(b, c)

        a = SymInteger(2)
        b = a / (0.0, 1.0)
        c = parse('1.0 - 1.0*I')
        !Cannot do exact comparison
        !call assert_eq(b, c)
        b = (2.0, 2.0) / a
        c = parse('1.0 + 1.0*I')
        call assert_eq(b, c)
        b = a / (1.0d0, 1.0d0)
        c = parse('1.0 - 1.0*I')
        !Cannot do exact comparison
        !call assert_eq(b, c)
        b = (2.0d0, 2.0d0) / a
        c = parse('1.0 + 1.0*I')
        call assert_eq(b, c)

        a = SymInteger(1)
        b = a ** (1.0, 1.0)
        c = parse('1.0 + 0.0*I')
        call assert_eq(b, c)
        b = (1.0, 1.0) ** a
        c = parse('1.0 + 1.0*I')
        call assert_eq(b, c)
        b = a ** (1.0d0, 1.0d0)
        c = parse('1.0 + 0.0*I')
        call assert_eq(b, c)
        b = (1.0d0, 1.0d0) ** a
        c = parse('1.0 + 1.0*I')
        call assert_eq(b, c)

        a = SymInteger(23)
        b = SymInteger(45)
        c = SymComplex(a, b)
        d = parse('23 + 45*I')
        call assert_eq(c, d)

        M = DenseMatrix(1, 2, [ptr(SymInteger(1)), ptr(SymInteger(2))])
        N = DenseMatrix(1, 2, [ptr(SymInteger(3)), ptr(SymInteger(4))])
        M = M + SymInteger(2)
        call assert_str_eq(M%str(), '[3, 4]')
        call assert_matrix_eq(M, N)
        M = DenseMatrix(1, 2, [ptr(SymInteger(1)), ptr(SymInteger(2))])
        M = SymInteger(2) + M
        call assert_matrix_eq(M, N)
    
        M = DenseMatrix(1, 2, [ptr(SymInteger(1)), ptr(SymInteger(2))])
        N = DenseMatrix(1, 2, [ptr(SymInteger(3)), ptr(SymInteger(4))])
        M = M + 2
        call assert_matrix_eq(M, N)
    
        M = DenseMatrix(1, 2, [ptr(SymInteger(1)), ptr(SymInteger(2))])
        N = DenseMatrix(1, 2, [ptr(SymInteger(3)), ptr(SymInteger(4))])
        M = 2 + M
        call assert_matrix_eq(M, N)
    
        M = DenseMatrix(1, 2, [ptr(SymInteger(1)), ptr(SymInteger(2))])
        N = DenseMatrix(1, 2, [ptr(SymInteger(3)), ptr(SymInteger(4))])
        M = M + 2_int64
        call assert_matrix_eq(M, N)
    
        M = DenseMatrix(1, 2, [ptr(SymInteger(1)), ptr(SymInteger(2))])
        N = DenseMatrix(1, 2, [ptr(SymInteger(3)), ptr(SymInteger(4))])
        M = 2_int64 + M
        call assert_matrix_eq(M, N)
    
        M = DenseMatrix(1, 2, [ptr(SymInteger(1)), ptr(SymInteger(2))])
        N = DenseMatrix(1, 2, [ptr(RealDouble(3.0)), ptr(RealDouble(4.0))])
        M = M + 2.0
        call assert_matrix_eq(M, N)
    
        M = DenseMatrix(1, 2, [ptr(SymInteger(1)), ptr(SymInteger(2))])
        N = DenseMatrix(1, 2, [ptr(RealDouble(3.0)), ptr(RealDouble(4.0))])
        M = 2.0 + M
        call assert_matrix_eq(M, N)
    
        M = DenseMatrix(1, 2, [ptr(SymInteger(1)), ptr(SymInteger(2))])
        N = DenseMatrix(1, 2, [ptr(RealDouble(3.0)), ptr(RealDouble(4.0))])
        M = M + 2.0d0
        call assert_matrix_eq(M, N)
    
        M = DenseMatrix(1, 2, [ptr(SymInteger(1)), ptr(SymInteger(2))])
        N = DenseMatrix(1, 2, [ptr(RealDouble(3.0)), ptr(RealDouble(4.0))])
        M = 2.0d0 + M
        call assert_matrix_eq(M, N)
    
        M = DenseMatrix(1, 2, [ptr(SymInteger(1)), ptr(SymInteger(2))])
        N = DenseMatrix(1, 2, [ptr(SymInteger(-1)), ptr(SymInteger(-2))])
        M = -M
        call assert_matrix_eq(M, N)
    
        M = DenseMatrix(1, 2, [ptr(SymInteger(1)), ptr(SymInteger(2))])
        N = DenseMatrix(1, 2, [ptr(SymInteger(-1)), ptr(SymInteger(0))])
        a = SymInteger(2)
        M = (M - a)
        call assert_matrix_eq(M, N)
        !
        M = DenseMatrix(1, 2, [ptr(SymInteger(1)), ptr(SymInteger(2))])
        N = DenseMatrix(1, 2, [ptr(SymInteger(1)), ptr(SymInteger(0))])
        M = a - M
        call assert_matrix_eq(M, N)
        !
        M = DenseMatrix(1, 2, [ptr(SymInteger(1)), ptr(SymInteger(2))])
        N = DenseMatrix(1, 2, [ptr(SymInteger(-1)), ptr(SymInteger(0))])
        M = M - 2
        call assert_matrix_eq(M, N)
    
        M = DenseMatrix(1, 2, [ptr(SymInteger(1)), ptr(SymInteger(2))])
        N = DenseMatrix(1, 2, [ptr(SymInteger(1)), ptr(SymInteger(0))])
        M = 2 - M
        call assert_matrix_eq(M, N)
    
        M = DenseMatrix(1, 2, [ptr(SymInteger(1)), ptr(SymInteger(2))])
        N = DenseMatrix(1, 2, [ptr(SymInteger(-1)), ptr(SymInteger(0))])
        M = M - 2_int64
        call assert_matrix_eq(M, N)
    
        M = DenseMatrix(1, 2, [ptr(SymInteger(1)), ptr(SymInteger(2))])
        N = DenseMatrix(1, 2, [ptr(SymInteger(1)), ptr(SymInteger(0))])
        M = 2_int64 - M
        call assert_matrix_eq(M, N)
    
        M = DenseMatrix(1, 2, [ptr(SymInteger(1)), ptr(SymInteger(3))])
        N = DenseMatrix(1, 2, [ptr(RealDouble(-1.0)), ptr(RealDouble(1.0))])
        M = M - 2.0
        call assert_matrix_eq(M, N)
    
        M = DenseMatrix(1, 2, [ptr(SymInteger(1)), ptr(SymInteger(3))])
        N = DenseMatrix(1, 2, [ptr(RealDouble(1.0)), ptr(RealDouble(-1.0))])
        M = 2.0 - M
        call assert_matrix_eq(M, N)
    
        M = DenseMatrix(1, 2, [ptr(SymInteger(1)), ptr(SymInteger(2))])
        N = DenseMatrix(1, 2, [ptr(SymInteger(2)), ptr(SymInteger(4))])
        M = M * SymInteger(2)
        call assert_matrix_eq(M, N)
    
        M = DenseMatrix(1, 2, [ptr(SymInteger(1)), ptr(SymInteger(2))])
        N = DenseMatrix(1, 2, [ptr(SymInteger(2)), ptr(SymInteger(4))])
        M = SymInteger(2) * M
        call assert_matrix_eq(M, N)
    
        M = DenseMatrix(1, 2, [ptr(SymInteger(1)), ptr(SymInteger(2))])
        N = DenseMatrix(1, 2, [ptr(SymInteger(2)), ptr(SymInteger(4))])
        M = M * 2
        call assert_matrix_eq(M, N)
    
        M = DenseMatrix(1, 2, [ptr(SymInteger(1)), ptr(SymInteger(2))])
        N = DenseMatrix(1, 2, [ptr(SymInteger(2)), ptr(SymInteger(4))])
        M = 2 * M
        call assert_matrix_eq(M, N)
    
        M = DenseMatrix(1, 2, [ptr(SymInteger(1)), ptr(SymInteger(2))])
        N = DenseMatrix(1, 2, [ptr(SymInteger(2)), ptr(SymInteger(4))])
        M = M * 2_int64
        call assert_matrix_eq(M, N)
    
        M = DenseMatrix(1, 2, [ptr(SymInteger(1)), ptr(SymInteger(2))])
        N = DenseMatrix(1, 2, [ptr(SymInteger(2)), ptr(SymInteger(4))])
        M = 2_int64 * M
        call assert_matrix_eq(M, N)
    
        M = DenseMatrix(1, 2, [ptr(SymInteger(1)), ptr(SymInteger(2))])
        N = DenseMatrix(1, 2, [ptr(RealDouble(2.0)), ptr(RealDouble(4.0))])
        M = M * 2.0
        call assert_matrix_eq(M, N)
    
        M = DenseMatrix(1, 2, [ptr(SymInteger(1)), ptr(SymInteger(2))])
        N = DenseMatrix(1, 2, [ptr(RealDouble(2.0)), ptr(RealDouble(4.0))])
        M = 2.0 * M
        call assert_matrix_eq(M, N)
    
        M = DenseMatrix(1, 2, [ptr(SymInteger(1)), ptr(SymInteger(2))])
        N = DenseMatrix(1, 2, [ptr(RealDouble(2.0)), ptr(RealDouble(4.0))])
        M = M * 2.0d0
        call assert_matrix_eq(M, N)
    
        M = DenseMatrix(1, 2, [ptr(SymInteger(1)), ptr(SymInteger(2))])
        N = DenseMatrix(1, 2, [ptr(RealDouble(2.0)), ptr(RealDouble(4.0))])
        M = 2.0d0 * M
        call assert_matrix_eq(M, N)
    
        M = DenseMatrix(1, 2, [ptr(SymInteger(2)), ptr(SymInteger(4))])
        N = DenseMatrix(1, 2, [ptr(SymInteger(1)), ptr(SymInteger(2))])
        M = M / SymInteger(2)
        call assert_matrix_eq(M, N)
    
        M = DenseMatrix(1, 2, [ptr(SymInteger(2)), ptr(SymInteger(4))])
        N = DenseMatrix(1, 2, [ptr(SymInteger(1)), ptr(SymInteger(2))])
        M = M / 2
        call assert_matrix_eq(M, N)
    
        M = DenseMatrix(1, 2, [ptr(SymInteger(2)), ptr(SymInteger(4))])
        N = DenseMatrix(1, 2, [ptr(SymInteger(1)), ptr(SymInteger(2))])
        M = M / 2_int64
        call assert_matrix_eq(M, N)
    
        M = DenseMatrix(1, 2, [ptr(SymInteger(2)), ptr(SymInteger(4))])
        N = DenseMatrix(1, 2, [ptr(RealDouble(1.0)), ptr(RealDouble(2.0))])
        M = M / 2.0
        call assert_matrix_eq(M, N)
    
        M = DenseMatrix(1, 2, [ptr(SymInteger(2)), ptr(SymInteger(4))])
        N = DenseMatrix(1, 2, [ptr(RealDouble(1.0)), ptr(RealDouble(2.0))])
        M = M / 2.0d0
        call assert_matrix_eq(M, N)
    
        M = DenseMatrix(2, 2, [ptr(SymInteger(1)), ptr(SymInteger(2)), ptr(SymInteger(3)), ptr(SymInteger(4))])
        N = DenseMatrix(2, 2, [ptr(SymInteger(1)), ptr(SymInteger(3)), ptr(SymInteger(2)), ptr(SymInteger(4))])
        M = transpose(M)
        call assert_matrix_eq(M, N)
    
        M = DenseMatrix(2, 1, [ptr(SymInteger(1)), ptr(SymInteger(2))])
        N = DenseMatrix(1, 2, [ptr(SymInteger(1)), ptr(SymInteger(2))])
        M = M * N
        N = DenseMatrix(2, 2, [ptr(SymInteger(1)), ptr(SymInteger(2)), ptr(SymInteger(2)), ptr(SymInteger(4))])
        call assert_matrix_eq(M, N)
    
        M = DenseMatrix(2, 1, [ptr(SymInteger(1)), ptr(SymInteger(2))])
        N = DenseMatrix(2, 1, [ptr(SymInteger(3)), ptr(SymInteger(4))])
        M = M + N
        N = DenseMatrix(2, 1, [ptr(SymInteger(4)), ptr(SymInteger(6))])
        call assert_matrix_eq(M, N)
    
        M = DenseMatrix(2, 1, [ptr(SymInteger(1)), ptr(SymInteger(2))])
        N = DenseMatrix(2, 1, [ptr(SymInteger(3)), ptr(SymInteger(4))])
        M = M - N
        N = DenseMatrix(2, 1, [ptr(SymInteger(-2)), ptr(SymInteger(-2))])
        call assert_matrix_eq(M, N)
    
        M = ones(2, 2)
        N = DenseMatrix(2, 2, [ptr(SymInteger(1)), ptr(SymInteger(1)), ptr(SymInteger(1)), ptr(SymInteger(1))])
        call assert_matrix_eq(M, N)
    
        M = zeros(2, 2)
        N = DenseMatrix(2, 2, [ptr(SymInteger(0)), ptr(SymInteger(0)), ptr(SymInteger(0)), ptr(SymInteger(0))])
        call assert_matrix_eq(M, N)
    
        M = eye(2, 0)
        N = DenseMatrix(2, 2, [ptr(SymInteger(1)), ptr(SymInteger(0)), ptr(SymInteger(0)), ptr(SymInteger(1))])
        call assert_matrix_eq(M, N)
    
        a = Symbol("x") + Symbol("y")
        set = a%free_symbols()
        a = set%get(1)
        b = Symbol("x")
        call assert_eq(a, b)
        a = set%get(2)
        b = Symbol("y")
        call assert_eq(a, b)
    
        a = emptyset()
        b = universalset()
        call assert_eq(b, set_union(a, b))
        call assert_eq(a, set_intersection(a, b))
        a = complexes()
        b = reals()
        call assert_eq(a, set_union(a, b))
        call assert_eq(b, set_intersection(a, b))
        a = rationals()
        b = integers()
        call assert_eq(a, set_union(a, b))
        call assert_eq(b, set_intersection(a, b))
    
        a = SymInteger(1)
        b = SymInteger(2)
        c = interval(a, b)
        c = interval(1, 2, left_open=.true., right_open=.false.)
        c = interval(1, 2_int64)
        c = interval(1_int64, 2)
        c = interval(1_int64, 2_int64)
        c = interval(1.0, 2.0)
        c = interval(1.0, 2)
        c = interval(1.0, 2_int64)
        c = interval(1, 2.0)
        c = interval(1_int64, 2.0)
        c = interval(1.0d0, 2)
        c = interval(1.0d0, 2_int64)
        c = interval(1, 2.0d0, right_open=.true.)
        c = interval(1_int64, 2.0d0, left_open=.false.)
    
        a = finiteset([ptr(SymInteger(1)), ptr(SymInteger(2)), ptr(SymInteger(3)), ptr(SymInteger(4))])

    end subroutine
end module