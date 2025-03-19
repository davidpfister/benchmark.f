module parameters
    use benchmark_kinds
    
    implicit none; private j
    
    public :: i1, i2, i4, i8, r4, r8, r16
    
    integer :: j
    integer, parameter :: neq = 23 
    
    real(r8), parameter :: x = 0.175_r8
    real(r8), parameter :: y = 0.110_r8
    real(r8), parameter :: z = 0.900_r8
    
    real(r8), parameter :: x1 = 0.508_r8
    real(r8), parameter :: x2 = 30.000_r8
    
    real(r8), parameter :: a = 0.900_r8
    real(r8), parameter :: b = 0.100_r8
    real(r8), parameter :: c = 0.110_r8
    real(r8), parameter :: d = 0.120_r8
    real(r8), parameter :: e = 0.130_r8
    real(r8), parameter :: f = 0.140_r8
    
    real(r8) :: results(neq)  
    data(results(j), j=1, neq) / 0.9508_r8, &
                                0.7958_r8, &
                                0.2846_r8, &
                                1.1247_r8, &
                                0.9177_r8, &
                                1.0599_r8, &
                                0.8064_r8, &
                                3.2193_r8, &
                                0.9981_r8, &
                                0.8366_r8, &
                                0.9441_r8, &
                                0.1457_r8, &
                                4.2508_r8, &
                                1.2064_r8, &
                                1.2227_r8, &
                                20.6962_r8, &
                                112.1323_r8, &
                                1.5597_r8, &
                                7.5806_r8, &
                                1.5574_r8, &
                                0.8869_r8, &
                                3.0118_r8, &
                                1.5311_r8 /
    
    character(200) :: eqstring(1:neq)
    data(eqstring(j), j=1, neq)/ &
		'a+b*x1', & 
        '(a*x**b)/(c+x**b)', &
        '(a*x)/(b+(x*(1+x/c)))', &
        'a+b*exp(c*x)+d*exp(e*x)', &
        'a+b*(exp(c*x) - 1)/c', &
        'a+b*log(x)+c*log(x)**2', &
        'a-log(1+b*exp(-c*x))', &
        '(a+b*x)/(c+x)', &
        'a+b*exp(-(c*x))', &
        'a+b*sin(2*3.14*x/c+d)', &
        'a+b*sin(2*4.14*x/c+d)**2', &
        '1-exp(-a*x)', &
        'a+b*x1+c*x2', &
        'a+b*log(x1)+c*log(x2)', &
        'a*x1**b*x2**c', &
        'cosh(log(abs(y*z+x**2+x1**x2)))+a*d*(exp(c*f)+154.3)', &
        'a+b*log(x1)+c*x2+d*x2**2', &
        'atan(sinh(log(abs(exp(z/x)*sqrt(y+a**c+f*e)))))', &
        'a+b/x1+c*log(x2)+d*log(x2)**2+e*log(x2)**3', &
        'atan(sinh(log(abs(exp(z/x)*sqrt(y+a**c+f*e)))))*cos(log(abs(sqrt(y+a**c+f*e))))', &
        'a+b*log(x1)+c*log(x1)**2+d/x2+e/x2**2', &
        '(x+a)/(b+c*(x+a)+d*(x+a)**2)', &
		'a+b*log(x1)+c*log(x1)**2+d*log(x1)**3+e/x'/
end module