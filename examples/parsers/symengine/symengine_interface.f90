module symengine_interface

use iso_c_binding, only: c_size_t, c_ptr, c_long, c_int, c_char, c_double, c_f_pointer
implicit none

interface
    integer(c_size_t) function c_strlen(string) bind(C, name='strlen')
        import :: c_size_t, c_ptr
        type(c_ptr), intent(in), value :: string
    end function c_strlen
    type(c_ptr) function c_basic_new_heap() bind(c, name='basic_new_heap')
        import :: c_ptr
    end function
    subroutine c_basic_free_heap(s) bind(c, name='basic_free_heap')
        import :: c_ptr
        type(c_ptr), value :: s
    end subroutine
    integer(c_long) function c_basic_assign(a, b) bind(c, name='basic_assign')
        import c_long, c_ptr
        type(c_ptr), value :: a, b
    end function
    type(c_ptr) function c_basic_str(s) bind(c, name='basic_str')
        import :: c_ptr
        type(c_ptr), value :: s
    end function
    integer(c_long) function c_basic_parse(s, c) bind(c, name='basic_parse')
        import c_long, c_ptr, c_char
        type(c_ptr), value :: s
        character(kind=c_char), dimension(*) :: c
    end function
    subroutine c_basic_str_free(s) bind(c, name='basic_str_free')
        import :: c_ptr
        type(c_ptr), value :: s
    end subroutine
    integer(c_long) function c_basic_add(s, a, b) bind(c, name='basic_add')
        import :: c_long, c_ptr
        type(c_ptr), value :: s, a, b
    end function
    integer(c_long) function c_basic_sub(s, a, b) bind(c, name='basic_sub')
        import :: c_long, c_ptr
        type(c_ptr), value :: s, a, b
    end function
    integer(c_long) function c_basic_mul(s, a, b) bind(c, name='basic_mul')
        import :: c_long, c_ptr
        type(c_ptr), value :: s, a, b
    end function
    integer(c_long) function c_basic_div(s, a, b) bind(c, name='basic_div')
        import :: c_long, c_ptr
        type(c_ptr), value :: s, a, b
    end function
    integer(c_long) function c_basic_pow(s, a, b) bind(c, name='basic_pow')
        import :: c_long, c_ptr
        type(c_ptr), value :: s, a, b
    end function
    integer(c_int) function c_basic_eq(a, b) bind(c, name='basic_eq')
        import :: c_int, c_ptr
        type(c_ptr), value :: a, b
    end function
    integer(c_int) function c_basic_neq(a, b) bind(c, name='basic_neq')
        import :: c_int, c_ptr
        type(c_ptr), value :: a, b
    end function
    integer(c_long) function c_basic_erf(s, a) bind(c, name='basic_erf')
        import :: c_long, c_ptr
        type(c_ptr), value :: s, a
    end function
    integer(c_long) function c_basic_erfc(s, a) bind(c, name='basic_erfc')
        import :: c_long, c_ptr
        type(c_ptr), value :: s, a
    end function
    integer(c_long) function c_basic_sin(s, a) bind(c, name='basic_sin')
        import :: c_long, c_ptr
        type(c_ptr), value :: s, a
    end function
    integer(c_long) function c_basic_cos(s, a) bind(c, name='basic_cos')
        import :: c_long, c_ptr
        type(c_ptr), value :: s, a
    end function
    integer(c_long) function c_basic_tan(s, a) bind(c, name='basic_tan')
        import :: c_long, c_ptr
        type(c_ptr), value :: s, a
    end function
    integer(c_long) function c_basic_asin(s, a) bind(c, name='basic_asin')
        import :: c_long, c_ptr
        type(c_ptr), value :: s, a
    end function
    integer(c_long) function c_basic_acos(s, a) bind(c, name='basic_acos')
        import :: c_long, c_ptr
        type(c_ptr), value :: s, a
    end function
    integer(c_long) function c_basic_atan(s, a) bind(c, name='basic_atan')
        import :: c_long, c_ptr
        type(c_ptr), value :: s, a
    end function
    integer(c_long) function c_basic_csc(s, a) bind(c, name='basic_csc')
        import :: c_long, c_ptr
        type(c_ptr), value :: s, a
    end function
    integer(c_long) function c_basic_sec(s, a) bind(c, name='basic_sec')
        import :: c_long, c_ptr
        type(c_ptr), value :: s, a
    end function
    integer(c_long) function c_basic_cot(s, a) bind(c, name='basic_cot')
        import :: c_long, c_ptr
        type(c_ptr), value :: s, a
    end function
    integer(c_long) function c_basic_acsc(s, a) bind(c, name='basic_acsc')
        import :: c_long, c_ptr
        type(c_ptr), value :: s, a
    end function
    integer(c_long) function c_basic_asec(s, a) bind(c, name='basic_asec')
        import :: c_long, c_ptr
        type(c_ptr), value :: s, a
    end function
    integer(c_long) function c_basic_acot(s, a) bind(c, name='basic_acot')
        import :: c_long, c_ptr
        type(c_ptr), value :: s, a
    end function
    integer(c_long) function c_basic_sinh(s, a) bind(c, name='basic_sinh')
        import :: c_long, c_ptr
        type(c_ptr), value :: s, a
    end function
    integer(c_long) function c_basic_cosh(s, a) bind(c, name='basic_cosh')
        import :: c_long, c_ptr
        type(c_ptr), value :: s, a
    end function
    integer(c_long) function c_basic_tanh(s, a) bind(c, name='basic_tanh')
        import :: c_long, c_ptr
        type(c_ptr), value :: s, a
    end function
    integer(c_long) function c_basic_asinh(s, a) bind(c, name='basic_asinh')
        import :: c_long, c_ptr
        type(c_ptr), value :: s, a
    end function
    integer(c_long) function c_basic_acosh(s, a) bind(c, name='basic_acosh')
        import :: c_long, c_ptr
        type(c_ptr), value :: s, a
    end function
    integer(c_long) function c_basic_atanh(s, a) bind(c, name='basic_atanh')
        import :: c_long, c_ptr
        type(c_ptr), value :: s, a
    end function
    integer(c_long) function c_basic_csch(s, a) bind(c, name='basic_csch')
        import :: c_long, c_ptr
        type(c_ptr), value :: s, a
    end function
    integer(c_long) function c_basic_sech(s, a) bind(c, name='basic_sech')
        import :: c_long, c_ptr
        type(c_ptr), value :: s, a
    end function
    integer(c_long) function c_basic_coth(s, a) bind(c, name='basic_coth')
        import :: c_long, c_ptr
        type(c_ptr), value :: s, a
    end function
    integer(c_long) function c_basic_acsch(s, a) bind(c, name='basic_acsch')
        import :: c_long, c_ptr
        type(c_ptr), value :: s, a
    end function
    integer(c_long) function c_basic_asech(s, a) bind(c, name='basic_asech')
        import :: c_long, c_ptr
        type(c_ptr), value :: s, a
    end function
    integer(c_long) function c_basic_acoth(s, a) bind(c, name='basic_acoth')
        import :: c_long, c_ptr
        type(c_ptr), value :: s, a
    end function
    integer(c_long) function c_basic_lambertw(s, a) bind(c, name='basic_lambertw')
        import :: c_long, c_ptr
        type(c_ptr), value :: s, a
    end function
    integer(c_long) function c_basic_zeta(s, a) bind(c, name='basic_zeta')
        import :: c_long, c_ptr
        type(c_ptr), value :: s, a
    end function
    integer(c_long) function c_basic_dirichlet_eta(s, a) bind(c, name='basic_dirichlet_eta')
        import :: c_long, c_ptr
        type(c_ptr), value :: s, a
    end function
    integer(c_long) function c_basic_gamma(s, a) bind(c, name='basic_gamma')
        import :: c_long, c_ptr
        type(c_ptr), value :: s, a
    end function
    integer(c_long) function c_basic_loggamma(s, a) bind(c, name='basic_loggamma')
        import :: c_long, c_ptr
        type(c_ptr), value :: s, a
    end function
    integer(c_long) function c_basic_floor(s, a) bind(c, name='basic_floor')
        import :: c_long, c_ptr
        type(c_ptr), value :: s, a
    end function
    integer(c_long) function c_basic_ceiling(s, a) bind(c, name='basic_ceiling')
        import :: c_long, c_ptr
        type(c_ptr), value :: s, a
    end function
    integer(c_long) function c_basic_exp(s, a) bind(c, name='basic_exp')
        import :: c_long, c_ptr
        type(c_ptr), value :: s, a
    end function
    integer(c_long) function c_basic_log(s, a) bind(c, name='basic_log')
        import :: c_long, c_ptr
        type(c_ptr), value :: s, a
    end function
    !integer(c_long) function c_basic_log10(s, a) bind(c, name='basic_log10')
    !    import :: c_long, c_ptr
    !    type(c_ptr), value :: s, a
    !end function
    integer(c_long) function c_basic_abs(s, a) bind(c, name='basic_abs')
        import :: c_long, c_ptr
        type(c_ptr), value :: s, a
    end function
    integer(c_long) function c_basic_sqrt(s, a) bind(c, name='basic_sqrt')
        import :: c_long, c_ptr
        type(c_ptr), value :: s, a
    end function
    integer(c_long) function c_basic_atan2(s, a, b) result(res) bind(c, name='basic_atan2')
        import :: c_long, c_ptr
        type(c_ptr), value :: s, a, b
    end function
    integer(c_long) function c_basic_evalf(s, b, bits, domain) bind(c, name='basic_evalf')
        import :: c_long, c_int, c_ptr
        type(c_ptr), value :: s
        type(c_ptr), value :: b
        integer(c_long), value :: bits
        integer(c_int), value :: domain
    end function
    subroutine c_basic_const_pi(s) bind(c, name='basic_const_pi')
        import :: c_ptr
        type(c_ptr), value :: s
    end subroutine
    subroutine c_basic_const_e(s) bind(c, name='basic_const_E')
        import :: c_ptr
        type(c_ptr), value :: s
    end subroutine
    subroutine c_basic_const_euler_gamma(s) bind(c, name='basic_const_EulerGamma')
        import :: c_ptr
        type(c_ptr), value :: s
    end subroutine
    subroutine c_basic_const_catalan(s) bind(c, name='basic_const_Catalan')
        import :: c_ptr
        type(c_ptr), value :: s
    end subroutine
    subroutine c_basic_const_goldenratio(s) bind(c, name='basic_const_GoldenRatio')
        import :: c_ptr
        type(c_ptr), value :: s
    end subroutine
    integer(c_long) function c_basic_max(s, d) bind(c, name='basic_max')
        import :: c_ptr, c_long
        type(c_ptr), value :: s, d
    end function
    integer(c_long) function c_basic_subs2(s, e, a, b) bind(c, name='basic_subs2')
        import :: c_ptr, c_long
        type(c_ptr), value :: s, e, a, b
    end function
    integer(c_long) function c_basic_free_symbols(self, symbols) bind(c, name='basic_free_symbols')
        import :: c_long, c_ptr
        type(c_ptr), value :: self, symbols
    end function
    integer(c_long) function c_integer_set_si(s, i) bind(c, name='integer_set_si')
        import :: c_long, c_ptr
        type(c_ptr), value :: s
        integer(c_long), value :: i
    end function
    integer(c_long) function c_integer_get_si(s) bind(c, name='integer_get_si')
        import c_long, c_ptr
        type(c_ptr), value :: s
    end function
    integer(c_long) function c_rational_set_si(s, a, b) bind(c, name='rational_set_si')
        import :: c_long, c_ptr
        type(c_ptr), value :: s
        integer(c_long), value :: a, b
    end function
    integer(c_long) function c_real_double_set_d(s, d) bind(c, name='real_double_set_d')
        import :: c_double, c_long, c_ptr
        type(c_ptr), value :: s
        real(c_double), value :: d
    end function
    integer(c_long) function c_complex_set(s, re, im) bind(c, name='complex_set')
        import :: c_long, c_ptr
        type(c_ptr), value :: s, re, im
    end function
    integer(c_long) function c_symbol_set(s, c) bind(c, name='symbol_set')
        import c_long, c_ptr, c_char
        type(c_ptr), value :: s
        character(kind=c_char), dimension(*) :: c
    end function
    type(c_ptr) function c_setbasic_new() bind(c, name='setbasic_new')
        import :: c_ptr
    end function
    subroutine c_setbasic_free(s) bind(c, name='setbasic_free')
        import :: c_ptr
        type(c_ptr), value :: s
    end subroutine
    subroutine c_setbasic_get(s, n, r) bind(c, name='setbasic_get')
        import :: c_int, c_ptr
        type(c_ptr), value :: s, r
        integer(c_int), value :: n
    end subroutine
    integer(c_int) function c_setbasic_insert(self, v) bind(c, name='setbasic_insert')
        import :: c_int, c_ptr
        type(c_ptr), value :: self, v
    end function
    integer(c_long) function c_setbasic_size(s) bind(c, name='setbasic_size')
        import :: c_long, c_ptr
        type(c_ptr), value :: s
    end function
    type(c_ptr) function c_vecbasic_new() bind(c, name='vecbasic_new')
        import c_ptr
    end function
    subroutine c_vecbasic_free(s) bind(c, name='vecbasic_free')
        import :: c_ptr
        type(c_ptr), value :: s
    end subroutine
    integer(c_long) function c_vecbasic_push_back(s, val) bind(c, name='vecbasic_push_back')
        import :: c_ptr, c_long
        type(c_ptr), value :: val, s
    end function
    type(c_ptr) function c_dense_matrix_new() bind(c, name='dense_matrix_new')
        import :: c_ptr
    end function
    type(c_ptr) function c_dense_matrix_new_vec(rows, cols, l) bind(c, name='dense_matrix_new_vec')
        import :: c_ptr, c_long
        integer(c_long), value :: rows, cols
        type(c_ptr), value :: l
    end function
    subroutine c_dense_matrix_free(s) bind(c, name='dense_matrix_free')
        import :: c_ptr
        type(c_ptr), value :: s
    end subroutine
    integer(c_long) function c_dense_matrix_set(s, d) bind(c, name='dense_matrix_set')
        import :: c_ptr, c_long
        type(c_ptr), value :: s, d
    end function
    integer(c_long) function c_dense_matrix_add_scalar(s, a, b) bind(c, name='dense_matrix_add_scalar')
        import :: c_ptr, c_long
        type(c_ptr), value :: s, a, b
    end function
    integer(c_long) function c_dense_matrix_mul_scalar(s, a, b) bind(c, name='dense_matrix_mul_scalar')
        import :: c_ptr, c_long
        type(c_ptr), value :: s, a, b
    end function
    type(c_ptr) function c_dense_matrix_str(s) bind(c, name='dense_matrix_str')
        import :: c_ptr
        type(c_ptr), value :: s
    end function
    integer(c_int) function c_dense_matrix_eq(lhs, rhs) bind(c, name='dense_matrix_eq')
        import :: c_int, c_ptr
        type(c_ptr), value :: lhs, rhs
    end function
    integer(c_long) function c_dense_matrix_transpose(s, a) bind(c, name='dense_matrix_transpose')
        import :: c_long, c_ptr
        type(c_ptr), value :: s, a
    end function
    integer(c_long) function c_dense_matrix_mul_matrix(s, a, b) bind(c, name='dense_matrix_mul_matrix')
        import :: c_long, c_ptr
        type(c_ptr), value :: s, a, b
    end function
    integer(c_long) function c_dense_matrix_add_matrix(s, a, b) bind(c, name='dense_matrix_add_matrix')
        import :: c_long, c_ptr
        type(c_ptr), value :: s, a, b
    end function
    integer(c_long) function c_dense_matrix_ones(s, r, c) bind(c, name='dense_matrix_ones')
        import :: c_long, c_ptr
        type(c_ptr), value :: s
        integer(c_long), value :: r, c
    end function
    integer(c_long) function c_dense_matrix_zeros(s, r, c) bind(c, name='dense_matrix_zeros')
        import :: c_long, c_ptr
        type(c_ptr), value :: s
        integer(c_long), value :: r, c
    end function
    integer(c_long) function c_dense_matrix_eye(s, n, m, k) bind(c, name='dense_matrix_eye')
        import :: c_long, c_int, c_ptr
        type(c_ptr), value :: s
        integer(c_long), value :: n, m
        integer(c_int), value :: k
    end function
    subroutine c_basic_set_emptyset(s) bind(c, name='basic_set_emptyset')
        import :: c_ptr
        type(c_ptr), value :: s
    end subroutine
    subroutine c_basic_set_universalset(s) bind(c, name='basic_set_universalset')
        import :: c_ptr
        type(c_ptr), value :: s
    end subroutine
    subroutine c_basic_set_complexes(s) bind(c, name='basic_set_complexes')
        import :: c_ptr
        type(c_ptr), value :: s
    end subroutine
    subroutine c_basic_set_reals(s) bind(c, name='basic_set_reals')
        import :: c_ptr
        type(c_ptr), value :: s
    end subroutine
    subroutine c_basic_set_rationals(s) bind(c, name='basic_set_rationals')
        import :: c_ptr
        type(c_ptr), value :: s
    end subroutine
    subroutine c_basic_set_integers(s) bind(c, name='basic_set_integers')
        import :: c_ptr
        type(c_ptr), value :: s
    end subroutine
    integer(c_long) function c_basic_set_union(s, a, b) bind(c, name='basic_set_union')
        import :: c_long, c_ptr
        type(c_ptr), value :: s, a, b
    end function
    integer(c_long) function c_basic_set_intersection(s, a, b) bind(c, name='basic_set_intersection')
        import :: c_long, c_ptr
        type(c_ptr), value :: s, a, b
    end function
    integer(c_long) function c_basic_set_interval(s, start, en, left_open, right_open) bind(c, name='basic_set_interval')
        import :: c_long, c_int, c_ptr
        type(c_ptr), value :: s, start, en
        integer(c_int) :: left_open, right_open
    end function
    integer(c_long) function c_basic_set_finiteset(s, container) bind(c, name='basic_set_finiteset')
        import :: c_long, c_ptr
        type(c_ptr), value :: s, container
    end function
end interface 

contains

function convert_string(cstring) result(res)
    type(c_ptr) :: cstring
    integer :: nchars
    character, pointer, dimension(:) :: tempstr
    character(:), allocatable :: res

    nchars = c_strlen(cstring)
    call c_f_pointer(cstring, tempstr, [nchars])
    allocate(character(len=nchars) :: res)
    res = transfer(tempstr(1:nchars), res)
end function

end module
