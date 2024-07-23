module test_utility
    use benchmark_kinds
    
    implicit none 
    
    contains
    
    pure subroutine test_dummy(a, m)
        real(r8), intent(in) :: a
        integer, intent(in)  :: m
        !private
        integer :: i
        real(r8) :: b
        
        b = a
        do i = 1, m
            b = b + b
        end do
    end subroutine
    
end module
    
#include <assert.inc>
#include <benchmark.inc>
TESTPROGRAM(main)

    TEST(test_arguments)
        use benchmark_method_argument
        use benchmark_kinds
        use benchmark_string
        
        real(r8) :: arg1 = 5.0_r8
        real(r8) :: arg2
        type(arg) :: a
        
        !testing right hand side assignment
        a = arg1
    
        EXPECT_TRUE(arg1 == a)
        EXPECT_TRUE(a == arg1)
        EXPECT_STREQ(str(arg1), a%to_string())
        
        !testing left hand side assignment
        arg2 = a
        EXPECT_DOUBLE_EQ(arg1, arg2)
        EXPECT_TRUE(arg2 == a)
        EXPECT_STREQ(str(arg2), a%to_string())
        
        !testing constructor
        a = arg(12.25_r8, 'arg1')
        EXPECT_STREQ('arg1', a%to_string())
    END_TEST
    
    TEST(test_string)
        use benchmark_kinds
        use benchmark_string
        
        character(:), allocatable :: s
        
        s = str(1)
        EXPECT_STREQ('1', s)
        s = str(12.52e4)
#ifdef __GFORTRAN__
        EXPECT_STREQ('1.252E+5', s)
#else
        EXPECT_STREQ('1.252E+05', s)
#endif
        s = str(954.1235d10)
        EXPECT_STREQ('9.541E+12', s)
        
    END_TEST
    
    TEST(test_calling)
        use benchmark_library
        use test_utility
        
        type(runner) :: br
        logical :: exists
        integer :: fsize
        
        open(unit=15, file = 'report.csv')
        br%unit = 15

        !calling using the macro benchmark
        benchmark(br, run(1.0d-6, 30, test_dummy))
    
        !calling using the derived type and the 'full' name
        br%name = '(1.0d-6, 30, test_dummy)'
        call br%run(1.0d-6, 30, test_dummy)
    
        !calling using the derived type and the 'full' name without brackets
        br%name = '1.0d-6, 30, test_dummy'
        call br%run(1.0d-6, 30, test_dummy)
    
        !calling using the derived type and partial name
        br%name = '30, test_dummy'
        call br%run(1.0d-6, 30, test_dummy)
    
        !calling using the derived type and derived type
        br%name = '1.0d-6,, test_dummy'
        call br%run(1.0d-6, 30, test_dummy)
    
        !calling using the derived type and only the function name
        br%name = 'test_dummy'
        call br%run(1.0d-6, 30, test_dummy)
    
        !calling using the derived type and and empty name
        br%name = ''
        call br%run(1.0d-6, 30, test_dummy)
        
        inquire(unit=15, exist=exists)
        EXPECT_TRUE(exists)
        
        inquire(unit=15, size=fsize)
#ifdef __GFORTRAN__
        EXPECT_EQ(7430, fsize)
#else
        EXPECT_EQ(7747, fsize)
#endif
        
        close(15, status='delete')
    END_TEST
END_TESTPROGRAM