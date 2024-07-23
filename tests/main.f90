#include <assert.inc>
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
END_TESTPROGRAM