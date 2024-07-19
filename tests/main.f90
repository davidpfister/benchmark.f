#include <assert.inc>
PROGRAMNAME(main)

    !TEST(test_arguments)
    !    use benchmark_method_argument
    !    use benchmark_kinds
    !    use benchmark_string
    !    
    !    real(r8) :: rarg = 5.0_r8
    !    type(arg) :: a
    !    
    !    a = arg(rarg)
    !
    !    EXPECT_TRUE(rarg == a)
    !    EXPECT_TRUE(a == rarg)
    !    EXPECT_STREQ(str(rarg), a%to_string())
    !END_TEST
end program