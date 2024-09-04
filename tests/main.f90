#include <assert.inc>
#include <benchmark.inc>
TESTPROGRAM(main)

    TEST(test_arguments)
        use benchmark_argument
        use benchmark_kinds
        use benchmark_string
        use test_utility
        
        real(r8) :: arg1 = 5.0_r8
        real(r8) :: arg2
        type(arg_type) :: arg3
        type(arg) :: a
        
        !testing right hand side assignment
        a = arg1
        EXPECT_STREQ(str(arg1), a%to_string())
        
        !testing constructor
        a = arg(12.25_r8, 'arg1')
        EXPECT_STREQ(a%to_string(), 'arg1')
        
        !testing right hand side assignment with type
        arg3 = arg_type([1.0_r8, 5.8_r8, 7.8_r8], 2)
        a = arg3
        select type(x => a%value)
        type is (arg_type)
            EXPECT_TRUE(.true.)
        class default
            EXPECT_TRUE(.false.)
        end select
        EXPECT_STREQ(a%to_string(), str(arg3))
 
    END_TEST
    
    TEST(test_string)
        use benchmark_kinds
        use benchmark_string
        use test_utility
        
        character(:), allocatable :: s
        type(arg_type) :: a
        
        s = str(1)
        EXPECT_STREQ(s, '1')
        s = str(12.52e4)
#ifdef __GFORTRAN__
        EXPECT_STREQ(s, '1.252E+5')
#else
        EXPECT_STREQ(s, '1.252E+05')
#endif
        s = str(954.1235d10)
        EXPECT_STREQ(s, '9.541E+12')
        
        s = str(a)
        EXPECT_STREQ(s, '?')
        
    END_TEST
    
    TEST(test_serialization)
        use benchmark_kinds
        use benchmark_library
        
        type(runner) :: br
        type(runner) :: br2
        logical :: exists
        integer :: lu
        
        br%maxcalls = 10
        br%csv_unit = -10
        br%mintime = 50_r8
        br%maxtime = 100_r8
        br%offset = 20.0_r8
        br%sampling_window = 10
        br%ssd_threshold = 0.025_r8
        
        call br%write('benchmark.nml')
        
        inquire(file='benchmark.nml', exist=exists)
        EXPECT_TRUE(exists)
        
        call br2%read('benchmark.nml')
        
        EXPECT_EQ(br2%maxcalls, 10)
        EXPECT_EQ(br2%csv_unit, -10)
        EXPECT_EQ(br2%mintime, 50_r8)
        EXPECT_EQ(br2%maxtime, 100_r8)
        EXPECT_EQ(br2%offset, 20.0_r8)
        EXPECT_EQ(br2%sampling_window, 10)
        EXPECT_EQ(br2%ssd_threshold, 0.025_r8)
        
        open(file='benchmark.nml', newunit=lu); close(lu, status='delete')
    END_TEST
        
    TEST(test_calling)
        use benchmark_library
        use test_utility
        
        type(runner) :: br
        logical :: exists
        integer :: fsize
        
        open(unit=15, file = 'report.csv')
        br%unit = 15
        
        br%skip_prelude = .true.
        br%maxcalls = 1

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
        call br%run(1.0d-6, 30, test_dummy)
        
        inquire(unit=15, exist=exists)
        EXPECT_TRUE(exists)
        
        inquire(unit=15, size=fsize)
        EXPECT_EQ(fsize, 4223)
        call br%dispose()
        close(15, status='delete')
    END_TEST
    
    TEST(test_methods)
        use benchmark_method
        use benchmark_string
        use test_utility
        
        type(method) :: mtd
        real(r8) :: a1 = 1.0d-6
        integer :: a2 = 30
        
        mtd = method(test_dummy, arg(a1, str(a1)), arg(a2, str(a2)))
        
        call mtd%invoke()
        
        call mtd%invoke(a1, a2)
        
        mtd = method(test_dummy2, arg(a1, str(a1)), arg(a2, str(a2)), caller_dummy)
        
        call mtd%invoke(a1, a2)
        
        EXPECT_TRUE(.true.)
    END_TEST

END_TESTPROGRAM