# Upper Case {#example_upper}

This examples demonstrate for pieces of code to convert a text into upper case. 
It is based on the discussion https://github.com/fortran-lang/stdlib/issues/703

This example demonstrate the use of the `caller` functionality and casting into explicit interface. 

```
block
    integer :: i
    type(runner) :: br   
    type(string) :: s(3)

    s(1) = 'abcdefghijklmnopqrstuvwxyz'
    s(2) = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    s(3) = 'Lorem ipsum dolor sit amet, consectetur adipiscing elit, '           // &
                    'sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.'  // &
                    ' Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris' // &
                    ' nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in ' // &
                    'reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla '// &
                    'pariatur. Excepteur sint occaecat cupidatat non proident, sunt in '  // &
                    'culpa qui officia deserunt mollit anim id est laborum'


    call br%set_caller(upper_caller)
    do i = 1, 3
        br%name = 'upper1'; call br%run(s(i), upper1)
        br%name = 'upper2'; call br%run(s(i), upper2)
        br%name = 'upper3'; call br%run(s(i), upper3)
    end do

end block
```

The results are as follows: 

<center>
|              Method Name                      |          Mean          |    Standard Deviation  |
|:----------------------------------------------|:-----------------------|:-----------------------|
|upper1(abcdefghijklmnopqrstuvwxyz)             |                0.175 us|            +/- 0.015 us|
|upper2(abcdefghijklmnopqrstuvwxyz)             |                0.203 us|            +/- 0.016 us|
|upper3(abcdefghijklmnopqrstuvwxyz)             |                0.139 us|            +/- 0.008 us|
|upper1(ABCDEFGHIJKLMNOPQRSTUVWXYZ)             |                0.161 us|            +/- 0.010 us|
|upper2(ABCDEFGHIJKLMNOPQRSTUVWXYZ)             |                0.175 us|            +/- 0.016 us|
|upper3(ABCDEFGHIJKLMNOPQRSTUVWXYZ)             |                0.181 us|            +/- 0.016 us|
|upper1(Lorem ipsum dolor sit amet, consectetur |                2.587 us|            +/- 0.305 us|
|upper2(Lorem ipsum dolor sit amet, consectetur |                3.512 us|            +/- 0.266 us|
|upper3(Lorem ipsum dolor sit amet, consectetur |                2.544 us|            +/- 0.214 us|

</center>
