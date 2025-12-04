program main
    implicit none

    logical :: test_flag
    integer :: num_args, index, day_int, day_int_stat
    character(len=3) :: day_str
    character(len=5) :: test_str

    num_args = command_argument_count()
    if (num_args .lt. 1 .or. num_args .gt. 2) then
        call syntax_error()
    end if

    call get_command_argument(1, day_str)
    read(day_str, *, iostat=day_int_stat) day_int
    if (day_int_stat .ne. 0) then
        call syntax_error()
    end if

    test_flag = .false.
    if (num_args .eq. 2) then
        call get_command_argument(2, test_str)
        if (test_str /= "test") then
            call syntax_error()
        end if
        test_flag = .true.
    end if

    print *, "Day = ", day_int
    print *, "Test flag = ", test_flag
    call run_day(day_int, test_flag)

end program main

! On syntax error, display a message and exit the program.
subroutine syntax_error
    implicit none

    stop "Syntax: advent_of_code <day> [test] (where day is 1-12)"

end subroutine syntax_error

! Run the code or tests for the specified day.
subroutine run_day(day, test)
    implicit none
    integer, intent(in) :: day
    logical, intent(in) :: test

    if (day .eq. 1) then
        call day1(test)
    else if (day .eq. 2) then
        call day2(test)
    else if (day .eq. 3) then
        call day3(test)
    end if

end subroutine run_day
