module Day2_tests
    use Day2_Part1
    use Day2_Part2
    implicit none
    contains
    subroutine test_safe_report_part2()
    implicit none

    integer, allocatable :: levels(:)
    integer :: n
    logical :: result
    integer :: passed_tests, failed_tests

    passed_tests = 0
    failed_tests = 0

    allocate(levels(5))
    levels = [7, 6, 4, 2, 1]
    n = size(levels)
    result = is_day2_safe_report_part2(levels, n)
    if (result) then
        passed_tests = passed_tests + 1
        print *, "Test 1 Passed: [7, 6, 4, 2, 1] is Safe"
    endif
    deallocate(levels)

    allocate(levels(5))
    levels = [1, 2, 7, 8, 9]
    n = size(levels)
    result = is_day2_safe_report_part2(levels, n)
    if (.not. result) then
        passed_tests = passed_tests + 1
        print *, "Test 2 Passed: [1, 2, 7, 8, 9] is Unsafe"
    endif
    deallocate(levels)

    allocate(levels(5))
    levels = [1, 3, 2, 4, 5]
    n = size(levels)
    result = is_day2_safe_report_part2(levels, n)
    if (result) then
        passed_tests = passed_tests + 1
        print *, "Test 3 Passed: [1, 3, 2, 4, 5] is Safe"
    endif
    deallocate(levels)

end subroutine test_safe_report_part2
    subroutine test_safe_report()
        implicit none

        integer, allocatable :: levels(:)
        integer :: n
        logical :: result
        integer :: passed_tests, failed_tests

        passed_tests = 0
        failed_tests = 0

        allocate(levels(5))
        levels = [1, 2, 3, 4, 5]
        n = size(levels)
        result = is_day2_safe_report(levels, n)
        if (result) then
            passed_tests = passed_tests + 1
            print *, "Test 1 Passed: [1, 2, 3, 4, 5] is Safe"
        endif
        deallocate(levels)

        allocate(levels(5))
        levels = [9, 7, 5, 3, 1]
        n = size(levels)
        result = is_day2_safe_report(levels, n)
        if (result) then
            passed_tests = passed_tests + 1
            print *, "Test 2 Passed: [9, 7, 5, 3, 1] is Safe"
        endif
        deallocate(levels)

        allocate(levels(5))
        levels = [1, 3, 2, 4, 5]
        n = size(levels)
        result = is_day2_safe_report(levels, n)
        if (.not. result) then
            passed_tests = passed_tests + 1
            print *, "Test 3 Passed: [1, 3, 2, 4, 5] is Unsafe"
        endif
        deallocate(levels)

        allocate(levels(3))
        levels = [1, 5, 9]
        n = size(levels)
        result = is_day2_safe_report(levels, n)
        if (.not. result) then
            passed_tests = passed_tests + 1
            print *, "Test 4 Passed: [1, 5, 9] is Unsafe"
        endif
        deallocate(levels)

        allocate(levels(4))
        levels = [4, 4, 4, 4]
        n = size(levels)
        result = is_day2_safe_report(levels, n)
        if (.not. result) then
            passed_tests = passed_tests + 1
            print *, "Test 5 Passed: [4, 4, 4, 4] is Unsafe"
        endif
        deallocate(levels)

        allocate(levels(1))
        levels = [5]
        n = size(levels)
        result = is_day2_safe_report(levels, n)
        if (.not. result) then
            passed_tests = passed_tests + 1
            print *, "Test 6 Passed: [5] is Unsafe"
        endif
        deallocate(levels)


    end subroutine test_safe_report

end module Day2_tests
