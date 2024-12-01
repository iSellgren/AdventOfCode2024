program main
    use Day1_tests
    use Day1_Part1
    use day1_Part2
    implicit none

    call test_sort_array()
    call test_calculate_total_distance()
    call run_day1_part1()
    call run_day1_part2()

end program main
