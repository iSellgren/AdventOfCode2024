program main
    use Day1_tests
    use Day2_tests
    use Day1_Part1
    use Day1_Part2
    use Day2_Part1
    use Day3_Part1
    use Day3_Part2
    use Day4_Part1
    use Day4_Part2
    use Day5_Part1
    use Day5_Part2
    use Day5_tests
    implicit none

    !call test_sort_array()
    !call test_calculate_total_distance()
    call run_day1_part1()
    call run_day1_part2()
    
    !call test_safe_report()
    !call test_safe_report_part2()
    call run_day2_part1()
    call run_day2_part2()
    call run_day3_part1()
    call run_day3_part2()
    call run_day4_part1()
    call run_day4_part2()
    call run_day5_part1()
    call test_bubble_sort()
    call run_day5_part2()

    end program main
