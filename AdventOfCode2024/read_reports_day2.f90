module Day2_Part1
    use file_Utils
    implicit none
contains

    subroutine run_day2_part1()
        integer, allocatable :: reports(:,:)
        integer :: n_reports, max_levels, i, safe_count
        character(len=128) :: input_file

        input_file = 'input/Day2.txt'

        print *, "Running Day 2, Part 1..."

        call read_reports(input_file, reports, n_reports, max_levels)

        safe_count = 0
        do i = 1, n_reports
            if (is_safe_report(reports(i, 1:max_levels), max_levels)) then
                safe_count = safe_count + 1
            endif
        end do

        print *, "Number of safe reports (Day 2, Part 1):", safe_count
    end subroutine run_day2_part1

    logical function is_safe_report(levels, n)
        integer, intent(in) :: levels(:), n
        integer :: i
        logical :: increasing, decreasing

        if (n < 2) then
            is_safe_report = .false.
            return
        endif

        increasing = .true.
        decreasing = .true.

        do i = 1, n - 1
            if (abs(levels(i+1) - levels(i)) < 1 .or. abs(levels(i+1) - levels(i)) > 3) then
                is_safe_report = .false.
                return
            endif
            if (levels(i+1) > levels(i)) decreasing = .false.
            if (levels(i+1) < levels(i)) increasing = .false.
        end do

        is_safe_report = increasing .or. decreasing
    end function is_safe_report

end module Day2_Part1
