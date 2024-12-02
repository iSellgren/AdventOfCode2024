module Day2_Part2
    use file_Utils
    use Day2_Part1
    implicit none
contains

    subroutine run_day2_part2()
        integer, allocatable :: reports(:,:), level_counts(:)
        integer :: n_reports, max_levels, i, safe_count
        character(len=128) :: input_file

        input_file = 'input/Day2.txt'

        print *, "Kor Day 2, Part 2"

        call read_reports(input_file, reports, level_counts, n_reports, max_levels)

        safe_count = 0

        do i = 1, n_reports
            if (is_day2_safe_report_part2(reports(i, 1:level_counts(i)), level_counts(i))) then
                safe_count = safe_count + 1
            endif
        end do

        print *, safe_count
    end subroutine run_day2_part2

    logical function is_day2_safe_report_part2(levels, n)
        integer, intent(in) :: levels(:), n
        integer :: i
        logical :: temp_result
        integer, allocatable :: modified_levels(:)

        if (is_day2_safe_report(levels, n)) then
            is_day2_safe_report_part2 = .true.
            return
        endif

        do i = 1, n
            allocate(modified_levels(n - 1))

            if (i == 1) then
                modified_levels = levels(2:n)
            elseif (i == n) then
                modified_levels = levels(1:n-1)
            else
                modified_levels(1:i-1) = levels(1:i-1)
                modified_levels(i:n-1) = levels(i+1:n)
            endif

            temp_result = is_day2_safe_report(modified_levels, n - 1)
            if (temp_result) then
                is_day2_safe_report_part2 = .true.
                deallocate(modified_levels)
                return
            endif

            deallocate(modified_levels)
        end do

        is_day2_safe_report_part2 = .false.
    end function is_day2_safe_report_part2

end module Day2_Part2
