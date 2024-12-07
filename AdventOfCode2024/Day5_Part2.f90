module Day5_Part2
    use Day5_Part1
    implicit none

contains

subroutine run_day5_part2()
        implicit none
        integer, dimension(:), allocatable :: line
        character(len=6), dimension(:), allocatable :: rules
        integer :: rule_count, line_length
        integer :: totalSum, middle_element

        character(len=256), parameter :: file = 'input/Day5.txt'
        character(len=2000) :: input_line
        integer :: ios

        totalSum = 0
        print *, "Kor Day 5, Part 2"
        allocate(rules(2048))
        allocate(line(128))

        rules = ""
        line = 0

        call load_rules_from_file(file, rules, rule_count)

        open(unit=10, file=file, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            print *, "Error opening file for reading lines."
            stop
        end if

        do while (read_next_line(input_line))
            call parse_line(input_line, line, line_length)

            if (.not. is_sorted(line, line_length, rules, rule_count)) then
                call reorder_line(line, line_length, rules, rule_count)
                middle_element = find_middle_element(line, line_length)
                totalSum = totalSum + middle_element
            end if
        end do

        close(10)
        print *, "res: ", totalSum

    end subroutine run_day5_part2

    subroutine reorder_line(line, line_length, rules, rule_count)
        implicit none
        integer, intent(inout), dimension(:) :: line
        integer, intent(in) :: line_length
        character(len=6), intent(in), dimension(:) :: rules
        integer, intent(in) :: rule_count

        call bubble_sort(line, line_length, rules, rule_count)
    end subroutine reorder_line

    subroutine bubble_sort(line, line_length, rules, rule_count)
        implicit none
        integer, intent(inout), dimension(:) :: line
        integer, intent(in) :: line_length
        character(len=6), intent(in), dimension(:) :: rules
        integer, intent(in) :: rule_count
        integer :: i, j
        logical :: swapped

        do i = 1, line_length - 1
            swapped = .false.
            do j = 1, line_length - i
                if (.not. is_sorted_pair(line(j), line(j + 1), rules, rule_count)) then
                    call swap(line(j), line(j + 1))
                    swapped = .true.
                end if
            end do
            if (.not. swapped) exit
        end do
    end subroutine bubble_sort

    logical function is_sorted_pair(a, b, rules, rule_count)
        implicit none
        integer, intent(in) :: a, b
        character(len=6), intent(in), dimension(:) :: rules
        integer, intent(in) :: rule_count
        character(len=6) :: pair_str

        write(pair_str, '(I0,"|",I0)') a, b
        is_sorted_pair = any(adjustl(trim(pair_str)) == rules(1:rule_count))
    end function is_sorted_pair


    subroutine swap(a, b)
        implicit none
        integer, intent(inout) :: a, b
        integer :: temp

        temp = a
        a = b
        b = temp
    end subroutine swap

end module Day5_Part2
