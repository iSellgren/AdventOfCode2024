module Day5_Part1
    implicit none

contains

    subroutine run_day5_part1()
        implicit none
        character(len=256), parameter :: file = 'input/Day5.txt'
        character(len=2048) :: input_line
        character(len=6), dimension(:), allocatable :: rules
        integer, dimension(:), allocatable :: line
        integer :: rule_count, line_length
        integer :: ios, totalSum, middle_element
        print *, "Kor Day 5, Part 1"
        rule_count = 0
        totalSum = 0

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

            if (is_sorted(line, line_length, rules, rule_count)) then
                middle_element = find_middle_element(line, line_length)
                totalSum = totalSum + middle_element
            end if
        end do

        close(10)
        print *, "res: ", totalSum


    end subroutine run_day5_part1

    subroutine load_rules_from_file(file, rules, rule_count)
        implicit none
        character(len=*), intent(in) :: file
        character(len=6), intent(out), dimension(:) :: rules
        integer, intent(out) :: rule_count
        character(len=2000) :: input_line
        integer :: ios

        open(unit=10, file=file, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            print *, "Error opening file: ", trim(file)
            stop
        end if

        rule_count = 0
        do
            read(10, '(A)', iostat=ios) input_line
            if (ios /= 0 .or. trim(input_line) == "") exit

            rule_count = rule_count + 1
            rules(rule_count) = adjustl(trim(input_line))
            if (rule_count >= size(rules)) then
                print *, "Exceeded maximum number of rules."
                exit
            end if
        end do

    end subroutine load_rules_from_file

    logical function read_next_line(input_line)
        implicit none
        character(len=*), intent(out) :: input_line
        integer :: ios

        read(10, '(A)', iostat=ios) input_line
        read_next_line = (ios == 0)
    end function read_next_line

    subroutine parse_line(input_line, line, length)
        implicit none
        character(len=*), intent(in) :: input_line
        integer, intent(out), dimension(:) :: line
        integer, intent(out) :: length
        integer :: idx, read_status

        length = 0
        do idx = 1, len_trim(input_line), 3
            if (idx + 1 > len_trim(input_line)) then
                exit
            end if
            read(input_line(idx:idx+1), '(I2)', iostat=read_status) line(length + 1)
            if (read_status /= 0) exit
            length = length + 1
            if (length >= size(line)) exit
        end do

    end subroutine parse_line

    logical function is_sorted(line, line_length, rules, rule_count)
        implicit none
        integer, intent(in), dimension(:) :: line
        integer, intent(in) :: line_length
        character(len=6), intent(in), dimension(:) :: rules
        integer, intent(in) :: rule_count
        integer :: i
        character(len=6) :: rule_str

        do i = 1, line_length - 1
            write(rule_str, '(I2,"|",I2)') line(i), line(i + 1)
            if (.not. any(adjustl(trim(rule_str)) == rules(1:rule_count))) then
                is_sorted = .false.
                return
            end if
        end do

        is_sorted = .true.
    end function is_sorted

    integer function find_middle_element(line, line_length)
        implicit none
        integer, intent(in), dimension(:) :: line
        integer, intent(in) :: line_length

        find_middle_element = line((line_length + 1) / 2)
    end function find_middle_element

end module Day5_Part1
