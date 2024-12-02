module file_Utils
    implicit none
contains

    subroutine read_data(filename, left, right, n)
        character(len=*), intent(in) :: filename
        integer, allocatable, intent(out) :: left(:), right(:)
        integer, intent(out) :: n
        integer :: unit, iostat, num1, num2
        integer, allocatable :: temp_left(:), temp_right(:)
        integer :: count

        unit = 10
        open(unit=unit, file=filename, status='old', action='read', iostat=iostat)
        if (iostat /= 0) then
            print *, "Error opening file: ", trim(filename)
            stop
        endif

        allocate(temp_left(1000), temp_right(1000))
        count = 0

        do
            read(unit, *, iostat=iostat) num1, num2
            if (iostat /= 0) exit
            count = count + 1
            temp_left(count) = num1
            temp_right(count) = num2
        end do

        close(unit)

        n = count
        allocate(left(n), right(n))
        left = temp_left(1:n)
        right = temp_right(1:n)

        deallocate(temp_left, temp_right)
    end subroutine read_data
    
    subroutine read_reports(filename, reports, level_counts, n_reports, max_levels)
        character(len=*), intent(in) :: filename
        integer, allocatable, intent(out) :: reports(:,:)
        integer, allocatable, intent(out) :: level_counts(:)
        integer, intent(out) :: n_reports, max_levels

        character(len=256) :: line
        integer, allocatable :: temp(:)
        integer :: level_count, max_lines, iostat
        logical :: success

        max_lines = 1000
        allocate(reports(0:max_lines, 0:100))
        allocate(level_counts(0:max_lines))
        n_reports = 0
        max_levels = 0

        call open_file(filename, success)
        do
            call read_line(line, success, iostat)
            if (.not. success) then
                print *, "End of file or error reading line. Exiting loop."
                exit
            endif

            if (len_trim(line) == 0) cycle

            print *, "Processing line:", trim(line)
            call parse_levels(line, temp, level_count)
            call store_report(temp, level_count, reports, level_counts, n_reports, max_levels)
        end do

        reports = reports(1:n_reports, 1:max_levels)
        level_counts = level_counts(1:n_reports)
        call close_file()
    end subroutine read_reports

    subroutine store_report(levels, level_count, reports, level_counts, n_reports, max_levels)
        integer, intent(in) :: levels(:), level_count
        integer, allocatable, intent(inout) :: reports(:,:)
        integer, allocatable, intent(inout) :: level_counts(:)
        integer, intent(inout) :: n_reports, max_levels
        integer :: j

        n_reports = n_reports + 1
        level_counts(n_reports) = level_count

        do j = 1, level_count
            reports(n_reports, j) = levels(j)
        end do

        if (level_count > max_levels) max_levels = level_count

        !print *, "Stored report:", reports(n_reports, 1:level_count)
    end subroutine store_report
    
    
    
    subroutine parse_levels(line, levels, level_count)
        implicit none
        character(len=*), intent(in) :: line
        integer, allocatable, intent(out) :: levels(:)
        integer, intent(out) :: level_count

        integer :: temp_value, temp_count, iostat
        integer, allocatable :: temp(:)
        integer :: start, end_pos
        character(len=20) :: word

        allocate(temp(10))
        temp_count = 0
        start = 1

        do
            do while (start <= len_trim(line) .and. line(start:start) == ' ')
                start = start + 1
            end do
            if (start > len_trim(line)) exit

            end_pos = start
            do while (end_pos <= len_trim(line) .and. line(end_pos:end_pos) /= ' ')
                end_pos = end_pos + 1
            end do
            end_pos = end_pos - 1

            word = line(start:end_pos)

            read(word, *, iostat=iostat) temp_value

            temp_count = temp_count + 1
            if (temp_count > size(temp)) then
                call resize_array(temp, size(temp) * 2)
            endif
            temp(temp_count) = temp_value

            start = end_pos + 2
        end do

        allocate(levels(temp_count))
        levels = temp(1:temp_count)
        level_count = temp_count

        !print *, "Parsed levels:", levels(1:level_count)

        deallocate(temp)
    end subroutine parse_levels

    subroutine resize_array(array, new_size)
        integer, allocatable, intent(inout) :: array(:)
        integer, intent(in) :: new_size

        integer, allocatable :: temp(:)

        allocate(temp(new_size))
        temp(1:size(array)) = array(1:size(array))

        deallocate(array)
        allocate(array(new_size))
        array = temp

        deallocate(temp)
    end subroutine resize_array

    subroutine open_file(filename, success)
        character(len=*), intent(in) :: filename
        logical, intent(out) :: success
        integer :: iostat

        open(unit=10, file=filename, status='old', action='read', iostat=iostat)
        success = (iostat == 0)
    end subroutine open_file

    subroutine read_line(line, success, iostat)
        character(len=256), intent(out) :: line
        logical, intent(out) :: success
        integer, intent(out) :: iostat

        read(10, '(A)', iostat=iostat) line
        if (iostat == 0) then
            print *, "Read line successfully:", trim(line)
            success = .true.
        else
            print *, "Failed to read line. IOSTAT =", iostat 
            success = .false.
        endif
    end subroutine read_line

    subroutine close_file()
        close(10)
    end subroutine close_file

end module file_Utils
