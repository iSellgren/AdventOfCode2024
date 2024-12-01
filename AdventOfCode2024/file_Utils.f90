module file_Utils
    implicit none
    contains

    subroutine read_paired_data(filename, left, right, n)
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
    end subroutine read_paired_data

end module file_Utils
