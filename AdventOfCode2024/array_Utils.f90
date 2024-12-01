module array_Utils
    implicit none
    contains
!Part1
    subroutine sort_array(arr, n)
        ! Sorterar en array i stigande ordning
        integer, intent(inout) :: arr(:)
        integer, intent(in) :: n
        integer :: i, j, temp

        do i = 1, n - 1
            do j = 1, n - i
                if (arr(j) > arr(j + 1)) then
                    temp = arr(j)
                    arr(j) = arr(j + 1)
                    arr(j + 1) = temp
                endif
            end do
        end do
    end subroutine sort_array
!Part1
    integer function calculate_total_distance(left, right, n)
        ! Beräknar total distans mellan två listor
        integer, intent(in) :: left(:), right(:), n
        integer :: i

        calculate_total_distance = 0
        do i = 1, n
            calculate_total_distance = calculate_total_distance + abs(left(i) - right(i))
        end do
    end function calculate_total_distance
!Part2
    integer function count_occurrences(value, array, n)
        ! Räknar hur många gånger ett värde förekommer i en array
        integer, intent(in) :: value, array(:), n
        integer :: i

        count_occurrences = 0
        do i = 1, n
            if (array(i) == value) then
                count_occurrences = count_occurrences + 1
            endif
        end do
    end function count_occurrences

end module array_Utils
