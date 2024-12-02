module Day1_tests
    use array_Utils
    implicit none
contains

    subroutine test_sort_array()
        integer, dimension(10) :: array
        integer, dimension(10) :: sorted_arr
        integer :: passed

        array = [6, 2, 3, 9, 1, 10, 7, 4, 8, 5]

        sorted_arr = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]


        call sort_array(array, size(array))

        passed = all(array == sorted_arr)

        if (passed) then
            print *, "Test OK: array efter sort_array", array
        endif
    end subroutine test_sort_array

    subroutine test_calculate_total_distance()
        integer, dimension(6) :: left, right
        integer :: result, exp_result

        left = [3, 4, 2, 1, 3, 3]
        right = [4, 3, 5, 3, 9, 3]

        exp_result = 13  ! abs(3-4) + abs(4-3) + abs(2-5) + abs(1-3) + abs(3-9) + abs(3-3) = 13 enligt woffe

        result = calculate_total_distance(left, right, size(left))
        
        if (result == exp_result) then
            print *, "Test OK: calculate_total_distance" , result
        endif
    end subroutine test_calculate_total_distance

end module Day1_tests
