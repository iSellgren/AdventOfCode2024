module Day1_Part1
    use array_Utils
    use file_Utils
    implicit none
    contains

    subroutine run_day1_part1()
        ! Kör lösning för Dag 1, Del 1
        integer, allocatable :: left(:), right(:)
        integer :: n, total_distance
        character(len=128) :: input_file

        input_file = 'Day1.txt'

        print *, "Kor Dag 1, Del 1..."

        ! Läs in data från fil
        call read_paired_data(input_file, left, right, n)

        call sort_array(left, n)
        call sort_array(right, n)

        total_distance = calculate_total_distance(left, right, n)
        
        print *, "Total distans mellan listorna:  (Dag 1, Del 1):", total_distance
    end subroutine run_day1_part1

end module Day1_Part1
