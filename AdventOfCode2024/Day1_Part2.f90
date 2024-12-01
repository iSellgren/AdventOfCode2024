module Day1_Part2
    use array_Utils
    use file_Utils
    implicit none
    contains

    subroutine run_day1_part2()
        ! Kör lösning för Dag 1, Del 2
        integer, allocatable :: left(:), right(:)
        integer :: n_left, n_right, similarity_score, i, count
        character(len=128) :: input_file

        input_file = 'Day1.txt'

        print *, "Kor Dag 1, Del 2..."

        ! Läs in data från fil
        call read_paired_data(input_file, left, right, n_left)

        n_right = size(right)

        ! Initiera likhetspoängen
        similarity_score = 0

        ! Iterera genom vänsterlistan
        do i = 1, n_left
            count = count_occurrences(left(i), right, n_right)
            similarity_score = similarity_score + left(i) * count
        end do

        ! Skriv ut resultatet
        print *, "Likhetspoang mellan listorna (Dag 1, Del 2):", similarity_score
    end subroutine run_day1_part2

end module Day1_Part2
