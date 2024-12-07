module Day3_Part1
    implicit none
contains

    subroutine run_day3_part1()
        implicit none
        integer(kind=8) :: res, a, b, ios
        character(len=256), parameter :: input_file = 'input/Day3_cheat.txt'
        res = 0
        print *, "Kor Day 3, Part 1"
        open(unit=10, file=input_file, status='old', action='read', iostat=ios)
        do
            read(10, *, iostat=ios) a, b
            if (ios /= 0) exit
            res = res + a * b
        end do

        close(10)
        print *, 'res: ', res

    end subroutine run_day3_part1

end module Day3_Part1
