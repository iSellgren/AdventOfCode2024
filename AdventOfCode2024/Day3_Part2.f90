module Day3_Part2
    implicit none
contains

    subroutine run_day3_part2()
        implicit none
        character(len=256), parameter :: file = 'input/Day3_part2_input.txt'
        character(len=2000) :: line
        integer :: total, ios
        integer(kind=8) :: x, y
        logical :: skip

        total = 0; skip = .false.
        open(10, file=file, status='old', action='read', iostat=ios)
        if (ios /= 0) stop 'Error opening file'

        do while (.true.)
            read(10, '(A)', iostat=ios) line
            if (ios /= 0) exit
            select case (trim(line))
            case ('do()')
                skip = .false.
            case ("don't()")
                skip = .true.
            case default
                if (index(line, 'mul(') == 1) then
                    call parse_mul(line, x, y)
                    if (.not. skip) total = total + x * y
                end if
            end select
        end do

        close(10)
        print *, "Sum: ", total
    end subroutine run_day3_part2

    subroutine parse_mul(arg, x, y)
        character(len=*), intent(in) :: arg
        integer(kind=8), intent(out) :: x, y
        integer :: comma_pos, ios

        comma_pos = index(arg, ',')
        read(arg(5:comma_pos-1), *, iostat=ios) x
        read(arg(comma_pos+1:index(arg, ')')-1), *, iostat=ios) y
        if (ios /= 0) stop 'Error parsing mul'
    end subroutine parse_mul

end module Day3_Part2
