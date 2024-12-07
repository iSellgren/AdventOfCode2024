module Day4_Part2
    implicit none
contains

subroutine run_day4_part2()
    implicit none
    character(len=256), parameter :: file = 'input/Day4.txt'
    character(len=1), allocatable, dimension(:,:) :: grid
    integer :: i, j, row_count, col_count, count

    count = 0
    row_count = 0
    col_count = 0
    print *, "Kor Day 4, Part 2"
    call get_grid_dimensions(file, row_count, col_count)
    allocate(grid(row_count, col_count))
    call read_grid(file, grid, row_count, col_count)

    do i = 2, row_count - 1
        do j = 2, col_count - 1
            if (grid(i, j) == "A") then
                if (check_diagonal_patterns(grid, i, j)) count = count + 1
            end if
        end do
    end do

    print *, "res: ", count
end subroutine run_day4_part2

subroutine get_grid_dimensions(file, rows, cols)
    implicit none
    character(len=256), intent(in) :: file
    integer, intent(out) :: rows, cols
    character(len=2000) :: line
    integer :: ios

    rows = 0
    cols = 0

    open(unit=10, file=file, status='old', action='read', iostat=ios)

    do
        read(10, '(A)', iostat=ios) line
        if (ios /= 0) exit
        rows = rows + 1
        if (cols == 0) cols = len_trim(line)
    end do

    close(10)
end subroutine get_grid_dimensions

subroutine read_grid(file, grid, rows, cols)
    implicit none
    character(len=256), intent(in) :: file
    character(len=1), dimension(:,:), intent(out) :: grid
    integer, intent(in) :: rows, cols
    character(len=2000) :: line
    integer :: i, j, ios

    open(unit=10, file=file, status='old', action='read', iostat=ios)

    do i = 1, rows
        read(10, '(A)', iostat=ios) line
        do j = 1, cols
            grid(i, j) = line(j:j)
        end do
    end do

    close(10)
end subroutine read_grid

logical function check_diagonal_patterns(grid, row, col)
    implicit none
    character(len=1), dimension(:,:), intent(in) :: grid
    integer, intent(in) :: row, col
    character(len=1) :: a, b, c, d

    a = grid(row-1, col-1)
    b = grid(row+1, col+1)
    c = grid(row-1, col+1)
    d = grid(row+1, col-1)

    check_diagonal_patterns = &
        (((a == "S" .and. b == "M") .or. (a == "M" .and. b == "S")) .and. &
         ((c == "S" .and. d == "M") .or. (c == "M" .and. d == "S")))
end function check_diagonal_patterns

end module Day4_Part2
