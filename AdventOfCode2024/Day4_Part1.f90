module Day4_Part1
    implicit none
contains

subroutine run_day4_part1()
    implicit none
    character(len=256), parameter :: file = 'input/Day4.txt'
    character(len=1), allocatable, dimension(:,:) :: grid
    integer :: i, j, row_count, col_count, count

    count = 0
    row_count = 0
    col_count = 0
    print *, "Kor Day 4, Part 1"
    call get_grid_dimensions(file, row_count, col_count)
    allocate(grid(row_count, col_count))
    call read_grid(file, grid, row_count, col_count)

    do i = 1, row_count
        do j = 1, col_count
            count = count + count_mas_patterns(grid, i, j, row_count, col_count)
        end do
    end do


    print *, "res: ", count

end subroutine run_day4_part1

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

function count_mas_patterns(grid, row, col, total_rows, total_cols) result(count)
    implicit none
    character(len=1), dimension(:,:), intent(in) :: grid
    integer, intent(in) :: row, col, total_rows, total_cols
    integer :: count

    count = 0

    if (col + 3 <= total_cols) then
        if (grid(row, col) == 'X' .and. grid(row, col+1) == 'M' .and. grid(row, col+2) == 'A' .and. grid(row, col+3) == 'S') count = count + 1
        if (grid(row, col) == 'S' .and. grid(row, col+1) == 'A' .and. grid(row, col+2) == 'M' .and. grid(row, col+3) == 'X') count = count + 1
    end if

    if (row + 3 <= total_rows) then
        if (grid(row, col) == 'X' .and. grid(row+1, col) == 'M' .and. grid(row+2, col) == 'A' .and. grid(row+3, col) == 'S') count = count + 1
        if (grid(row, col) == 'S' .and. grid(row+1, col) == 'A' .and. grid(row+2, col) == 'M' .and. grid(row+3, col) == 'X') count = count + 1
    end if

    if (row + 3 <= total_rows .and. col + 3 <= total_cols) then
        if (grid(row, col) == 'X' .and. grid(row+1, col+1) == 'M' .and. grid(row+2, col+2) == 'A' .and. grid(row+3, col+3) == 'S') count = count + 1
        if (grid(row, col) == 'S' .and. grid(row+1, col+1) == 'A' .and. grid(row+2, col+2) == 'M' .and. grid(row+3, col+3) == 'X') count = count + 1
    end if

    if (row - 3 >= 1 .and. col + 3 <= total_cols) then
        if (grid(row, col) == 'X' .and. grid(row-1, col+1) == 'M' .and. grid(row-2, col+2) == 'A' .and. grid(row-3, col+3) == 'S') count = count + 1
        if (grid(row, col) == 'S' .and. grid(row-1, col+1) == 'A' .and. grid(row-2, col+2) == 'M' .and. grid(row-3, col+3) == 'X') count = count + 1
    end if

end function count_mas_patterns

end module Day4_Part1
