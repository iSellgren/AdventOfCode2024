module Day6_Part1
    implicit none

contains

    subroutine run_day6_part1()
        implicit none
        character(len=1), allocatable, dimension(:,:) :: maze
        integer :: rows, cols, distinct_positions
        character(len=256) :: file_name

        print *, "Running Day 6, Part 1"

        file_name = "input/Day6.txt"

        call read_maze(file_name, maze, rows, cols)
        call run_driller(maze, rows, cols, distinct_positions)
        print *, "Distinct positions visited: ", distinct_positions

        deallocate(maze)
    end subroutine run_day6_part1

    subroutine read_maze(file_name, maze, rows, cols)
        implicit none
        character(len=*), intent(in) :: file_name
        character(len=1), allocatable, dimension(:,:) :: maze
        integer, intent(out) :: rows, cols
        integer :: unit, ios

        call open_file(file_name, unit, ios)
        if (ios /= 0) then
            print *, "Error opening file: ", trim(file_name)
            stop
        end if

        call find_maze_size(unit, rows, cols, ios)
        call create_maze(maze, rows, cols)
        call populate_maze(unit, maze, ios)

        call close_file(unit)
    end subroutine read_maze

    subroutine open_file(file_name, unit, ios)
        implicit none
        character(len=*), intent(in) :: file_name
        integer, intent(out) :: unit, ios

        open(newunit=unit, file=file_name, status='old', action='read', iostat=ios)
    end subroutine open_file

    subroutine close_file(unit)
        implicit none
        integer, intent(in) :: unit

        close(unit)
    end subroutine close_file

    subroutine find_maze_size(unit, rows, cols, ios)
        implicit none
        integer, intent(in) :: unit
        integer, intent(out) :: rows, cols
        integer, intent(out) :: ios
        character(len=256) :: input_line
        integer :: max_line_len

        rows = 0
        cols = 0
        do
            read(unit, '(A)', iostat=ios) input_line
            if (ios /= 0) exit
            rows = rows + 1
            max_line_len = len_trim(input_line)
            cols = max(cols, max_line_len)
        end do
        rewind(unit)
    end subroutine find_maze_size

    subroutine create_maze(maze, rows, cols)
        implicit none
        character(len=1), allocatable, dimension(:,:) :: maze
        integer, intent(in) :: rows, cols

        allocate(maze(rows, cols))
        maze = ' '
    end subroutine create_maze

    subroutine populate_maze(unit, maze, ios)
        implicit none
        integer, intent(in) :: unit
        character(len=1), dimension(:,:), intent(out) :: maze
        integer, intent(out) :: ios
        character(len=256) :: input_line
        integer :: row_count, col_count

        row_count = 0
        do
            read(unit, '(A)', iostat=ios) input_line
            if (ios /= 0) exit

            row_count = row_count + 1
            do col_count = 1, len_trim(input_line)
                maze(row_count, col_count) = input_line(col_count:col_count)
            end do
        end do
    end subroutine populate_maze

    subroutine run_driller(maze, rows, cols, distinct_positions)
        implicit none
        character(len=1), intent(inout), dimension(:,:) :: maze
        integer, intent(in) :: rows, cols
        integer, intent(out) :: distinct_positions
        logical, allocatable, dimension(:,:) :: visited
        integer :: pos_x, pos_y, direction
        integer, dimension(4) :: dx = [-1, 0, 1, 0]
        integer, dimension(4) :: dy = [0, 1, 0, -1]
        integer :: next_x, next_y, steps
        character(len=1) :: pos_char
        logical :: in_bounds

        allocate(visited(rows, cols))
        visited = .false.

        call find_start_pos(maze, rows, cols, pos_x, pos_y, direction)

        steps = 0

        do
            call check_bounds(pos_x, pos_y, rows, cols, in_bounds)
            if (.not. in_bounds) exit

            if (.not. visited(pos_x, pos_y)) then
                steps = steps + 1
                visited(pos_x, pos_y) = .true.
                maze(pos_x, pos_y) = 'X'
            end if

            next_x = pos_x + dx(direction)
            next_y = pos_y + dy(direction)

            call check_bounds(next_x, next_y, rows, cols, in_bounds)
            if (.not. in_bounds) then
                steps = steps + 1
                exit
            end if

            pos_char = select_pos_char(direction)
            maze(pos_x, pos_y) = pos_char
            call print_maze(maze, rows, cols)
            if (is_obs(next_x, next_y, maze)) then
                direction = update_direction(direction)
            else
                maze(pos_x, pos_y) = 'X'
                pos_x = next_x
                pos_y = next_y
            end if
        end do

        distinct_positions = count_X(maze, rows, cols)
        deallocate(visited)
    end subroutine run_driller

    subroutine check_bounds(x, y, rows, cols, in_bounds)
        implicit none
        integer, intent(in) :: x, y, rows, cols
        logical, intent(out) :: in_bounds

        in_bounds = (x >= 1 .and. x <= rows .and. y >= 1 .and. y <= cols)
    end subroutine check_bounds

    logical function is_obs(x, y, maze)
        implicit none
        integer, intent(in) :: x, y
        character(len=1), intent(in), dimension(:,:) :: maze

        is_obs = (maze(x, y) == '#')
    end function is_obs

    integer function update_direction(current_direction)
        implicit none
        integer, intent(in) :: current_direction

        update_direction = mod(current_direction, 4) + 1
    end function update_direction

    integer function count_X(maze, rows, cols)
        implicit none
        character(len=1), intent(in), dimension(:,:) :: maze
        integer, intent(in) :: rows, cols
        integer :: i, j

        count_X = 0
        do i = 1, rows
            do j = 1, cols
                if (maze(i, j) == 'X') count_X = count_X + 1
            end do
        end do
    end function count_X

    subroutine find_start_pos(maze, rows, cols, pos_x, pos_y, direction)
        implicit none
        character(len=1), intent(in), dimension(:,:) :: maze
        integer, intent(in) :: rows, cols
        integer, intent(out) :: pos_x, pos_y, direction
        integer :: i, j

        pos_x = -1
        pos_y = -1
        direction = -1

        do i = 1, rows
            do j = 1, cols
                select case (maze(i, j))
                    case ('^')
                        pos_x = i
                        pos_y = j
                        direction = 1
                        return
                    case ('>')
                        pos_x = i
                        pos_y = j
                        direction = 2
                        return
                    case ('v')
                        pos_x = i
                        pos_y = j
                        direction = 3
                        return
                    case ('<')
                        pos_x = i
                        pos_y = j
                        direction = 4
                        return
                end select
            end do
        end do

        if (pos_x == -1) then
            print *, "No starting position found in the maze."
            stop
        end if
    end subroutine find_start_pos

    character(len=1) function select_pos_char(direction)
        implicit none
        integer, intent(in) :: direction

        select case (direction)
            case (1)
                select_pos_char = '^'
            case (2)
                select_pos_char = '>'
            case (3)
                select_pos_char = 'v'
            case (4)
                select_pos_char = '<'
            case default
                select_pos_char = ' '
        end select
    end function select_pos_char

    subroutine print_maze(maze, rows, cols)
        implicit none
        character(len=1), intent(in), dimension(:,:) :: maze
        integer, intent(in) :: rows, cols
        integer :: i, j
        character(len=:), allocatable :: row_output

        do i = 1, rows
            row_output = ''
            do j = 1, cols
                row_output = row_output // maze(i, j)
            end do
            print '(A)', trim(row_output)
        end do
    end subroutine print_maze

end module Day6_Part1
