module Day8_Part2
    use Day8_Part1
    implicit none

contains

    recursive integer(kind=8) function gcd(a, b)
        integer(kind=8), intent(in) :: a, b
        integer(kind=8) :: temp
        if (b == 0) then
            gcd = a
            return
        endif
        temp = mod(a, b)
        if (temp == 0) then
            gcd = b
            return
        else
            gcd = gcd(b, temp)
        endif
    end function gcd

    subroutine run_day8_part2()
        implicit none

        integer(kind=8) :: FileUnit
        integer(kind=8) :: maxX, maxY
        type(FrequencyGroup), allocatable :: frequencyGroups(:)
        integer :: numFrequencies
        logical, allocatable :: visitedCells(:,:)
        integer(kind=8) :: totalVisitedCells
        integer(kind=8) :: i, j, k
        integer(kind=8) :: status

        maxX = 0
        maxY = 0
        numFrequencies = 0
        allocate(frequencyGroups(0))
        print *, "Kor Day 8, Part 2"


        open(newunit=FileUnit, file='input/Day8.txt', status='old', action='read', iostat=status)
        if (status /= 0) then
            print *, "Error: Cannot open input file 'input/Day8.txt'."
            stop 1
        endif

        call ReadFile(FileUnit, maxX, maxY, frequencyGroups)

        close(FileUnit)

        print *, "Grid dimensions: maxX =", maxX, ", maxY =", maxY

        allocate(visitedCells(maxX + 1, maxY + 1))
        visitedCells = .false.

        do i = 1, size(frequencyGroups)
            if (size(frequencyGroups(i)%antennas) >= 2) then
                do j = 1, size(frequencyGroups(i)%antennas) - 1
                    do k = j + 1, size(frequencyGroups(i)%antennas)
                        call markAntennasPaths(frequencyGroups(i)%antennas(j), frequencyGroups(i)%antennas(k), visitedCells, maxX, maxY)
                    end do
                end do
                do j = 1, size(frequencyGroups(i)%antennas)
                    visitedCells(frequencyGroups(i)%antennas(j)%x + 1, frequencyGroups(i)%antennas(j)%y + 1) = .true.
                end do
            endif
        end do
        totalVisitedCells = 0
        do i = 1, maxX + 1
            do j = 1, maxY + 1
                if (visitedCells(i,j)) then
                    totalVisitedCells = totalVisitedCells + 1
                endif
            end do
        end do

        print *, "Total unique visited cells:", totalVisitedCells
    end subroutine run_day8_part2

    subroutine PrintFrequencyGroups(frequencyGroups)
        implicit none
        type(FrequencyGroup), intent(in) :: frequencyGroups(:)
        integer(kind=BigNumber) :: i, j

        do i = 1, size(frequencyGroups)
            write(*, '(A)', advance='no') "Frequency '"
            write(*, '(A)', advance='no') frequencyGroups(i)%frequency
            write(*, '(A)', advance='no') "': "
            do j = 1, size(frequencyGroups(i)%antennas)
                write(*, '(A, I0, A, I0, A)', advance='no') "  [", &
                      frequencyGroups(i)%antennas(j)%x, ", ", &
                      frequencyGroups(i)%antennas(j)%y, "]"
            end do
            write(*,*)
        end do
    end subroutine PrintFrequencyGroups

    subroutine AddAntennaToGroupByFrequency(frequencyGroups, numFrequencies, freqChar, x, y)
        implicit none
        type(FrequencyGroup), allocatable, intent(inout) :: frequencyGroups(:)
        integer, intent(inout) :: numFrequencies
        character(len=1), intent(in) :: freqChar
        integer(kind=8), intent(in) :: x, y

        integer(kind=8) :: idx
        logical :: found
        type(FrequencyGroup), allocatable :: tempFreq(:)

        found = .false.
        idx = 0

        do idx = 1, size(frequencyGroups)
            if (frequencyGroups(idx)%frequency == freqChar) then
                found = .true.
                exit
            endif
        end do

        if (.not. found) then
            numFrequencies = numFrequencies + 1
            allocate(tempFreq(numFrequencies))
            if (size(frequencyGroups) > 0) then
                tempFreq(1:size(frequencyGroups)) = frequencyGroups
            endif
            tempFreq(numFrequencies)%frequency = freqChar
            allocate(tempFreq(numFrequencies)%antennas(1))
            tempFreq(numFrequencies)%antennas(1)%x = x
            tempFreq(numFrequencies)%antennas(1)%y = y
            if (allocated(frequencyGroups)) then
                deallocate(frequencyGroups)
            endif
            frequencyGroups = tempFreq
            deallocate(tempFreq)
        else
            call AddAntennaPos(frequencyGroups(idx)%antennas, x, y)
        endif
    end subroutine AddAntennaToGroupByFrequency

    subroutine AddAntennaPos(antennas, x, y)
        implicit none
        type(Pos), allocatable, intent(inout) :: antennas(:)
        integer(kind=8), intent(in) :: x, y

        integer(kind=8) :: newSize
        type(Pos), allocatable :: tempAntennas(:)
        logical :: exists
        integer :: i

        exists = .false.
        do i = 1, size(antennas)
            if (antennas(i)%x == x .and. antennas(i)%y == y) then
                exists = .true.
                exit
            endif
        end do

        if (.not. exists) then
            newSize = size(antennas) + 1
            allocate(tempAntennas(newSize))
            tempAntennas(1:size(antennas)) = antennas
            tempAntennas(newSize)%x = x
            tempAntennas(newSize)%y = y

            deallocate(antennas)
            antennas = tempAntennas
            deallocate(tempAntennas)
        endif
    end subroutine AddAntennaPos

    subroutine markAntennasPaths(antennaA, antennaB, visitedCells, maxX, maxY)
        implicit none
        type(Pos), intent(in) :: antennaA, antennaB
        logical, intent(inout) :: visitedCells(:,:)
        integer(kind=8), intent(in) :: maxX, maxY

        integer(kind=8) :: dx, dy
        integer(kind=8) :: gcd_val
        integer(kind=8) :: step_x, step_y
        type(Pos) :: currPos

        dx = antennaA%x - antennaB%x
        dy = antennaA%y - antennaB%y

        if (dx == 0 .and. dy == 0) then
            return
        endif

        gcd_val = gcd(abs(dx), abs(dy))
        if (gcd_val == 0) gcd_val = 1

        step_x = dx / gcd_val
        step_y = dy / gcd_val

        currPos%x = antennaA%x
        currPos%y = antennaA%y
        do
            currPos%x = currPos%x - step_x
            currPos%y = currPos%y - step_y
            if (currPos%x < 0 .or. currPos%x > maxX .or. &
                currPos%y < 0 .or. currPos%y > maxY) exit
            if (.not. visitedCells(currPos%x + 1, currPos%y + 1)) then
                visitedCells(currPos%x + 1, currPos%y + 1) = .true.
            endif
        end do

        currPos%x = antennaB%x
        currPos%y = antennaB%y
        do
            currPos%x = currPos%x + step_x
            currPos%y = currPos%y + step_y
            if (currPos%x < 0 .or. currPos%x > maxX .or. &
                currPos%y < 0 .or. currPos%y > maxY) exit
            if (.not. visitedCells(currPos%x + 1, currPos%y + 1)) then
                visitedCells(currPos%x + 1, currPos%y + 1) = .true.
            endif
        end do
    end subroutine markAntennasPaths

end module Day8_Part2
