module Day8_Part1
    implicit none

    integer, parameter :: BigNumber = selected_int_kind(15)

    type :: Pos
        integer(kind=BigNumber) :: x
        integer(kind=BigNumber) :: y
    end type Pos

    type :: FrequencyGroup
        character(len=1) :: frequency
        type(Pos), allocatable :: antennas(:)
    end type FrequencyGroup

contains

    subroutine run_day8_part1()
        implicit none

        integer(kind=BigNumber) :: FileUnit
        integer(kind=BigNumber) :: MaxX, MaxY
        type(FrequencyGroup), allocatable :: frequencyGroups(:)
        type(Pos), allocatable :: emptySpots(:)
        integer(kind=BigNumber) :: TotalCount
        integer :: Status

        print *, "Kor Day 8, Part 1"
        print *, "Opening and reading input file..."

        open(newunit=FileUnit, file='input/Day8.txt', status='old', action='read', iostat=Status)
        if (Status /= 0) then
            print *, "Error: Cannot open input file 'input/Day8.txt'."
            stop 1
        endif

        allocate(frequencyGroups(0))
        call ReadFile(FileUnit, MaxX, MaxY, frequencyGroups)
        close(FileUnit)
        print *, "Reading done. MaxX:", MaxX, " MaxY:", MaxY

        print *, "Finding empty spots..."
        call FindEmptySpots(MaxX, MaxY, frequencyGroups, emptySpots)
        print *, "Empty spots found."

        TotalCount = size(emptySpots)
        print *, "Total empty spots:", TotalCount - 1

        if (allocated(frequencyGroups)) deallocate(frequencyGroups)
        if (allocated(emptySpots)) deallocate(emptySpots)

    end subroutine run_day8_part1

    subroutine ReadFile(FileUnit, MaxX, MaxY, frequencyGroups)
        implicit none
        integer(kind=BigNumber), intent(in) :: FileUnit
        integer(kind=BigNumber), intent(out) :: MaxX, MaxY
        type(FrequencyGroup), allocatable, intent(out) :: frequencyGroups(:)

        character(len=256) :: LineFromFile
        integer(kind=BigNumber) :: x, y, Status
        character(len=1) :: frequency
        integer(kind=BigNumber) :: FoundIndex

        allocate(frequencyGroups(0))
        y = 0
        MaxX = 0

        do
            read(FileUnit, '(A)', iostat=Status) LineFromFile
            if (Status /= 0) exit

            y = y + 1
            MaxX = max(MaxX, len_trim(LineFromFile) - 1)

            do x = 0, len_trim(LineFromFile) - 1
                frequency = LineFromFile(x + 1:x + 1)
                if (frequency == '.') cycle

                FoundIndex = FindFrequencyGroup(frequencyGroups, frequency)

                if (FoundIndex == 0) then
                    call AddFrequencyGroup(frequencyGroups, frequency, x, y - 1)
                else
                    call AddAntennaToGroup(frequencyGroups(FoundIndex), x, y - 1)
                endif
            end do
        end do

        MaxY = y - 1
        print *, "File read. Frequency Groups:", size(frequencyGroups)

    end subroutine ReadFile

    integer function FindFrequencyGroup(frequencyGroups, frequency) result(Index)
        implicit none
        type(FrequencyGroup), intent(in) :: frequencyGroups(:)
        character(len=1), intent(in) :: frequency
        integer(kind=BigNumber) :: i

        Index = 0
        do i = 1, size(frequencyGroups)
            if (frequencyGroups(i)%frequency == frequency) then
                Index = i
                return
            endif
        end do
    end function FindFrequencyGroup

    subroutine AddFrequencyGroup(frequencyGroups, frequency, x, y)
        implicit none
        type(FrequencyGroup), allocatable, intent(inout) :: frequencyGroups(:)
        character(len=1), intent(in) :: frequency
        integer(kind=BigNumber), intent(in) :: x, y
        type(FrequencyGroup), allocatable :: TempGroups(:)

        allocate(TempGroups(size(frequencyGroups) + 1))
        if (size(frequencyGroups) > 0) then
            TempGroups(1:size(frequencyGroups)) = frequencyGroups
            deallocate(frequencyGroups)
        end if

        TempGroups(size(TempGroups))%frequency = frequency
        allocate(TempGroups(size(TempGroups))%antennas(1))
        TempGroups(size(TempGroups))%antennas(1)%x = x
        TempGroups(size(TempGroups))%antennas(1)%y = y

        frequencyGroups = TempGroups
        deallocate(TempGroups)

    end subroutine AddFrequencyGroup

    subroutine AddAntennaToGroup(Group, x, y)
        implicit none
        type(FrequencyGroup), intent(inout) :: Group
        integer(kind=BigNumber), intent(in) :: x, y
        type(Pos), allocatable :: TempAntennas(:)
        logical :: Exists
        integer :: i

        Exists = .false.
        do i = 1, size(Group%antennas)
            if (Group%antennas(i)%x == x .and. Group%antennas(i)%y == y) then
                Exists = .true.
                exit
            endif
        end do

        if (.not. Exists) then
            allocate(TempAntennas(size(Group%antennas) + 1))
            TempAntennas(1:size(Group%antennas)) = Group%antennas
            TempAntennas(size(TempAntennas))%x = x
            TempAntennas(size(TempAntennas))%y = y

            deallocate(Group%antennas)
            Group%antennas = TempAntennas
            deallocate(TempAntennas)
        endif

    end subroutine AddAntennaToGroup

    subroutine FindEmptySpots(MaxX, MaxY, frequencyGroups, emptySpots)
        implicit none
        integer(kind=BigNumber), intent(in) :: MaxX, MaxY
        type(FrequencyGroup), allocatable, intent(in) :: frequencyGroups(:)
        type(Pos), allocatable, intent(out) :: emptySpots(:)

        integer(kind=BigNumber) :: I, J, K
        type(Pos), allocatable :: TempEmpty(:)

        if (allocated(emptySpots)) deallocate(emptySpots)
        allocate(emptySpots(0))

        do I = 1, size(frequencyGroups)
            do J = 1, size(frequencyGroups(I)%antennas) - 1
                do K = J + 1, size(frequencyGroups(I)%antennas)
                    call CheckSpots(MaxX, MaxY, frequencyGroups(I)%antennas(J), frequencyGroups(I)%antennas(K), TempEmpty)
                    call AddPlaces(emptySpots, TempEmpty)
                    if (allocated(TempEmpty)) deallocate(TempEmpty)
                end do
            end do
        end do

    end subroutine FindEmptySpots

    subroutine AddPlaces(ResultSet, NewPlaces)
        implicit none
        type(Pos), allocatable, intent(inout) :: ResultSet(:)
        type(Pos), allocatable, intent(in) :: NewPlaces(:)
        type(Pos), allocatable :: TempPoss(:)
        integer(kind=BigNumber) :: i, j, k
        logical :: IsDuplicate
        integer(kind=BigNumber) :: NewSize

        if (.not. allocated(ResultSet)) then
            allocate(ResultSet(size(NewPlaces)))
            ResultSet = NewPlaces
        else
            NewSize = size(ResultSet)
            do i = 1, size(NewPlaces)
                IsDuplicate = .false.
                do k = 1, size(ResultSet)
                    if (ResultSet(k)%x == NewPlaces(i)%x .and. &
                        ResultSet(k)%y == NewPlaces(i)%y) then
                        IsDuplicate = .true.
                        exit
                    endif
                end do
                if (.not. IsDuplicate) then
                    NewSize = NewSize + 1
                endif
            end do

            allocate(TempPoss(NewSize))
            TempPoss(1:size(ResultSet)) = ResultSet

            k = size(ResultSet)
            do i = 1, size(NewPlaces)
                IsDuplicate = .false.
                do j = 1, size(ResultSet)
                    if (ResultSet(j)%x == NewPlaces(i)%x .and. &
                        ResultSet(j)%y == NewPlaces(i)%y) then
                        IsDuplicate = .true.
                        exit
                    endif
                end do
                if (.not. IsDuplicate) then
                    k = k + 1
                    TempPoss(k) = NewPlaces(i)
                endif
            end do

            deallocate(ResultSet)
            allocate(ResultSet(size(TempPoss)))
            ResultSet = TempPoss
            deallocate(TempPoss)
        endif

    end subroutine AddPlaces

    subroutine CheckSpots(MaxX, MaxY, AntennaA, AntennaB, EmptySpots)
        implicit none
        integer(kind=BigNumber), intent(in) :: MaxX, MaxY
        type(Pos), intent(in) :: AntennaA, AntennaB
        type(Pos), allocatable, intent(out) :: EmptySpots(:)

        integer(kind=BigNumber) :: deltaX, deltaY

        deltaX = AntennaB%x - AntennaA%x
        deltaY = AntennaB%y - AntennaA%y

        if (allocated(EmptySpots)) deallocate(EmptySpots)
        allocate(EmptySpots(2))

        EmptySpots(1)%x = AntennaA%x - deltaX
        EmptySpots(1)%y = AntennaA%y - deltaY
        EmptySpots(2)%x = AntennaB%x + deltaX
        EmptySpots(2)%y = AntennaB%y + deltaY

        if (.not. IsInBounds(EmptySpots(1), MaxX, MaxY)) then
            EmptySpots(1)%x = -1
            EmptySpots(1)%y = -1
        endif
        if (.not. IsInBounds(EmptySpots(2), MaxX, MaxY)) then
            EmptySpots(2)%x = -1
            EmptySpots(2)%y = -1
        endif

    end subroutine CheckSpots

    logical function IsInBounds(currPos, MaxX, MaxY)
        implicit none
        type(Pos), intent(in) :: currPos
        integer(kind=BigNumber), intent(in) :: MaxX, MaxY

        IsInBounds = (currPos%x >= 0 .and. currPos%x <= MaxX) .and. &
                    (currPos%y >= 0 .and. currPos%y <= MaxY)
    end function IsInBounds

end module Day8_Part1
