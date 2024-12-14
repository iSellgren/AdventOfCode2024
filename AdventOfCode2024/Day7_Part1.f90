module Day7_Part1
    implicit none

    integer, parameter :: IntegerKind = selected_int_kind(15)
    integer(kind=IntegerKind), parameter :: CalcAdd = 1, CalcMultiply = 2

contains

    subroutine run_day7_part1()
        implicit none

        integer(kind=IntegerKind) :: InputUnit, FinalResult

        FinalResult = 0

        open(newunit=InputUnit, file='input/Day7.txt', status='old', action='read')

        do while (ProcessInput(InputUnit, FinalResult))
        end do

        close(InputUnit)

        print *, "Final result:", FinalResult
    end subroutine run_day7_part1

    function ProcessInput(InputUnit, FinalResult) result(HasMoreLines)
        integer(kind=IntegerKind), intent(in) :: InputUnit
        integer(kind=IntegerKind), intent(inout) :: FinalResult
        logical :: HasMoreLines

        character(len=256) :: InputLine
        integer(kind=IntegerKind), dimension(:), allocatable :: NumList
        integer(kind=IntegerKind) :: TargetNum, InputStatus
        logical :: IsEqValid

        read(InputUnit, '(A)', iostat=InputStatus) InputLine
        if (InputStatus /= 0) then
            HasMoreLines = .false.
            return
        end if

        call ParseInput(InputLine, TargetNum, NumList)
        IsEqValid = CheckEq(NumList, size(NumList), TargetNum)
        if (IsEqValid) then
            FinalResult = FinalResult + TargetNum
        end if

        deallocate(NumList)
        HasMoreLines = .true.
    end function ProcessInput

    recursive function CheckEq(NumList, ListSize, TargetNum) result(IsValid)
        integer(kind=IntegerKind), intent(in) :: NumList(:)
        integer(kind=IntegerKind), intent(in) :: ListSize, TargetNum
        logical :: IsValid

        if (ListSize == 1) then
            IsValid = (NumList(1) == TargetNum)
            print *, "Base case: Comparing", NumList(1), "with target", TargetNum, ". Result:", IsValid
            return
        end if

        IsValid = TryCalcs(NumList, ListSize, TargetNum)
    end function CheckEq

    function TryCalcs(NumList, ListSize, TargetNum) result(IsValid)
        integer(kind=IntegerKind), intent(in) :: NumList(:)
        integer(kind=IntegerKind), intent(in) :: ListSize, TargetNum
        logical :: IsValid

        integer(kind=IntegerKind), allocatable :: UpdatedNumList(:)
        integer(kind=IntegerKind) :: CalculatedResult, CurrentCalc

        IsValid = .false.

        do CurrentCalc = CalcAdd, CalcMultiply
            CalculatedResult = PerformCalc(NumList(1), NumList(2), CurrentCalc)

            print *, "Running on", NumList(1), "and", NumList(2), ". Result:", CalculatedResult

            allocate(UpdatedNumList(ListSize - 1))
            UpdatedNumList(1) = CalculatedResult
            if (ListSize > 2) UpdatedNumList(2:) = NumList(3:ListSize)

            if (CheckEq(UpdatedNumList, ListSize - 1, TargetNum)) then
                IsValid = .true.
                deallocate(UpdatedNumList)
                return
            end if

            deallocate(UpdatedNumList)
        end do
    end function TryCalcs

    function PerformCalc(Value1, Value2, Calc) result(ResultValue)
        integer(kind=IntegerKind), intent(in) :: Value1, Value2, Calc
        integer(kind=IntegerKind) :: ResultValue

        select case (Calc)
            case (CalcAdd)
                ResultValue = Value1 + Value2
                print *, "Add: ", Value1, "+", Value2, "=", ResultValue
            case (CalcMultiply)
                ResultValue = Value1 * Value2
                print *, "Multi: ", Value1, "*", Value2, "=", ResultValue
        end select
    end function PerformCalc

    subroutine ParseInput(InputLine, TargetNum, NumList)
        character(len=*), intent(in) :: InputLine
        integer(kind=IntegerKind), intent(out) :: TargetNum
        integer(kind=IntegerKind), allocatable, intent(out) :: NumList(:)

        character(len=256) :: TargetPart, NumsPart

        call SplitLine(InputLine, ":", TargetPart, NumsPart)

        TargetNum = ReadInt(TargetPart)
        call ExtractNums(NumsPart, NumList)
    end subroutine ParseInput

    subroutine SplitLine(InputString, Delimiter, LeftPart, RightPart)
        character(len=*), intent(in) :: InputString, Delimiter
        character(len=256), intent(out) :: LeftPart, RightPart

        integer :: ColonPosition

        ColonPosition = index(InputString, Delimiter)
        if (ColonPosition == 0) stop "Invalid input line: " // trim(InputString)

        LeftPart = trim(adjustl(InputString(1:ColonPosition - 1)))
        RightPart = trim(adjustl(InputString(ColonPosition + len(Delimiter):)))
    end subroutine SplitLine

    subroutine ExtractNums(NumString, NumList)
        character(len=*), intent(in) :: NumString
        integer(kind=IntegerKind), allocatable, intent(out) :: NumList(:)
        character(len=16), dimension(:), allocatable :: SplitNums

        call SplitString(NumString, SplitNums)
        NumList = ReadIntArray(SplitNums)

    end subroutine ExtractNums

    subroutine SplitString(OriginalString, PartsArray)
        character(len=*), intent(in) :: OriginalString
        character(len=16), allocatable, intent(out) :: PartsArray(:)

        integer :: PartCount, i, StartPos

        PartCount = CountParts(OriginalString)
        allocate(PartsArray(PartCount))

        StartPos = 1
        PartCount = 0

        do i = 1, len_trim(OriginalString)
            if (OriginalString(i:i) == ' ' .or. i == len_trim(OriginalString)) then
                if (i == len_trim(OriginalString) .and. OriginalString(i:i) /= ' ') then
                    PartCount = PartCount + 1
                    PartsArray(PartCount) = trim(adjustl(OriginalString(StartPos:i)))
                else if (i > StartPos) then
                    PartCount = PartCount + 1
                    PartsArray(PartCount) = trim(adjustl(OriginalString(StartPos:i - 1)))
                end if
                StartPos = i + 1
            end if
        end do
    end subroutine SplitString

    function CountParts(InputString) result(PartCount)
        character(len=*), intent(in) :: InputString
        integer :: PartCount, i

        PartCount = 0
        do i = 1, len_trim(InputString)
            if (InputString(i:i) == ' ' .or. i == len_trim(InputString)) then
                PartCount = PartCount + 1
            end if
        end do
    end function CountParts

    function ReadInt(InputString) result(Value)
        character(len=*), intent(in) :: InputString
        integer(kind=IntegerKind) :: Value

        read(InputString, *) Value
    end function ReadInt

    function ReadIntArray(StringArray) result(IntegerArray)
        character(len=16), intent(in) :: StringArray(:)
        integer(kind=IntegerKind), allocatable :: IntegerArray(:)
        integer :: i

        allocate(IntegerArray(size(StringArray)))
        do i = 1, size(StringArray)
            IntegerArray(i) = ReadInt(StringArray(i))
        end do
    end function ReadIntArray


end module Day7_Part1
