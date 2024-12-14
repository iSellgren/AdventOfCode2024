module Day7_Part2
    use Day7_Part1, only: ParseInput, SplitLine, ExtractNums, ReadInt, ReadIntArray
    implicit none

    integer, parameter :: IntegerKind = selected_int_kind(15)
    integer(kind=IntegerKind), parameter :: CalcAdd = 1, CalcMultiply = 2, CalcConcat = 3

contains

    subroutine run_day7_part2()
        implicit none

        integer(kind=IntegerKind) :: InputUnit, FinalResult

        FinalResult = 0

        open(newunit=InputUnit, file='input/Day7.txt', status='old', action='read')

        do while (ProcessInput(InputUnit, FinalResult))
        end do

        close(InputUnit)

        print *, "Final result:", FinalResult
    end subroutine run_day7_part2

    function ProcessInput(InputUnit, FinalResult) result(HasMoreLines)
        integer(kind=IntegerKind), intent(in) :: InputUnit
        integer(kind=IntegerKind), intent(inout) :: FinalResult
        logical :: HasMoreLines

        character(len=256) :: InputLine
        integer(kind=IntegerKind), dimension(:), allocatable :: NumList
        integer(kind=IntegerKind) :: TargetNum, InputStatus
        logical :: IsEquationValid

        read(InputUnit, '(A)', iostat=InputStatus) InputLine
        if (InputStatus /= 0) then
            HasMoreLines = .false.
            return
        end if

        call ParseInput(InputLine, TargetNum, NumList)

        IsEquationValid = CheckEquation(NumList, int(size(NumList), kind=IntegerKind), TargetNum)
        if (IsEquationValid) then
            FinalResult = FinalResult + TargetNum
        end if

        deallocate(NumList)
        HasMoreLines = .true.
    end function ProcessInput

    recursive function CheckEquation(NumList, ListSize, TargetNum) result(IsValid)
        integer(kind=IntegerKind), intent(in) :: NumList(:)
        integer(kind=IntegerKind), intent(in) :: ListSize, TargetNum
        logical :: IsValid

        if (ListSize == 1) then
            IsValid = (NumList(1) == TargetNum)
            return
        end if

        IsValid = TryCalcs(NumList, ListSize, TargetNum)
    end function CheckEquation

    function TryCalcs(NumList, ListSize, TargetNum) result(IsValid)
        integer(kind=IntegerKind), intent(in) :: NumList(:)
        integer(kind=IntegerKind), intent(in) :: ListSize, TargetNum
        logical :: IsValid

        integer(kind=IntegerKind), allocatable :: UpdatedNumList(:)
        integer(kind=IntegerKind) :: CalculatedResult, CurrentCalc

        IsValid = .false.

        do CurrentCalc = CalcAdd, CalcConcat
            CalculatedResult = PerformCalc(NumList(1), NumList(2), CurrentCalc)

            if (CalculatedResult < 0) cycle

            allocate(UpdatedNumList(ListSize - 1))
            UpdatedNumList(1) = CalculatedResult
            if (ListSize > 2) UpdatedNumList(2:) = NumList(3:ListSize)

            if (CheckEquation(UpdatedNumList, ListSize - 1, TargetNum)) then
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
                print *, "Multiply: ", Value1, "*", Value2, "=", ResultValue
            case (CalcConcat)
                ResultValue = Concatenate(Value1, Value2)
                print *, "Concat: ", Value1, "//", Value2, "=", ResultValue
        end select
    end function PerformCalc


    function Concatenate(Value1, Value2) result(ResultValue)
        integer(kind=IntegerKind), intent(in) :: Value1, Value2
        integer(kind=IntegerKind) :: ResultValue
        character(len=64) :: CombinedString
        character(len=32) :: String1, String2

        write(String1, '(I0)') Value1
        write(String2, '(I0)') Value2
        CombinedString = trim(String1) // trim(String2)

        read(CombinedString, *) ResultValue
    end function Concatenate

end module Day7_Part2
