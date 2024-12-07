module Day5_tests
    use Day5_Part2
    implicit none
contains
    subroutine test_bubble_sort()
        implicit none
        integer, allocatable :: pageNumbers(:), expectedPageNumbers(:)
        character(len=6), allocatable :: orderingRules(:)
        integer :: ruleCount, pageCount
        logical :: testResult
        integer :: passedTests, failedTests

        passedTests = 0
        failedTests = 0

        allocate(orderingRules(3))
        orderingRules = ['1|2', '1|3', '2|3']
        ruleCount = 3
        allocate(pageNumbers(3))
        pageNumbers = [3, 1, 2]
        pageCount = 3
        call bubble_sort(pageNumbers, pageCount, orderingRules, ruleCount)
        allocate(expectedPageNumbers(3))
        expectedPageNumbers = [1, 2, 3]
        testResult = all(pageNumbers(1:pageCount) == expectedPageNumbers(1:pageCount))
        if (testResult) then
            passedTests = passedTests + 1
            print *, "Passed: [3, 1, 2] sorted to [1, 2, 3]"
        else
            failedTests = failedTests + 1
            print *, "Failed: [3, 1, 2] sorted to ", pageNumbers(1:pageCount)
        end if
        deallocate(orderingRules, pageNumbers, expectedPageNumbers)

        allocate(orderingRules(3))
        orderingRules = ['2|1', '2|3', '1|3']
        ruleCount = 3
        allocate(pageNumbers(3))
        pageNumbers = [3, 2, 1]
        pageCount = 3
        call bubble_sort(pageNumbers, pageCount, orderingRules, ruleCount)
        allocate(expectedPageNumbers(3))
        expectedPageNumbers = [2, 1, 3]
        testResult = all(pageNumbers(1:pageCount) == expectedPageNumbers(1:pageCount))
        if (testResult) then
            passedTests = passedTests + 1
            print *, "Passed: [3, 2, 1] sorted to [2, 1, 3]"
        else
            failedTests = failedTests + 1
            print *, "Failed: [3, 2, 1] sorted to ", pageNumbers(1:pageCount)
        end if
        deallocate(orderingRules, pageNumbers, expectedPageNumbers)

        allocate(orderingRules(3))
        orderingRules = ['3|2', '3|1', '2|1']
        ruleCount = 3
        allocate(pageNumbers(3))
        pageNumbers = [1, 3, 2]
        pageCount = 3
        call bubble_sort(pageNumbers, pageCount, orderingRules, ruleCount)
        allocate(expectedPageNumbers(3))
        expectedPageNumbers = [3, 2, 1]
        testResult = all(pageNumbers(1:pageCount) == expectedPageNumbers(1:pageCount))
        if (testResult) then
            passedTests = passedTests + 1
            print *, "Passed: [1, 3, 2] sorted to [3, 2, 1]"
        else
            failedTests = failedTests + 1
            print *, "Failed: [1, 3, 2] sorted to ", pageNumbers(1:pageCount)
        end if
        deallocate(orderingRules, pageNumbers, expectedPageNumbers)

        allocate(orderingRules(6))
        orderingRules = ['1|2', '1|3', '2|3', '1|4', '2|4', '3|4']
        ruleCount = 6
        allocate(pageNumbers(4))
        pageNumbers = [4, 3, 2, 1]
        pageCount = 4
        call bubble_sort(pageNumbers, pageCount, orderingRules, ruleCount)
        allocate(expectedPageNumbers(4))
        expectedPageNumbers = [1, 2, 3, 4]
        testResult = all(pageNumbers(1:pageCount) == expectedPageNumbers(1:pageCount))
        if (testResult) then
            passedTests = passedTests + 1
            print *, "Passed: [4, 3, 2, 1] sorted to [1, 2, 3, 4]"
        else
            failedTests = failedTests + 1
            print *, "Failed: [4, 3, 2, 1] sorted to ", pageNumbers(1:pageCount)
        end if
        deallocate(orderingRules, pageNumbers, expectedPageNumbers)

        allocate(orderingRules(10))
        orderingRules = ['1|2', '1|3', '1|4', '1|5', '2|3', '2|4', '2|5', '3|4', '3|5', '4|5']
        ruleCount = 10
        allocate(pageNumbers(5))
        pageNumbers = [5, 4, 3, 2, 1]
        pageCount = 5
        call bubble_sort(pageNumbers, pageCount, orderingRules, ruleCount)
        allocate(expectedPageNumbers(5))
        expectedPageNumbers = [1, 2, 3, 4, 5]
        testResult = all(pageNumbers(1:pageCount) == expectedPageNumbers(1:pageCount))
        if (testResult) then
            passedTests = passedTests + 1
            print *, "Passed: [5, 4, 3, 2, 1] sorted to [1, 2, 3, 4, 5]"
        else
            failedTests = failedTests + 1
            print *, "Failed: [5, 4, 3, 2, 1] sorted to ", pageNumbers(1:pageCount)
        end if
        deallocate(orderingRules, pageNumbers, expectedPageNumbers)

        print *, "Tests Passed: ", passedTests
        print *, "Tests Failed: ", failedTests
    end subroutine test_bubble_sort
end module Day5_tests
