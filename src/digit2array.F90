module BaseFunction
    implicit none

    ! 常量定义
    integer(4), parameter :: companyNumber = 3
    integer(4), parameter :: itemNumber = 5
    integer(4), parameter :: judgeNumber = 7
    integer(4), parameter :: judgeOneNumber = 6
    integer(4), parameter :: oneCaseNumber = judgeOneNumber ** itemNumber
    real(8)   , parameter :: allCaseNumber = dble(oneCaseNumber) ** dble(judgeNumber)
    integer(4), parameter :: judgeOne(judgeOneNumber, companyNumber) = &
                            (/0, 1, 2, &
                              0, 2, 1, &
                              1, 0, 2, &
                              1, 2, 0, &
                              2, 0, 1, &
                              2, 1, 0/)

    integer(4), parameter :: itemScores(itemNumber, companyNumber) = &
                            (/25, 20, 15, &
                            25, 20, 15, &
                            10, 8, 6, &
                            10, 8, 6, &
                            30, 25, 20/)

    contains

        subroutine addOne(caseIndex, width, number)
            implicit none
            integer(4), intent(in) :: width
            integer(4), intent(inout) :: caseIndex(width)
            integer(4), intent(in) :: number
            integer(4) :: i

            do i = width, 1, -1
                if (caseIndex(i) == number - 1) then
                    caseIndex(i) = 0
                else
                    caseIndex(i) = caseIndex(i) + 1
                    exit
                end if
            end do

            return
        end subroutine addOne

        subroutine real2Array(caseIndex, width, number, value)
            implicit none
            integer(4), intent(in) :: width
            integer(4), intent(inout) :: caseIndex(width)
            integer(4), intent(in) :: number
            real(8), intent(in) :: value
            real(8) :: tmpMax, temValue
            integer(4) :: i
            
            if (value < (dble(number) ** dble(width)) .and. value >= 0) then
                caseIndex = 0
                temValue = value

                do i = 1, width
                    tmpMax = dble(number) ** dble(width-i)

                    if (temValue <= 0.5d0) then
                        exit
                    
                    else if (temValue >= tmpMax) then
                        caseIndex(i) = int(temValue / tmpMax)
                        temValue = temValue - dble(caseIndex(i)) * tmpMax

                    end if
                end do
            end if

            return
        end subroutine

end module BaseFunction