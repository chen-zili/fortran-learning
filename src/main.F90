program main
    use BaseFunction

	implicit none

    ! 穷举法-->排列组合
    ! Coarray

    ! 变量定义
    integer(4) :: caseJudgeIndex(judgeNumber) = 0       ! oneCaseNumber  ** judgeNumber
    integer(4) :: caseJudgeStopIndex(judgeNumber) = 0
    integer(4) :: caseItemIndex(itemNumber)             ! judgeOneNumber ** itemNumber
    integer(4) :: scoreTabOne(judgeNumber, itemNumber, companyNumber) = 0

	integer(4) :: time_start, time_end
	integer(4) :: rank = 0, size = 0, i, j, k, m, n

    integer(4) :: counts[*] = 0
    real(8) :: allcount = 0.d0

    ! image info
    rank = this_image()
    size = num_images()

    ! 计时开始
	if (rank == 1) then
		call system_clock(time_start)
	end if

    ! 确定image范围
    if (1 == rank) then
        caseJudgeIndex = 0
        call real2Array(caseJudgeStopIndex, judgeNumber, oneCaseNumber, &
                        allCaseNumber / dble(size) * dble(rank))
        
    else if (size == rank) then
        call real2Array(caseJudgeIndex, judgeNumber, oneCaseNumber, &
                        allCaseNumber / dble(size) * dble(rank-1))
        call addOne(caseJudgeIndex, judgeNumber, oneCaseNumber)

        caseJudgeStopIndex = oneCaseNumber - 1

    else
        call real2Array(caseJudgeIndex, judgeNumber, oneCaseNumber, &
                        allCaseNumber / dble(size) * dble(rank-1))
        call addOne(caseJudgeIndex, judgeNumber, oneCaseNumber)

        call real2Array(caseJudgeStopIndex, judgeNumber, oneCaseNumber, &
                        allCaseNumber / dble(size) * dble(rank))
    end if

    ! write(*, '(a, i, *(i))') 'Start', rank, caseJudgeIndex
    ! write(*, '(a, i, *(i))') 'Stop ', rank, caseJudgeStopIndex


	sync all											! 等待所有image运行完

	if (rank == 1) then
		do i = 1, size
			allcount = allcount + dble(counts[i])		! 累积所有image的结果
		end do

        write(*, *) ""
        write(*, '(a, f12.0)') "Case Counts: ", allcount
		write(*, '(a, es12.4)') "Eq Probability: ", allcount / allCaseNumber * 100.d0

        ! 计时结束
		call system_clock(time_end)
		write(*, '(a, f10.4)') "Time (s): ", dble(time_end-time_start) / 1000.d0

	end if

	stop
end program main

