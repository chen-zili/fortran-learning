program main
    use BaseFunction

	implicit none

    ! 穷举法-->排列组合
    ! Coarray

    ! 变量定义
    integer(4) :: caseJudgeIndex(judgeNumber) = 0       ! oneCaseNumber  ** judgeNumber
    integer(4) :: caseJudgeStopIndex(judgeNumber) = 0
    integer(4) :: caseItemIndex(itemNumber) = 0         ! judgeOneNumber ** itemNumber
    integer(4) :: scoreTabOne(judgeNumber, itemNumber, companyNumber) = 0
    real(8) :: socreOne(companyNumber) = 0
    real(8) :: socreOneTmp(judgeNumber) = 0

	integer(8) :: time_start, time_end
	integer(4) :: rank = 0, size = 0, i, j, k, m, n

    integer(4) :: counts[*] = 0
    real(8) :: allcount = 0.d0
    Character(len=99) :: fileName

    ! image info
    rank = this_image()
    size = num_images()

    write(fileName, *) "index ", rank, ".dat"

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

    ! sum(caseJudgeStopIndex - caseJudgeIndex) >= 0
    n = 0
    do while (n < 10000000)
        ! 得分情况
        do i = 1, judgeNumber
            call int2Array(caseItemIndex, itemNumber, judgeOneNumber, caseJudgeIndex(i))
            ! caseItemIndex = 0
            caseItemIndex = caseItemIndex + 1   ! 评分表是从1开始，计数器是从0开始

            ! if (rank == 1) then
            !     write(*, *) ""
            !     write(*, *) i
            ! end if

            do j = 1, itemNumber
                do k = 1, companyNumber
                    scoreTabOne(i, j, k) = itemScores(j, judgeOne(caseItemIndex(j), k))
                end do

                ! if (rank == 1) then
                !     write(*, *) scoreTabOne(i, j, 1:companyNumber)
                ! end if

            end do
        end do

        ! 得分计算
        socreOne = 0
        do k = 1, companyNumber
            do j = 1, itemNumber
                socreOneTmp = dble(scoreTabOne(1:judgeNumber, j, k))
                socreOne(k) = socreOne(k) + (sum(socreOneTmp) - maxval(socreOneTmp) - minval(socreOneTmp))
            end do
        end do

        ! if (rank == 1) then
        !     write(*, *) ""
        !     write(*, *) socreOne
        ! end if

        ! 是否符合条件
        if (socreOne(1) == socreOne(2) .and. socreOne(2) == socreOne(3)) then
            counts = counts + 1
            write(*, '(*(i))') caseJudgeIndex

            ! open(20, file=fileName, position='APPEND')
            !     write(20, '(*(i))') caseJudgeIndex
            ! close(20)
        end if
        
        call addOne(caseJudgeIndex, judgeNumber, oneCaseNumber)
        n = n + 1
    end do

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
		write(*, '(a, f10.4)') "Time (s): ", dble(time_end-time_start) / 1000000.d0

	end if

	stop
end program main
