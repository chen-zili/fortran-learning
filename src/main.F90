program main
    use BaseFunction

	implicit none

    ! 穷举法-->排列组合
    ! Coarray

    ! 变量定义
    integer(4) :: caseItemIndex(itemNumber) = 0
    integer(4) :: caseItemStopIndex(itemNumber) = 0
    integer(4) :: scoreTab(oneCaseNumber, judgeNumber, companyNumber) = 0
    integer(4) :: socreOne(companyNumber) = 0

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

    ! 初始化Tab
    n = 1
    do i = 1, judgeNumber+1
        do j = 1, i
            m = 0
            if (judgeNumber+1-i > 0) then
                do k = 1, judgeNumber+1-i
                    scoreTab(2*n-1, k, :) = judgeOne(1, 1, :)
                    scoreTab(2*n, k, :) = judgeOne(1, 2, :)
                end do
                m = judgeNumber+1-i
            end if

            if ((i-1) > 0) then
                if (j == i) then
                    do k = 1, i-1
                        scoreTab(2*n-1, k+m, :) = judgeOne(3, 1, :)
                        scoreTab(2*n, k+m, :) = judgeOne(3, 2, :)
                    end do
                
                else
                    do k = 1, i-j
                        scoreTab(2*n-1, k+m, :) = judgeOne(2, 1, :)
                        scoreTab(2*n, k+m, :) = judgeOne(2, 2, :)
                    end do
                    m = m + i-j

                    if (j > 1) then
                        do k = 1, j-1
                            scoreTab(2*n-1, k+m, :) = judgeOne(3, 1, :)
                            scoreTab(2*n, k+m, :) = judgeOne(3, 2, :)
                        end do
                    end if
                end if
            end if

            n = n + 1
        end do
    end do


    ! ! 确定image范围
    ! if (1 == rank) then
    !     caseItemIndex = 0
    !     call real2Array(caseItemStopIndex, itemNumber, oneCaseNumber, &
    !                     dble(allCaseNumber) / dble(size) * dble(rank))
        
    ! else if (size == rank) then
    !     call real2Array(caseItemIndex, itemNumber, oneCaseNumber, &
    !                     dble(allCaseNumber) / dble(size) * dble(rank-1))
    !     call addOne(caseItemIndex, itemNumber, oneCaseNumber)

    !     caseItemStopIndex = oneCaseNumber - 1

    ! else
    !     call real2Array(caseItemIndex, itemNumber, oneCaseNumber, &
    !                     dble(allCaseNumber) / dble(size) * dble(rank-1))
    !     call addOne(caseItemIndex, itemNumber, oneCaseNumber)

    !     call real2Array(caseItemStopIndex, itemNumber, oneCaseNumber, &
    !                     dble(allCaseNumber) / dble(size) * dble(rank))
    ! end if

    ! ! write(*, '(a, i, *(i))') 'Start', rank, caseItemIndex
    ! ! write(*, '(a, i, *(i))') 'Stop ', rank, caseItemStopIndex

    ! do while (sum(caseItemStopIndex - caseItemIndex) >= 0)
    !     ! 得分情况
    !     do i = 1, judgeNumber
    !         call int2Array(caseItemIndex, itemNumber, judgeOneNumber, caseItemIndex(i))
    !         caseItemIndex = caseItemIndex + 1   ! 评分表是从1开始，计数器是从0开始

    !         do j = 1, itemNumber
    !             do k = 1, companyNumber
    !                 scoreTabOne(i, j, k) = itemScores(j, judgeOne(caseItemIndex(j), k))
    !             end do
    !         end do
    !     end do

    !     ! 得分计算
    !     socreOne = 0
    !     do k = 1, companyNumber
    !         do j = 1, itemNumber
    !             socreOneTmp = scoreTabOne(1:judgeNumber, j, k)
    !             socreOne(k) = socreOne(k) + sum(socreOneTmp) - max(socreOneTmp) - min(socreOneTmp)
    !         end do
    !     end do

    !     ! 是否符合条件
    !     if (socreOne(1) == socreOne(2) .and. socreOne(2) == socreOne(3)) then
    !         counts = counts + 1
    !         write(*, '(*(i))') caseItemIndex

    !         open(20, file='./index.dat', position='APPEND')
    !             read(20, '(*(i))') caseItemIndex
    !         close(20)
    !     end if
        
    !     call addOne(caseItemIndex, judgeNumber, oneCaseNumber)
    ! end do

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
