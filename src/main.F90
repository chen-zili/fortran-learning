program fortranLearning
    use BaseFunction

    implicit none   ! 显示声明变量

    ! integer(4), external :: getMinFunc      ! 如果自定义函数在主函数后面定义，需要有声明

    integer(4) :: a = 10, b = 20, c = 0

    Call getMinSub(a, b, c)                   ! 子程序在修改形参的时候方便一点
    write(*, '(a, i4)') "最小值: ", c

    c = getMinFunc(a, b)                      ! 自定义函数在返回值的时候方便一点          
    write(*, '(a, i4)') "最小值: ", c

    STOP
end program fortranLearning
