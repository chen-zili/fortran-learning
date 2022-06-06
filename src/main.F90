program fortranLearning
    use Array

    implicit none   ! 显示声明变量
    type(DArray) :: d1

    Call d1%create(5)

    d1%data = 1.0d0
    write(*, *) d1%data

    Call d1%destroy()

    STOP
end program fortranLearning
