# fortran-learning
A simple Fortran learning path

### 基本约定

文件名统一以 F90结尾，这样能够使用C中的预处理命令

注意，预处理命令前面不能有缩进


## 基本语法

### 程序结构

打印 Hello World 

```
program fortranLearning
    implicit none   ! 显式声明变量
    
    WRITE(*, '(a)') "Hello Fortran!"

    STOP            ! 程序停止
end program fortranLearning

```

### 变量与常量

计算圆周长或者面积

```
program fortranLearning
    implicit none   ! 显式声明变量
    
    real(8) :: radius = 10.0d0                  ! 半径
    real(8), parameter :: pi = 3.1415926d0      ! 常量
    logical(2) :: perimeterOrArea = .False.     ! 计算周长还是面积 False=面积 True=周长

    write(*, '(a, f8.4)') "Radius: ", radius

    if (perimeterOrArea) then
        write(*, '(a, f8.4)') "Area: ", pi * radius * radius
    else
        write(*, '(a, f8.4)') "Perimeter: ", 2.0 * pi * radius
    end if

    STOP
end program fortranLearning

```

整数与实数之间的基本运算

```
program fortranLearning
    implicit none   ! 显式声明变量
    
    integer(4) :: a=80000, b=2, c=3_4           ! _4 表示该常量是4个字节的
    real(8) :: d=1.0d10, e=20.d0, f=30.d0       ! Fortran默认 1.0、10.0e0 是单精度，这里d表示双精度

    write(*, '(a, e)') "a*d: ", a * d
    write(*, '(a, e)') "a*d: ", dble(a) * d

    write(*, '(a, e)') "a/d: ", a / d
    write(*, '(a, e)') "a/d: ", dble(a) / d

    write(*, '(a, e)') "d/a: ", d / a
    write(*, '(a, e)') "d/a: ", d / dble(a)

    write(*, '(a, e)') "a/c: ", a / c           ! Error
    write(*, '(a, e)') "a/c: ", dble(a) / c
    write(*, '(a, e)') "a/c: ", a / dble(c)
    write(*, '(a, e)') "a/c: ", dble(a) / dble(c)

    write(*, '(a, e)') "c/b: ", c / b           ! Error
    write(*, '(a, e)') "c/b: ", dble(c) / dble(b)

    STOP
end program fortranLearning

```

### 数组

创建数组，倒序打印
```
program fortranLearning
    implicit none   ! 显示声明变量
    
    integer(4),parameter :: length = 5
    integer(4) :: i
    integer(4) :: array(length) = (/10,20,30,40,50/)    ! 数组长度必须是 常量

    do i = length, 1, -1                                ! start, stop, step
        write(*, '(i4, i4)') i, array(i)
    end do

    STOP
end program fortranLearning

```

计算一个序列的累积函数

$$ h(i) = \sum_{i=1}^{k}x_i $$

```
program fortranLearning
    implicit none   ! 显示声明变量
    
    integer(4),parameter :: length = 5
    integer(4) :: i
    integer(4) :: x(length)=0, h(length)=0              ! 赋予初值

    do i = 1, length
        x(i) = i
    end do

    do i = 1, length
        h(i) = sum(x(1:i))                              ! 序列切片与求和

        write(*, '(i2, i4)') x(i), h(i)
    end do

    STOP
end program fortranLearning

```


### IO

写入、读取 NameList
```
program fortranLearning
    implicit none   ! 显示声明变量
    
    integer(4),parameter :: length = 5
    integer(4) :: x_length                          ! namelist中必须是变量
    integer(4) :: x(length) = (/1,2,3,4,5/)
    character(100) :: fileName = './temp.dat'
    logical(2) :: alive

    NAMELIST /xList/ x_length, x

    x_length = length

    inquire(file=fileName, exist=alive)             ! 文件是否存在
    if(alive) then
        write(*, '(a20, a)') fileName, " is open!"

        open(20, file=fileName)
        read(20, NML=xList)                         ! 按namelist读取数据
        close(20)

    else
        write(*, '(a20, a)') fileName, " is not exist!"
        open(20, file=fileName)
        write(20, NML=xList)                        ! 按namelist写入数据
        close(20)

    end if

    STOP
end program fortranLearning

```

将二维数组写入文件
```
program fortranLearning
    implicit none   ! 显示声明变量
    
    integer(4),parameter :: length = 5
    integer(4) :: i,j
    integer(4) :: x(2, length) = 0          ! 二维数组，列优先存储

    do i = 1, 2
        do j = 1, length
            x(i, j) = j
        end do
    end do

    write(*, '(10i2)') x

    open(20, file='tem.dat')
        do i = 1, 2
            do j = 1, length
                write(20, '(i4, 1x,\)') x(i, j)     ! 加入 \ 表示不换行
            end do

            write(20,*) ""
        end do

        ! or
        ! do i = 1, 2
        !     write(20, '(5i4)') x(i, :)
        ! end do

    close(20)

    STOP
end program fortranLearning

```

## 流程控制

选择排序
```
program fortranLearning
    implicit none   ! 显示声明变量
    
    integer(4),parameter :: length = 10
    integer(4) :: i, j, temp
    integer(4) :: x(length) = (/2, 3, 5, 4, 3, 10, 5, 2, 8, 11/)

    write(*, '(a, 10i3)') "排序前: ", x

    ! 从小到大
    do i = 1, length-1
        do j = i+1, length
            if (x(i) > x(j)) then       ! 交换
                temp = x(i)
                x(i) = x(j)
                x(j) = temp
            end if
        end do
    end do

    write(*, '(a, 10i3)') "排序后: ", x

    STOP
end program fortranLearning

```

另外，有cycle，exit等控制语句，有select case等选择控制方法

## 函数与模块

自定义函数 与 子函数 定义在模块中，然后在主函数调用，要求能够输出函数被调用的次数

mian.F90
```
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

```

fun.F90
```
module BaseFunction
    implicit none

    character(20) :: moduleName="BaseFunction"          ! 全局变量

    contains

        subroutine getMinSub(par1, par2, out)           ! 子程序
            implicit none 
            integer(4),intent(in) :: par1               ! 说明形参列表
            integer(4),intent(in) :: par2
            integer(4),intent(out) :: out
            integer(4),save :: counts = 0               ! 静态变量

            counts = counts + 1
            write(*, '(a, i4)') 'getMinSub called: ', counts

            if (par1 < par2) then
                out = par1
            else
                out = par2
            end if

            return
        end subroutine getMinSub
    
        
        integer(4) function getMinFunc(par1, par2)      ! 自定义函数
            implicit none
            integer(4),intent(in) :: par1               ! 说明形参列表
            integer(4),intent(in) :: par2
            integer(4),save :: counts = 0

            counts = counts + 1
            write(*, '(a, i4)') 'getMinFunc called: ', counts

            if (par1 < par2) then
                getMinFunc = par1
            else
                getMinFunc = par2
            end if

            return
        end

end module BaseFunction

```

## 面向对象

一个可变长度的数组，使用宏定义确定浮点数的字节数

main.F90
```
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

```

array.F90
```
module Array
    implicit none

! 宏定义，浮点数的精度
#ifndef REALN
#define REALN 8
#endif

    Type, public :: DArray

        real(REALN), public, allocatable :: data(:)
        integer(4), public :: length = 0

    contains

        procedure, public :: create
        procedure, public :: destroy

    end Type DArray


    contains

        subroutine create(me, length)
            class(DArray), intent(inout) :: me
            integer(4),intent(in) :: length

            if (length > 0) then
                if (allocated(me%data)) deallocate(me%data)
                
                me%length = length
                Allocate(me%data(me%length))
            else
                write(*, '(a)') "长度小于0!"
            end if
            
        end subroutine create


        subroutine destroy(me)
            class(DArray), intent(inout) :: me

            if (allocated(me%data)) deallocate(me%data)
            me%length = 0
            
        end subroutine destroy

end module Array
```


## 并行计算



## 其它

### 与C的接口


## 参考

较为完整的教程（面向过程）：
https://www.yiibai.com/fortran/

面向过程：
https://zhuanlan.zhihu.com/p/367443139


浮点数问题：
https://blog.csdn.net/weixin_43940314/article/details/105679440

IO格式：
http://sunmengyao0712.lofter.com/post/1d197b79_64207c0


面向对象的基本结构可以参考：
https://github.com/jacobwilliams/pyplot-fortran/blob/master/src/pyplot_module.F90
