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
