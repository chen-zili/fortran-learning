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