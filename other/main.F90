!  testcoarray.f90 
!
!  FUNCTIONS:
!  testcoarray - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: testcoarray
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************
program CAFHelloworld
    implicit none
    Integer(4) TestInt[*]
    Integer(4) TestNum[*]
    Real(8) :: TestReal[*]

    Integer(4) :: ierr
    Integer(4) :: rank = 0
    Integer(4) :: size = 0

    TestInt = this_image()
    TestNum = num_images()
    TestReal = Dble(TestInt)*2.d0

    print *, 'Fortan image ', this_image(), ' of ', num_images()

    ! call mpi_init(ierr)
    ! call mpi_comm_size(MPI_COMM_WORLD,size,ierr)
    ! call mpi_comm_rank(MPI_COMM_WORLD,rank,ierr)
    ! print *, 'mpi ', rank, ' of ', size


    ! call mpi_finalize(ierr)

    Call solve(TestInt, TestNum)

 end program CAFHelloWorld