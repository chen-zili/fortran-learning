#include "stdio.h"

#include "petscsys.h"
#include "petscmat.h"
#include "petscvec.h"
#include "petscviewer.h"
#include "petscis.h"

// petsc初始化字符串
static char help[] = "Solves 2D poisson.\n\n";

int solve_(int *rank, int *size)
{
    PetscErrorCode      ierr = 0;
    PetscMPIInt         r = 0,s = 0;
    Vec                 b;

    printf("C image : %d of %d\n", *rank, *size);

    // 初始化petsc, 这里不需要给命令行参数
    int argc = 1;
    char arg1[1][10] = {{""}};
    char **args = arg1;
    ierr = PetscInitialize(&argc, &args, (char*)0, help); if (ierr) return ierr;

    ierr = MPI_Comm_size(PETSC_COMM_WORLD, &s); CHKERRQ(ierr);
    ierr = MPI_Comm_rank(PETSC_COMM_WORLD, &r); CHKERRQ(ierr);
    printf("C mpi : %d of %d\n", r, s);

    return 0;
}