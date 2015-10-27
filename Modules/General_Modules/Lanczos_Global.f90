  MODULE lanczos_Global
!deck lanczos_Global.f
!**begin prologue     lanczos_Global
!**date written       010829   (yymmdd)
!**revision date      yymmdd   (yymmdd)
!**keywords
!**author             schneider, barry (nsf)
!**source
!**purpose            Global arrays for time propagator.
!**references
!**routines called
!**end prologue       lanczos_Global
  USE input_output
  USE prop_Global
  IMPLICIT NONE
!
!
  REAL(idp),       DIMENSION(:,:),               &
                   ALLOCATABLE                     :: vec_d
  REAL(idp),       DIMENSION(:),                 &          
                   ALLOCATABLE                     :: h_vec_d
  REAL(idp),       DIMENSION(:,:),               &
                   ALLOCATABLE                     :: s_vec_d
  REAL(idp),       DIMENSION(:,:),               &
                   ALLOCATABLE                     :: s_mat_d
  REAL(idp),       DIMENSION(:,:),               &
                   ALLOCATABLE                     :: s_mat_work_d
  REAL(idp),       DIMENSION(:,:),               &
                   ALLOCATABLE                     :: h_small_d
  REAL(idp),       DIMENSION(:),                 &
                   ALLOCATABLE                     :: v_tri_d
  REAL(idp),       DIMENSION(:),                 &
                   ALLOCATABLE                     :: work_d
  REAL(idp),       DIMENSION(:),                 &
                   ALLOCATABLE                     :: vscr_d
  REAL(idp),       DIMENSION(:),                 &
                   ALLOCATABLE                     :: rwork 
  REAL(idp),       DIMENSION(:),                 &
                   ALLOCATABLE                     :: eig
  REAL(idp),       DIMENSION(:),                 &
                   ALLOCATABLE                     :: a, b
  REAL(idp),       DIMENSION(:,:),               &
                   ALLOCATABLE                     :: hamiltonian_d 
  REAL(idp),       DIMENSION(:,:),               &
                   ALLOCATABLE                     :: overlap_d 
  REAL(idp),       DIMENSION(:,:),               &
                   ALLOCATABLE                     :: eigenvectors_d 
  REAL(idp),       DIMENSION(:,:),               &
                   ALLOCATABLE                     :: eigen_vectors 
  REAL(idp),       DIMENSION(:),                 &
                   ALLOCATABLE                     :: eigenvalues
  COMPLEX(idp),    DIMENSION(:,:),               &
                   ALLOCATABLE                     :: vec_z
  COMPLEX(idp),    DIMENSION(:),                 &
                   ALLOCATABLE                     :: h_vec_z
  COMPLEX(idp),    DIMENSION(:,:),               &
                   ALLOCATABLE                     :: s_vec_z
  COMPLEX(idp),    DIMENSION(:,:),               &
                   ALLOCATABLE                     :: s_mat_z
  COMPLEX(idp),    DIMENSION(:,:),               &
                   ALLOCATABLE                     :: s_mat_work_z
  COMPLEX(idp),    DIMENSION(:,:),               &
                   ALLOCATABLE                     :: h_small_z
  COMPLEX(idp),    DIMENSION(:),                 &
                   ALLOCATABLE                     :: v_tri_z
  COMPLEX(idp),    DIMENSION(:),                 &
                   ALLOCATABLE                     :: work_z
  COMPLEX(idp),    DIMENSION(:),                 &
                   ALLOCATABLE                     :: vscr_z
  COMPLEX(idp),    DIMENSION(:,:),               &
                   ALLOCATABLE                     :: hamiltonian_z 
  COMPLEX(idp),    DIMENSION(:,:),               &
                   ALLOCATABLE                     :: overlap_z 
  COMPLEX(idp),    DIMENSION(:,:),               &
                   ALLOCATABLE                     :: eigenvectors_z
! 
  LOGICAL                                       :: non_orth
  REAL(idp)                                     :: error
  REAL(idp)                                     :: wfn_tst
  INTEGER                                       :: lwork
  INTEGER                                       :: maximum_number_of_time_subintervals
  REAL(idp)                                     :: eps=1.d-08
  LOGICAL                                       :: null_vec
  LOGICAL                                       :: schmidt
  CHARACTER(LEN=16)                             :: convergence
  CHARACTER(LEN=16)                             :: lanczos_convergence
  CHARACTER(LEN=16)                             :: type
  INTEGER                                       :: last_it
  INTEGER                                       :: info
  REAL(idp)                                     :: anorm
  REAL(idp)                                     :: norm
  INTEGER                                       :: total_number_of_iterations
  REAL(idp)                                     :: t_start
  REAL(idp)                                     :: t_end
  REAL(idp)                                     :: save_deltat

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END MODULE lanczos_Global
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!




