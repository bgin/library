!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  MODULE Iterative_Global
!deck Iterative_Global.f
!**begin prologue     Iterative_Global
!**date written       010829   (yymmdd)
!**revision date      yymmdd   (yymmdd)
!**keywords
!**author             schneider, barry (nsf)
!**source
!**purpose            Global arrays for time propagator.
!**references
!**routines called
!**end prologue       Iterative_Global
  USE accuracy
  IMPLICIT NONE
!
  INTEGER,         DIMENSION(:,:),               &
                   ALLOCATABLE                      :: ibuf
  INTEGER,         DIMENSION(:),                 &
                   ALLOCATABLE                      :: ipvt
  REAL(idp),       DIMENSION(:),                 &
                   ALLOCATABLE                      :: eig
  REAL(idp),       DIMENSION(:),                 &
                   ALLOCATABLE                      :: a
  REAL(idp),       DIMENSION(:),                 &
                   ALLOCATABLE                      :: b
  REAL(idp),       DIMENSION(:),                 &
                   ALLOCATABLE                      :: sub_diagonal
  REAL(idp),       DIMENSION(:,:),               &
                   ALLOCATABLE                      :: eigen_vectors 
  REAL(idp),       DIMENSION(:),                 &
                   ALLOCATABLE                      :: eigen_values
  REAL(idp),       DIMENSION(:),                 &
                   ALLOCATABLE                      :: working_eigen_values
  REAL(idp),       DIMENSION(:),                 &
                   ALLOCATABLE                      :: eig_previous
  REAL(idp),       DIMENSION(:,:),               &
                   ALLOCATABLE                      :: eigen_vectors_previous 
  REAL(isp),       DIMENSION(:),                 &
                   ALLOCATABLE                      :: total_time
  REAL(isp)                                      & 
                                                    :: local_time
!
  TYPE REAL_PROP_VAR
!
  REAL(idp),       DIMENSION(:,:),                &
                   ALLOCATABLE                      :: hamiltonian 
  REAL(idp),       DIMENSION(:,:),                &
                   ALLOCATABLE                      :: matrix 
  REAL(idp),       DIMENSION(:,:),                &
                   ALLOCATABLE                      :: overlap 
  REAL(idp),       DIMENSION(:),                  &
                   ALLOCATABLE                      :: triangle_hamiltonian 
  REAL(idp),       DIMENSION(:),                  &
                   ALLOCATABLE                      :: triangle_overlap 
  REAL(idp),       DIMENSION(:),                  &
                   ALLOCATABLE                      :: triangle_matrix 
  REAL(idp),       DIMENSION(:),                  &
                   ALLOCATABLE                      :: upper 
  REAL(idp),       DIMENSION(:),                  &
                   ALLOCATABLE                      :: lower 
  REAL(idp),       DIMENSION(:,:),                &
                   ALLOCATABLE                      :: eigenvectors 
  REAL(idp),       DIMENSION(:),                  &
                   ALLOCATABLE                     :: v_in
  REAL(idp),       DIMENSION(:),                  &
                   ALLOCATABLE                     :: v_out
  REAL(idp),       DIMENSION(:,:),                &
                   ALLOCATABLE                     :: vec
  REAL(idp),       DIMENSION(:),                  &
                   ALLOCATABLE                     :: h_vec
  REAL(idp),       DIMENSION(:,:),                &
                   ALLOCATABLE                     :: s_vec
  REAL(idp),       DIMENSION(:,:),                &
                   ALLOCATABLE                     :: h_vectors
  REAL(idp),       DIMENSION(:,:),                &
                   ALLOCATABLE                     :: s_mat
  REAL(idp),       DIMENSION(:),                  &
                   ALLOCATABLE                     :: s_mat_tri
  REAL(idp),       DIMENSION(:,:),                &
                   ALLOCATABLE                     :: s_mat_work
  REAL(idp),       DIMENSION(:),                  &
                   ALLOCATABLE                     :: s_mat_work_tri
  REAL(idp),       DIMENSION(:,:),                &
                   ALLOCATABLE                     :: h_mat
  REAL(idp),       DIMENSION(:),                  &
                   ALLOCATABLE                     :: h_mat_tri
  REAL(idp),       DIMENSION(:,:),                &
                   ALLOCATABLE                     :: h_mat_work 
  REAL(idp),       DIMENSION(:),                  &
                   ALLOCATABLE                     :: h_mat_work_tri 
  REAL(idp),       DIMENSION(:),                  &
                   ALLOCATABLE                     :: h_buf
  REAL(idp),       DIMENSION(:),                  &
                   ALLOCATABLE                     :: v_tri
  REAL(idp),       DIMENSION(:),                  &
                   ALLOCATABLE                     :: lanczos_tri
  REAL(idp),       DIMENSION(:),                  &
                   ALLOCATABLE                     :: diag
  REAL(idp),       DIMENSION(:),                  &
                   ALLOCATABLE                     :: rhs
  REAL(idp),       DIMENSION(:),                  &
                   ALLOCATABLE                     :: small_rhs
  REAL(idp),       DIMENSION(:),                  &
                   ALLOCATABLE                     :: small_rhs_work
  REAL(idp),       DIMENSION(:,:),                &
                   ALLOCATABLE                     :: residual
  REAL(idp),       DIMENSION(:),                  &
                   ALLOCATABLE                     :: rhs_tri
  REAL(idp),       DIMENSION(:),                  &
                   ALLOCATABLE                     :: soln_tri
  REAL(idp),       DIMENSION(:),                  &
                   ALLOCATABLE                     :: work
  REAL(idp),       DIMENSION(:),                  &
                   ALLOCATABLE                     :: rwork 
  REAL(idp),       DIMENSION(:),                  &
                   ALLOCATABLE                     :: local_scratch
  REAL(idp),       DIMENSION(:),                  &
                   ALLOCATABLE                     :: psi
  REAL(idp),       DIMENSION(:),                  &
                   ALLOCATABLE                     :: v_scr
  END TYPE REAL_PROP_VAR
!
  TYPE COMPLEX_PROP_VAR
!
  COMPLEX(idp),    DIMENSION(:,:),                &
                   ALLOCATABLE                     :: hamiltonian 
  COMPLEX(idp),    DIMENSION(:,:),                &
                   ALLOCATABLE                     :: matrix 
  COMPLEX(idp),    DIMENSION(:,:),                &
                   ALLOCATABLE                     :: overlap 
  COMPLEX(idp),    DIMENSION(:)  ,                &
                   ALLOCATABLE                     :: triangle_hamiltonian 
  COMPLEX(idp),    DIMENSION(:)  ,                &
                   ALLOCATABLE                     :: triangle_matrix 
  COMPLEX(idp),    DIMENSION(:),                  &
                   ALLOCATABLE                     :: triangle_overlap 
  COMPLEX(idp),    DIMENSION(:),                  &
                   ALLOCATABLE                     :: upper 
  COMPLEX(idp),    DIMENSION(:),                  &
                   ALLOCATABLE                     :: lower 
  COMPLEX(idp),    DIMENSION(:,:),                &
                   ALLOCATABLE                     :: eigenvectors
  COMPLEX(idp),    DIMENSION(:),                  &
                   ALLOCATABLE                     :: v_in
  COMPLEX(idp),    DIMENSION(:),                  &
                   ALLOCATABLE                     :: v_out
  COMPLEX(idp),    DIMENSION(:,:),                &
                   ALLOCATABLE                     :: vec
  COMPLEX(idp),    DIMENSION(:),                  &
                   ALLOCATABLE                     :: h_vec
  COMPLEX(idp),    DIMENSION(:,:),                &
                   ALLOCATABLE                     :: s_vec
  COMPLEX(idp),    DIMENSION(:,:),                &
                   ALLOCATABLE                     :: h_vectors
  COMPLEX(idp),    DIMENSION(:,:),                &
                   ALLOCATABLE                     :: s_mat
  COMPLEX(idp),    DIMENSION(:),                  &
                   ALLOCATABLE                     :: s_mat_tri
  COMPLEX(idp),    DIMENSION(:,:),                &
                   ALLOCATABLE                     :: s_mat_work
  COMPLEX(idp),    DIMENSION(:)  ,                &
                   ALLOCATABLE                     :: s_mat_work_tri
  COMPLEX(idp),    DIMENSION(:,:),                &
                   ALLOCATABLE                     :: h_mat
  COMPLEX(idp),    DIMENSION(:),                  &
                   ALLOCATABLE                     :: h_mat_tri
  COMPLEX(idp),    DIMENSION(:,:),                &
                   ALLOCATABLE                     :: h_mat_work
  COMPLEX(idp),    DIMENSION(:),                  &
                   ALLOCATABLE                     :: h_mat_work_tri
  COMPLEX(idp),    DIMENSION(:),                  &
                   ALLOCATABLE                     :: h_buf
  COMPLEX(idp),    DIMENSION(:),                  &
                   ALLOCATABLE                     :: v_tri
  COMPLEX(idp),    DIMENSION(:),                  &
                   ALLOCATABLE                     :: lanczos_tri
  COMPLEX(idp),    DIMENSION(:),                  &
                   ALLOCATABLE                     :: diag
  COMPLEX(idp),    DIMENSION(:),                  &
                   ALLOCATABLE                     :: rhs
  COMPLEX(idp),    DIMENSION(:),                  &
                   ALLOCATABLE                     :: small_rhs
  COMPLEX(idp),    DIMENSION(:),                  &
                   ALLOCATABLE                     :: small_rhs_work
  COMPLEX(idp),    DIMENSION(:,:),                &
                   ALLOCATABLE                     :: residual
  COMPLEX(idp),    DIMENSION(:),                  &
                   ALLOCATABLE                     :: rhs_tri
  COMPLEX(idp),    DIMENSION(:),                  &
                   ALLOCATABLE                     :: soln_tri
  COMPLEX(idp),    DIMENSION(:),                  &
                   ALLOCATABLE                     :: work
  COMPLEX(idp),    DIMENSION(:),                  &
                   ALLOCATABLE                     :: local_scratch
  COMPLEX(idp),    DIMENSION(:),                  &
                   ALLOCATABLE                     :: psi
  COMPLEX(idp),    DIMENSION(:),                  &
                   ALLOCATABLE                     :: v_scr
  END TYPE COMPLEX_PROP_VAR
!
  TYPE PROP_VAR
    TYPE(REAL_PROP_VAR)                             :: prop_mat_d
    TYPE(COMPLEX_PROP_VAR)                          :: prop_mat_z
  END TYPE PROP_VAR
!
  TYPE(PROP_VAR)                                    :: prop_mat
!                           Allocations for Real Variables
!
  REAL(idp),       DIMENSION(:),                 &
                   ALLOCATABLE                      :: v_in_d
  REAL(idp),       DIMENSION(:),                 &
                   ALLOCATABLE                      :: v_out_d
  REAL(idp),       DIMENSION(:,:),               &
                   ALLOCATABLE                      :: vec_d
  REAL(idp),       DIMENSION(:),                 &
                   ALLOCATABLE                      :: h_vec_d
  REAL(idp),       DIMENSION(:,:),               &
                   ALLOCATABLE                      :: s_vec_d
  REAL(idp),       DIMENSION(:,:),               &
                   ALLOCATABLE                      :: h_vectors_d
  REAL(idp),       DIMENSION(:,:),               &
                   ALLOCATABLE                      :: s_mat_d
  REAL(idp),       DIMENSION(:),                 &
                   ALLOCATABLE                      :: s_mat_tri_d
  REAL(idp),       DIMENSION(:,:),               &
                   ALLOCATABLE                      :: s_mat_work_d
  REAL(idp),       DIMENSION(:),                 &
                   ALLOCATABLE                      :: s_mat_work_tri_d
  REAL(idp),       DIMENSION(:,:),               &
                   ALLOCATABLE                      :: h_mat_d
  REAL(idp),       DIMENSION(:),                 &
                   ALLOCATABLE                      :: h_mat_tri_d
  REAL(idp),       DIMENSION(:,:),               &
                   ALLOCATABLE                      :: h_mat_work_d 
  REAL(idp),       DIMENSION(:),                 &
                   ALLOCATABLE                      :: h_mat_work_tri_d 
  REAL(idp),       DIMENSION(:),                 &
                   ALLOCATABLE                      :: h_buf_d
  REAL(idp),       DIMENSION(:),                 &
                   ALLOCATABLE                      :: v_tri_d
  REAL(idp),       DIMENSION(:),                 &
                   ALLOCATABLE                      :: lanczos_tri_d
  REAL(idp),       DIMENSION(:),                 &
                   ALLOCATABLE                      :: diag_d
  REAL(idp),       DIMENSION(:),                 &
                   ALLOCATABLE                      :: rhs_d
  REAL(idp),       DIMENSION(:),                 &
                   ALLOCATABLE                      :: small_rhs_d
  REAL(idp),       DIMENSION(:),                 &
                   ALLOCATABLE                      :: small_rhs_work_d
  REAL(idp),       DIMENSION(:,:),               &
                   ALLOCATABLE                      :: residual_d
  REAL(idp),       DIMENSION(:),                 &
                   ALLOCATABLE                      :: rhs_tri_d
  REAL(idp),       DIMENSION(:),                 &
                   ALLOCATABLE                      :: soln_tri_d
!
!                           Allocations for Complex Variables
  COMPLEX(idp),   DIMENSION(:),                  &
                  ALLOCATABLE                       :: v_in_z
  COMPLEX(idp),   DIMENSION(:),                  &
                  ALLOCATABLE                       :: v_out_z
  COMPLEX(idp),   DIMENSION(:,:),                &
                  ALLOCATABLE                       :: vec_z
  COMPLEX(idp),   DIMENSION(:),                  &
                  ALLOCATABLE                       :: h_vec_z
  COMPLEX(idp),   DIMENSION(:,:),                &
                  ALLOCATABLE                       :: s_vec_z
  COMPLEX(idp),   DIMENSION(:,:),                &
                  ALLOCATABLE                       :: h_vectors_z
  COMPLEX(idp),   DIMENSION(:,:),                &
                  ALLOCATABLE                       :: s_mat_z
  COMPLEX(idp),   DIMENSION(:),                  &
                  ALLOCATABLE                       :: s_mat_tri_z
  COMPLEX(idp),   DIMENSION(:,:),                &
                  ALLOCATABLE                       :: s_mat_work_z
  COMPLEX(idp),   DIMENSION(:)  ,                &
                  ALLOCATABLE                       :: s_mat_work_tri_z
  COMPLEX(idp),   DIMENSION(:,:),                &
                  ALLOCATABLE                       :: h_mat_z
  COMPLEX(idp),   DIMENSION(:),                  &
                  ALLOCATABLE                       :: h_mat_tri_z
  COMPLEX(idp),   DIMENSION(:,:),                &
                  ALLOCATABLE                       :: h_mat_work_z
  COMPLEX(idp),   DIMENSION(:),                  &
                  ALLOCATABLE                       :: h_mat_work_tri_z
  COMPLEX(idp),   DIMENSION(:),                  &
                  ALLOCATABLE                       :: h_buf_z
  COMPLEX(idp),   DIMENSION(:),                  &
                  ALLOCATABLE                       :: v_tri_z
  COMPLEX(idp),   DIMENSION(:),                  &
                  ALLOCATABLE                       :: lanczos_tri_z
  COMPLEX(idp),   DIMENSION(:),                  &
                  ALLOCATABLE                       :: diag_z
  COMPLEX(idp),   DIMENSION(:),                  &
                  ALLOCATABLE                       :: rhs_z
  COMPLEX(idp),   DIMENSION(:),                  &
                  ALLOCATABLE                       :: small_rhs_z
  COMPLEX(idp),   DIMENSION(:),                  &
                  ALLOCATABLE                       :: small_rhs_work_z
  COMPLEX(idp),   DIMENSION(:,:),                &
                  ALLOCATABLE                       :: residual_z
  COMPLEX(idp),   DIMENSION(:),                  &
                  ALLOCATABLE                       :: rhs_tri_z
  COMPLEX(idp),   DIMENSION(:),                  &
                  ALLOCATABLE                       :: soln_tri_z
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!                          Common variables to a number of routines
!
!                           Allocations for Real Variables
!

  REAL(idp),      DIMENSION(:,:),                &
                  ALLOCATABLE                       :: hamiltonian_d 
  REAL(idp),      DIMENSION(:,:),                &
                  ALLOCATABLE                       :: matrix_d 
  REAL(idp),      DIMENSION(:,:),                &
                  ALLOCATABLE                       :: overlap_d 
  REAL(idp),      DIMENSION(:),                  &
                  ALLOCATABLE                       :: triangle_hamiltonian_d 
  REAL(idp),      DIMENSION(:),                  &
                  ALLOCATABLE                       :: triangle_overlap_d 
  REAL(idp),      DIMENSION(:),                  &
                  ALLOCATABLE                       :: triangle_matrix_d 
  REAL(idp),      DIMENSION(:),                  &
                  ALLOCATABLE                       :: upper_d 
  REAL(idp),      DIMENSION(:),                  &
                  ALLOCATABLE                       :: lower_d 
  REAL(idp),      DIMENSION(:,:),                &
                  ALLOCATABLE                       :: eigenvectors_d 
!
!                           Allocations for complex Variables
!
  COMPLEX(idp),   DIMENSION(:,:),                &
                  ALLOCATABLE                       :: hamiltonian_z 
 COMPLEX(idp),    DIMENSION(:,:),                &
                  ALLOCATABLE                       :: matrix_z 
  COMPLEX(idp),   DIMENSION(:,:),                &
                  ALLOCATABLE                       :: overlap_z 
  COMPLEX(idp),   DIMENSION(:)  ,                &
                  ALLOCATABLE                       :: triangle_hamiltonian_z 
  COMPLEX(idp),   DIMENSION(:)  ,                &
                  ALLOCATABLE                       :: triangle_matrix_z 
  COMPLEX(idp),   DIMENSION(:),                  &
                  ALLOCATABLE                       :: triangle_overlap_z 
  COMPLEX(idp),   DIMENSION(:),                  &
                  ALLOCATABLE                       :: upper_z 
  COMPLEX(idp),   DIMENSION(:),                  &
                  ALLOCATABLE                       :: lower_z 
  COMPLEX(idp),   DIMENSION(:,:),                &
                  ALLOCATABLE                       :: eigenvectors_z
! 
!                          Allocations for scratch variables
!

  REAL(idp),       DIMENSION(:),                 &
                   ALLOCATABLE                       :: work_d
  COMPLEX(idp),   DIMENSION(:),                  &
                   ALLOCATABLE                       :: work_z
  REAL(idp),       DIMENSION(:),                 &
                   ALLOCATABLE                       :: rwork 
  REAL(idp),       DIMENSION(:),                 &
                   ALLOCATABLE                       :: local_scratch_d
  COMPLEX(idp),   DIMENSION(:),                  &
                   ALLOCATABLE                       :: local_scratch_z
!
!                          Directives, Convergence and other variables
!
  REAL(idp)                                          :: error
  REAL(idp)                                          :: wfn_tst
  REAL(idp)                                          :: eps=1.d-10
  REAL(idp)                                          :: near_zero = 1.d-08
  INTEGER                                            :: lwork
  LOGICAL                                            :: null_vec
  LOGICAL                                            :: triangle
  LOGICAL                                            :: compute_energy
  CHARACTER(LEN=16)                                  :: orthogonalize
  CHARACTER(LEN=24)                                  :: iterative_method
  CHARACTER(LEN=16)                                  :: convergence
  CHARACTER(LEN=16)                                  :: type
  INTEGER                                            :: last_it
  INTEGER                                            :: it_min
  INTEGER                                            :: non_zero
  INTEGER                                            :: info
  REAL(idp)                                          :: anorm
  REAL(idp)                                          :: norm
!
!                          Keywords for Print Options
!
  CHARACTER (LEN=80), DIMENSION(10)                  :: print_iterative
  LOGICAL,            DIMENSION(10)                  :: log_iterative
  DATA print_iterative  / 'recursion_coefficients','iterative_vectors',                  &
                        'h_on_vector','overlap_matrix', 'small_matrices',                &
                        'eigenvalues', 'eigenvectors','convergence_tests',               &
                        'solution', 'none' /
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END MODULE Iterative_Global




