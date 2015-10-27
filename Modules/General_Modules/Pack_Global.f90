!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  MODULE Pack_Global
!deck Pack_Global.f
!**begin prologue     Pack_Global
!**date written       010829   (yymmdd)
!**revision da
!**keywords
!**author             schneider, barry (nsf)
!**source
!**purpose            Global arrays for packing routines.
!**references
!**routines called
!**end prologue       Pack_Global
  USE accuracy
  IMPLICIT NONE
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!

  TYPE REAL_MATRICES
     REAL(idp),       DIMENSION(:,:), ALLOCATABLE      :: ham
     REAL(idp),       DIMENSION(:,:), ALLOCATABLE      :: s
     REAL(idp),       DIMENSION(:,:), ALLOCATABLE      :: eigvec
     REAL(idp),       DIMENSION(:,:), ALLOCATABLE      :: channel_h_matrix
     REAL(idp),       DIMENSION(:),   ALLOCATABLE      :: work
     REAL(idp),       DIMENSION(:),   ALLOCATABLE      :: rwork
     REAL(idp),       DIMENSION(:,:), ALLOCATABLE      :: hamiltonian
     REAL(idp),       DIMENSION(:,:), ALLOCATABLE      :: overlap
     REAL(idp),       DIMENSION(:),   ALLOCATABLE      :: triangle_hamiltonian
     REAL(idp),       DIMENSION(:),   ALLOCATABLE      :: packed_matrix
     REAL(idp),       DIMENSION(:),   ALLOCATABLE      :: packed_rows
     REAL(idp),       DIMENSION(:),   ALLOCATABLE      :: packed_columns
     REAL(idp),       DIMENSION(:),   ALLOCATABLE      :: packed_rows_v
     REAL(idp),       DIMENSION(:),   ALLOCATABLE      :: packed_columns_v
     REAL(idp),       DIMENSION(:),   ALLOCATABLE      :: matrix_diagonal
     REAL(idp),       DIMENSION(:),   ALLOCATABLE      :: triangle_overlap
     REAL(idp),       DIMENSION(:),   ALLOCATABLE      :: upper
  END TYPE REAL_MATRICES
!
 TYPE COMPLEX_MATRICES
     COMPLEX(idp),    DIMENSION(:,:), ALLOCATABLE      :: ham
     COMPLEX(idp),    DIMENSION(:,:), ALLOCATABLE      :: s
     COMPLEX(idp),    DIMENSION(:,:), ALLOCATABLE      :: eigvec
     COMPLEX(idp),    DIMENSION(:,:), ALLOCATABLE      :: channel_h_matrix
     COMPLEX(idp),    DIMENSION(:),   ALLOCATABLE      :: work
     COMPLEX(idp),    DIMENSION(:),   ALLOCATABLE      :: rwork
     COMPLEX(idp),    DIMENSION(:,:), ALLOCATABLE      :: hamiltonian
     COMPLEX(idp),    DIMENSION(:,:), ALLOCATABLE      :: overlap
     COMPLEX(idp),    DIMENSION(:),   ALLOCATABLE      :: triangle_hamiltonian
     COMPLEX(idp),    DIMENSION(:),   ALLOCATABLE      :: packed_matrix
     COMPLEX(idp),    DIMENSION(:),   ALLOCATABLE      :: packed_rows
     COMPLEX(idp),    DIMENSION(:),   ALLOCATABLE      :: packed_columns
     COMPLEX(idp),    DIMENSION(:),   ALLOCATABLE      :: packed_rows_v
     COMPLEX(idp),    DIMENSION(:),   ALLOCATABLE      :: packed_columns_v
     COMPLEX(idp),    DIMENSION(:),   ALLOCATABLE      :: matrix_diagonal
     COMPLEX(idp),    DIMENSION(:),   ALLOCATABLE      :: triangle_overlap
     COMPLEX(idp),    DIMENSION(:),   ALLOCATABLE      :: upper
  END TYPE COMPLEX_MATRICES
!
!
  TYPE MATRICES
!                           Allocations for eal Variables
!
     TYPE(REAL_MATRICES)                               :: mat_d
     TYPE(COMPLEX_MATRICES)                            :: mat_z
     REAL(idp),       DIMENSION(:),   ALLOCATABLE      :: eigval
     INTEGER,         DIMENSION(:),   ALLOCATABLE      :: col_index  
     INTEGER,         DIMENSION(:),   ALLOCATABLE      :: row_index  
     INTEGER,         DIMENSION(:),   ALLOCATABLE      :: col_index_v  
     INTEGER,         DIMENSION(:),   ALLOCATABLE      :: row_index_v  
     INTEGER                                           :: number
     INTEGER,         DIMENSION(:),   ALLOCATABLE      :: non_zero_rows
     INTEGER,         DIMENSION(:),   ALLOCATABLE      :: non_zero_columns
     INTEGER,         DIMENSION(:),   ALLOCATABLE      :: non_zero_rows_v
     INTEGER,         DIMENSION(:),   ALLOCATABLE      :: non_zero_columns_v
     INTEGER                                           :: non_zero_overlap_elements
     INTEGER                                           :: non_zero_cholesky_elements
     INTEGER                                           :: non_zero_hamiltonian_elements
     INTEGER                                           :: max_col
     INTEGER                                           :: UNIT_NUMBER
     REAL(idp),       DIMENSION(:),   ALLOCATABLE      :: packed_matrix_d
     REAL(idp),       DIMENSION(:),   ALLOCATABLE      :: packed_rows_d
     REAL(idp),       DIMENSION(:),   ALLOCATABLE      :: packed_columns_d
     REAL(idp),       DIMENSION(:),   ALLOCATABLE      :: packed_rows_v_d
     REAL(idp),       DIMENSION(:),   ALLOCATABLE      :: packed_columns_v_d
     REAL(idp),       DIMENSION(:),   ALLOCATABLE      :: matrix_diagonal_d
     CHARACTER (LEN=16)                                :: UNIT_NAME
     CHARACTER(LEN=80)                                 :: matrix_format
     CHARACTER(LEN=80)                                 :: device
     LOGICAL                                           :: reformat_input_matrix_only
     LOGICAL                                           :: non_orth
     LOGICAL                                           :: overlap_to_disk

!
!                           Allocations for Complex Variables
     COMPLEX(idp), DIMENSION(:),      ALLOCATABLE      :: matrix_diagonal_z
     COMPLEX(idp), DIMENSION(:),      ALLOCATABLE      :: packed_matrix_z
     COMPLEX(idp), DIMENSION(:),      ALLOCATABLE      :: packed_rows_z
     COMPLEX(idp), DIMENSION(:),      ALLOCATABLE      :: packed_columns_z
     COMPLEX(idp), DIMENSION(:),      ALLOCATABLE      :: packed_rows_v_z
     COMPLEX(idp), DIMENSION(:),      ALLOCATABLE      :: packed_columns_v_z
!
  END TYPE MATRICES
                                                                                                                      

  TYPE (MATRICES), DIMENSION(:),     ALLOCATABLE       :: mat_var
  TYPE (MATRICES)                                      :: mat
! 
!
!                          Directives, Convergence and other variables
!
  LOGICAL                                              :: print_packed
  INTEGER                                              :: pointer
  INTEGER                                              :: trips
  INTEGER                                              :: left_over
  LOGICAL                                              :: packed
  REAL(idp)                                            :: drop_tol
  CHARACTER(LEN=24)                                    :: type_packing
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END MODULE Pack_Global




