!***********************************************************************
!**begin prologue     Atomic_Matrices
!**date written       080612   (yymmdd)
!**revision date               (yymmdd)
!**keywords           time propagation
!**
!**author             schneider, b. i.(nsf)
!**source             
!**purpose            This module packages all of the variables associated with
!***                  the use of atomic symmetry.
!***                  
!***description       
!***                  
!***                  
!***                  
!***                  
!***                  
!***                  
!***                  
!***                  
!***                  
!***                  
!***                  
!***                  
!***                  
!**references
!**modules needed     None directly
!**end prologue       Atomic_Matrices
!***********************************************************************
!***********************************************************************
                           MODULE Atomic_Matrices
                           use accuracy
!
                           IMPLICIT NONE
!
  INTEGER                                   :: L
  INTEGER                                   :: L_Prime
  INTEGER                                   :: L_Max
  INTEGER                                   :: locate_L
  INTEGER                                   :: locate_L_plus_one
  INTEGER                                   :: locate_L_minus_one
  INTEGER                                   :: number_of_channels
  INTEGER                                   :: state_tri_size
  INTEGER                                   :: channel_size
  INTEGER                                   :: final_total_matrix_size
  INTEGER                                   :: number_of_splines
  INTEGER                                   :: number_of_correlation_terms
  INTEGER                                   :: spline_order
  INTEGER                                   :: number_of_splines_i
  INTEGER                                   :: number_of_splines_j
  INTEGER                                   :: first_spline
  INTEGER                                   :: last_spline
  INTEGER, DIMENSION(:), ALLOCATABLE        :: spline_array_i
  INTEGER, DIMENSION(:), ALLOCATABLE        :: spline_array_j
  INTEGER                                   :: ic
  INTEGER                                   :: jc
  INTEGER                                   :: is
  INTEGER                                   :: js
  INTEGER                                   :: ii
  INTEGER                                   :: jj
  INTEGER                                   :: new_ind
  INTEGER                                   :: old_ind
  INTEGER                                   :: old_ij
  INTEGER                                   :: new_ij
  INTEGER                                   :: first
  INTEGER                                   :: last
  INTEGER                                   :: first_matrix_index
  INTEGER                                   :: last_matrix_index
  INTEGER                                   :: first_new_matrix_index
  INTEGER                                   :: last_new_matrix_index
  INTEGER                                   :: tri_size
  INTEGER                                   :: new_size
  INTEGER                                   :: n_dim
  INTEGER                                   :: new_tri_size
  INTEGER                                   :: count
  INTEGER                                   :: old_matrix_count_i
  INTEGER                                   :: old_matrix_count_j
  INTEGER                                   :: new_matrix_count_i
  INTEGER                                   :: new_matrix_count_j
  INTEGER                                   :: non_zero_overlap_elements
  INTEGER                                   :: non_zero_cholesky_elements
  INTEGER                                   :: non_zero_hamiltonian_elements
  INTEGER                                   :: lenbuf
  INTEGER                                   :: remove
  LOGICAL                                   :: channel_format
  LOGICAL, DIMENSION(2)                     :: spline_removal
  LOGICAL                                   :: remove_last_spline
  LOGICAL                                   :: remove_next_to_last_spline
  LOGICAL                                   :: channel_labels_only
  LOGICAL                                   :: dipole_matrices
  LOGICAL                                   :: write_pointers
  LOGICAL                                   :: write_channel_labels
  LOGICAL                                   :: ifeig=.false.
  LOGICAL                                   :: diagonalize_only
  LOGICAL                                   :: diagonalize_matrix
  LOGICAL                                   :: read_only
  LOGICAL                                   :: reformat_input_matrix_only
  LOGICAL                                   :: test_s_inverse
  LOGICAL                                   :: cholesky_from_disk
  LOGICAL                                   :: non_orth
  LOGICAL                                   :: packed_matrices
  LOGICAL                                   :: in_core
  LOGICAL                                   :: full_cholesky_to_disk
  LOGICAL                                   :: overlap_to_disk
  LOGICAL                                   :: time_dependent_potential
  LOGICAL                                   :: to_standard_eigenvalue_problem
  CHARACTER (LEN=1600)                      :: card
  CHARACTER (LEN=80)                        :: cpass
  CHARACTER (LEN=80)                        :: title
  CHARACTER (LEN=80)                        :: ham_file
  CHARACTER (LEN=80)                        :: file_format
  CHARACTER (LEN=80)                        :: input_matrices
  CHARACTER (LEN=80)                        :: output_matrices
  CHARACTER (LEN=128)                       :: cholesky_file_name
  CHARACTER (LEN=128)                       :: packed_file_name
  CHARACTER (LEN=128)                       :: state_file_name
  CHARACTER (LEN=80)                        :: reformatting_control
  CHARACTER (LEN=80)                        :: iteration_method
  CHARACTER (LEN=8)                         :: input_output_vector
  CHARACTER (LEN=80)                        :: matrix_type
  CHARACTER (LEN=80)                        :: matrix_file
  CHARACTER (LEN=80)                        :: matrix_directive
  CHARACTER (LEN=3)                         :: L_1
  CHARACTER (LEN=3)                         :: L_2
  CHARACTER (LEN=4)                         :: parity
  CHARACTER (LEN=8)                         :: use_atomic_symmetry
  CHARACTER (LEN=8)                         :: file_key
  CHARACTER (LEN=16)                        :: spatial_representation = 'spline'
  CHARACTER (LEN=16)                        :: species
!
  TYPE state_labels
       INTEGER,             DIMENSION(:),    ALLOCATABLE            :: state_quantum_numbers
  END TYPE state_labels
!
  TYPE(state_labels),       DIMENSION(:),    ALLOCATABLE            :: labels
!
  TYPE state_matrix
     REAL(idp),             DIMENSION(:),    ALLOCATABLE            :: state_h_matrix
     REAL(idp),             DIMENSION(:),    ALLOCATABLE            :: state_s_matrix
     REAL(idp),             DIMENSION(:),    ALLOCATABLE            :: state_eigen_values
     REAL(idp),             DIMENSION(:,:),  ALLOCATABLE            :: state_eigen_vectors
     INTEGER,               DIMENSION(:),    ALLOCATABLE            :: h_non_zero_columns
     INTEGER,               DIMENSION(:),    ALLOCATABLE            :: h_row_index
     REAL(idp),             DIMENSION(:),    ALLOCATABLE            :: h_packed_columns
     REAL(idp),             DIMENSION(:),    ALLOCATABLE            :: h_diagonal
     INTEGER                                                        :: h_number
     INTEGER,               DIMENSION(:),    ALLOCATABLE            :: s_non_zero_columns
     INTEGER,               DIMENSION(:),    ALLOCATABLE            :: s_row_index
     REAL(idp),             DIMENSION(:),    ALLOCATABLE            :: s_packed_columns
     REAL(idp),             DIMENSION(:),    ALLOCATABLE            :: s_diagonal
     INTEGER                                                        :: s_number
     INTEGER,               DIMENSION(:),    ALLOCATABLE            :: eig_non_zero_columns
     INTEGER,               DIMENSION(:),    ALLOCATABLE            :: eig_row_index
     REAL(idp),             DIMENSION(:),    ALLOCATABLE            :: eig_packed_columns
     REAL(idp),             DIMENSION(:),    ALLOCATABLE            :: eig_diagonal
     INTEGER                                                        :: eig_number
     INTEGER,               DIMENSION(:),    ALLOCATABLE            :: matrix_pointer
  END TYPE state_matrix
!
  TYPE (state_matrix),      DIMENSION(:),    ALLOCATABLE            :: state_mat
!
  TYPE dipole_matrix
     REAL(idp),             DIMENSION(:,:),  ALLOCATABLE            :: dipole_coupling_matrix
     INTEGER,               DIMENSION(:),    ALLOCATABLE            :: dipole_non_zero_columns
     INTEGER,               DIMENSION(:),    ALLOCATABLE            :: dipole_row_index
     REAL(idp),             DIMENSION(:),    ALLOCATABLE            :: dipole_packed_columns
     INTEGER                                                        :: dipole_number
  END TYPE dipole_matrix
!
  TYPE (dipole_matrix),     DIMENSION(:),    ALLOCATABLE            :: dipole_mat
!
     INTEGER,               DIMENSION(:),    ALLOCATABLE            :: matrix_size
!
     INTEGER,               DIMENSION(:),    ALLOCATABLE            :: state_matrix_size
     INTEGER,               DIMENSION(:),    ALLOCATABLE            :: final_state_matrix_size
!
  TYPE channel_labels
       INTEGER,             DIMENSION(:),    ALLOCATABLE            :: channel_quantum_numbers
  END TYPE channel_labels
!
  TYPE(channel_labels),     DIMENSION(:),    ALLOCATABLE            :: channel
!
  TYPE REAL_CHANNEL_MATRICES
       REAL(idp),           DIMENSION(:,:),  ALLOCATABLE            :: channel_h_matrix
       REAL(idp),           DIMENSION(:,:),  ALLOCATABLE            :: channel_s_matrix
  END TYPE REAL_CHANNEL_MATRICES
!
  TYPE COMPLEX_CHANNEL_MATRICES
       COMPLEX(idp),        DIMENSION(:,:),  ALLOCATABLE            :: channel_h_matrix
       COMPLEX(idp),        DIMENSION(:,:),  ALLOCATABLE            :: channel_s_matrix
  END TYPE COMPLEX_CHANNEL_MATRICES
!
  TYPE channel_matrices
     TYPE(REAL_CHANNEL_MATRICES)                                    :: mat_d
     TYPE(COMPLEX_CHANNEL_MATRICES)                                 :: mat_z
  END TYPE channel_matrices
!
  TYPE (CHANNEL_MATRICES),  DIMENSION(:,:),  ALLOCATABLE            :: channel_mat
! 
  INTEGER,                  DIMENSION(:,:),  ALLOCATABLE            :: channel_buf
!
  REAL(idp),                DIMENSION(:),    ALLOCATABLE            :: atomic_eigen_values
!
!
!***********************************************************************
!***********************************************************************
  END  MODULE Atomic_Matrices
!***********************************************************************
!***********************************************************************
