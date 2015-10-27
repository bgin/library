!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  MODULE prop_derived_types
!deck prop_derived_types.f
!**begin prologue     prop_derived_types
!**date written       010829   (yymmdd)
!**revision date      yymmdd   (yymmdd)
!**keywords
!**author             schneider, barry (nsf)
!**source
!**purpose            Global arrays for FE time propagator.
!**references
!**routines called
!**end prologue       prop_derived_types
  USE accuracy
  USE input_output
  USE prop_Global
  USE prop_prnt
  IMPLICIT NONE
  CHARACTER(LEN=8)                                      :: key
  CHARACTER (LEN=24)                                    :: diag_mod
  LOGICAL                                               :: absorb, no_cc, no_pot
  INTEGER, DIMENSION(4)                                 :: n_reg_real, n_reg_absorb
  INTEGER                                               :: starting_reg, ending_reg, n_reg
  INTEGER                                               :: maxmem_reg, prop_order
  INTEGER                                               :: n_prop, prop_point
  INTEGER                                               :: maxdim  
  INTEGER                                               :: nr_absorb  
  REAL(idp)                                             :: p_fac, p_loc, tau_loc
  INTEGER                                               :: nvec  
  REAL(idp)                                             :: con
  CHARACTER (LEN=16)                                    :: e_method
!  CHARACTER (LEN=16)                                    :: type_calculation
!  REAL(idp)                                             :: eig_o, eig_tts
!  REAL(idp)                                             :: eig_tst
  REAL(idp)                                             :: e_cur
!
  TYPE REAL_PROP_MAT
     REAL(idp),          DIMENSION(:,:),                                         &
                         ALLOCATABLE                    :: exp_tmp_d
     REAL(idp),          DIMENSION(:),                                           &
                         ALLOCATABLE                    :: etemp
     REAL(idp),          DIMENSION(:),                                           &
                         ALLOCATABLE                    :: psi_r
     REAL(idp),          DIMENSION(:),                                           &
                         ALLOCATABLE                    :: psi_i
     REAL(idp),          DIMENSION(:),                                           &
                         ALLOCATABLE                    :: pt
     REAL(idp),          DIMENSION(:,:),                                         &
                         ALLOCATABLE                    :: eigvec_mat
     REAL(idp),          DIMENSION(:),                                           &
                         ALLOCATABLE                    :: eigval_mat
     REAL(idp),          DIMENSION(:,:,:),                                       &
                         ALLOCATABLE                    :: exp_t_mat
     REAL(idp),          DIMENSION(:,:,:),                                       &
                         ALLOCATABLE                    :: cos_t_mat
     REAL(idp),          DIMENSION(:,:,:),                                       &
                         ALLOCATABLE                    :: sin_t_mat
     REAL(idp),          DIMENSION(:),                                           &
                         ALLOCATABLE                    :: psi 
     REAL(idp),          DIMENSION(:),                                           &
                         ALLOCATABLE                    :: v_scr 
     REAL(idp),          DIMENSION(:,:),                                         &
                         ALLOCATABLE                    :: exp_d
     REAL(idp),          DIMENSION(:),                                           &
                         ALLOCATABLE                    :: psi_1d 
     REAL(idp),          DIMENSION(:,:),                                         &
                         ALLOCATABLE                    :: psi_2d 
     REAL(idp),          DIMENSION(:,:,:),                                       &
                         ALLOCATABLE                    :: psi_3d 
     REAL(idp),          DIMENSION(:),                                           &
                         ALLOCATABLE                    :: v_scr_1d 
     REAL(idp),          DIMENSION(:,:),                                         &
                         ALLOCATABLE                    :: v_scr_2d 
     REAL(idp),          DIMENSION(:,:,:),                                       &
                         ALLOCATABLE                    :: v_scr_3d
     REAL(idp),          DIMENSION(:),                                           &
                         ALLOCATABLE                    :: scr
     REAL(idp),          DIMENSION(:),                                           &
                         ALLOCATABLE                    :: local_scratch
  END TYPE REAL_PROP_MAT
!
  TYPE COMPLEX_PROP_MAT
     COMPLEX(idp),       DIMENSION(:,:),                                         &
                         ALLOCATABLE                    :: ke_mat
     COMPLEX(idp),       DIMENSION(:),                                           &
                         ALLOCATABLE                    :: eigval_mat
     COMPLEX(idp),       DIMENSION(:,:),                                         &
                         ALLOCATABLE                    :: eigvec_mat_z_l
     COMPLEX(idp),       DIMENSION(:,:),                                         &
                         ALLOCATABLE                    :: eigvec_mat_z_r
     COMPLEX(idp),       DIMENSION(:),                                           &
                         ALLOCATABLE                    :: v_add
     COMPLEX(idp),       DIMENSION(:,:,:),                                       &
                         ALLOCATABLE                    :: exp_t_mat
     COMPLEX(idp),       DIMENSION(:),                                           &
                         ALLOCATABLE                    :: psi 
     COMPLEX(idp),       DIMENSION(:),                                           &
                         ALLOCATABLE                    :: v_scr 
     COMPLEX(idp),       DIMENSION(:),                                           &
                         ALLOCATABLE                    :: psi_1d 
     COMPLEX(idp),       DIMENSION(:),                                           &
                         ALLOCATABLE                    :: v_scr_1d 
     COMPLEX(idp),       DIMENSION(:,:),                                         &
                         ALLOCATABLE                    :: psi_2d 
     COMPLEX(idp),       DIMENSION(:,:),                                         &
                         ALLOCATABLE                    :: v_scr_2d 
     COMPLEX(idp),       DIMENSION(:,:,:),                                       &
                         ALLOCATABLE                    :: psi_3d 
     COMPLEX(idp),       DIMENSION(:,:,:),                                       &
                         ALLOCATABLE                    :: v_scr_3d 
     COMPLEX(idp),       DIMENSION(:),                                           &
                         ALLOCATABLE                    :: exp_diag
     COMPLEX(idp),       DIMENSION(:,:),                                         &
                         ALLOCATABLE                    :: exp_z
     COMPLEX(idp),       DIMENSION(:),                                           &
                         ALLOCATABLE                    :: scr
     COMPLEX(idp),       DIMENSION(:),                                           &
                         ALLOCATABLE                    :: local_scratch
  END TYPE COMPLEX_PROP_MAT
!
  TYPE PROP_MAT
    TYPE(REAL_PROP_MAT)                                 :: p_mat_d
    TYPE(COMPLEX_PROP_MAT)                              :: p_mat_z
  END TYPE PROP_MAT
!
  TYPE (PROP_MAT),       DIMENSION(:,:),                                         &
                         ALLOCATABLE                    :: p_mat_reg
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  TYPE c_temp
     REAL(idp),          DIMENSION(:),                                           &
                         ALLOCATABLE                    :: c, phi
  INTEGER,               DIMENSION(:),                                           &
                         ALLOCATABLE                    :: l
  END TYPE c_temp
!
  TYPE(c_temp),          DIMENSION(:),                                           &
                         ALLOCATABLE                    :: c_loc
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  TYPE z_temp
     REAL(idp),          DIMENSION(:),                                           &
                         ALLOCATABLE                    :: z_d
     COMPLEX(idp),       DIMENSION(:),                                           &
                         ALLOCATABLE                    :: z_z
  END TYPE z_temp
  TYPE(z_temp),          DIMENSION(:),                                           &
                         ALLOCATABLE                    :: zloc
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END MODULE prop_derived_types




