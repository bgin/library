!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  MODULE dvrprop_Global
!deck dvrprop_Global.f
!**begin prologue     dvrprop_Global
!**date written       010829   (yymmdd)
!**revision date      yymmdd   (yymmdd)
!**keywords
!**author             schneider, barry (nsf)
!**source
!**purpose            Global arrays for FEDVR time propagator.
!**references
!**routines called
!**end prologue       dvrprop_Global
  USE accuracy
  USE input_output
  USE prop_Global
  USE prop_prnt
  IMPLICIT NONE
  CHARACTER(LEN=8)                                   :: key
  CHARACTER (LEN=24)                                 :: diag_mod
  LOGICAL                                            :: absorb, no_cc, no_pot
  INTEGER, DIMENSION(4)                              :: n_reg_real, n_reg_absorb
  INTEGER                                            :: starting_reg, ending_reg, n_reg
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  TYPE dvr_mat
    REAL(idp),          DIMENSION(:,:),                                         &
                        POINTER                      :: ke_mat_d
    REAL(idp),          DIMENSION(:),                                           &
                        POINTER                      :: pt_d
    REAL(idp),          DIMENSION(:,:),                                         &
                        POINTER                      :: eigvec_mat_d
    REAL(idp),          DIMENSION(:),                                           &
                        POINTER                      :: eigval_mat_d
    REAL(idp),          DIMENSION(:,:,:),                                       &
                        POINTER                      :: exp_t_mat_d
    COMPLEX(idp),       DIMENSION(:,:,:),                                       &
                        POINTER                      :: exp_t_mat_z
    REAL(idp),          DIMENSION(:,:,:),                                       &
                        POINTER                      :: cos_t_mat
    REAL(idp),          DIMENSION(:,:,:),                                       &
                        POINTER                      :: sin_t_mat
    COMPLEX(idp),       DIMENSION(:,:),                                         &
                        POINTER                      :: ke_mat_z
    COMPLEX(idp),       DIMENSION(:),                                           &
                        POINTER                      :: eigval_mat_z
    COMPLEX(idp),       DIMENSION(:,:),                                         &
                        POINTER                      :: eigvec_mat_z_l
    COMPLEX(idp),       DIMENSION(:,:),                                         &
                        POINTER                      :: eigvec_mat_z_r
    COMPLEX(idp),       DIMENSION(:),                                           &
                        POINTER                      :: v_add_z
  END TYPE dvr_mat
  TYPE (dvr_mat),       DIMENSION(:,:),                                         &
                        ALLOCATABLE                  :: mat_reg
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  REAL(idp),            DIMENSION(:),                                           &
                        ALLOCATABLE                  :: psi_d, v_scr_d 
  REAL(idp),            DIMENSION(:),                                           &
                        ALLOCATABLE                  :: psi_1d_d, v_scr_1d_d 
  REAL(idp),            DIMENSION(:,:),                                         &
                        ALLOCATABLE                  :: psi_2d_d, v_scr_2d_d 
  REAL(idp),            DIMENSION(:,:,:),                                       &
                        ALLOCATABLE                  :: psi_3d_d, v_scr_3d_d 
  COMPLEX(idp),         DIMENSION(:),                                           &
                        ALLOCATABLE                  :: psi_z, v_scr_z 
  REAL(idp),            DIMENSION(:),                                           &
                        ALLOCATABLE                  :: psi_r
  REAL(idp),            DIMENSION(:),                                           &
                        ALLOCATABLE                  :: psi_i
  COMPLEX(idp),         DIMENSION(:),                                           &
                        ALLOCATABLE                  :: psi_1d_z, v_scr_1d_z 
  COMPLEX(idp),         DIMENSION(:,:),                                         &
                        ALLOCATABLE                  :: psi_2d_z, v_scr_2d_z 
  COMPLEX(idp),         DIMENSION(:,:,:),                                       &
                        ALLOCATABLE                  :: psi_3d_z, v_scr_3d_z 
  REAL(idp),            DIMENSION(:),                                           &
                        ALLOCATABLE                  :: exp_diag_d
  COMPLEX(idp),         DIMENSION(:),                                           &
                        ALLOCATABLE                  :: exp_diag_z
  REAL(idp),            DIMENSION(:,:),                                         &
                        ALLOCATABLE                  :: exp_d
  COMPLEX(idp),         DIMENSION(:,:),                                         &
                        ALLOCATABLE                  :: exp_z
  REAL(idp),            DIMENSION(:),                                           &
                        ALLOCATABLE                  :: scr_d
  COMPLEX(idp),         DIMENSION(:),                                           &
                        ALLOCATABLE                  :: scr_z
  REAL(idp),            DIMENSION(:,:),                                         &
                        ALLOCATABLE                  :: exp_tmp_d
  REAL(idp),            DIMENSION(:),                                           &
                        ALLOCATABLE                  :: etemp
  INTEGER,              DIMENSION(:,:),                                         &
                        ALLOCATABLE                  :: itemp
  REAL(idp)                                          :: t_init
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  TYPE c_temp
    REAL(idp),          DIMENSION(:),                                           &
                        POINTER                      :: c, phi
    INTEGER,            DIMENSION(:),                                           &
                        POINTER                      :: l
  END TYPE c_temp
  TYPE(c_temp),         DIMENSION(:),                                           &
                        ALLOCATABLE                  :: c_loc
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  TYPE z_temp
    REAL(idp),          DIMENSION(:),                                           &
                        POINTER                      :: z_d
    COMPLEX(idp),       DIMENSION(:),                                           &
                        POINTER                      :: z_z
  END TYPE z_temp
  TYPE(z_temp),         DIMENSION(:),                                           &
                        ALLOCATABLE                  :: zloc
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  INTEGER                                            :: maxmem_reg, prop_order
  INTEGER                                            :: n_prop, prop_point
  INTEGER                                            :: maxdim  
  INTEGER                                            :: nr_absorb  
  REAL(idp)                                          :: p_fac, p_loc, tau_loc
  INTEGER                                            :: nvec  
  REAL(idp)                                          :: con
  CHARACTER (LEN=16)                                 :: e_method
  CHARACTER (LEN=16)                                 :: type_calculation
  REAL(idp)                                          :: eig_old, eig_tts
  REAL(idp)                                          :: eig_tst
  REAL(idp)                                          :: e_cur
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END MODULE dvrprop_Global




