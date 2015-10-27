!***********************************************************************
!**begin prologue     Global_Time_Propagation_Module
!**date written       080612   (yymmdd)
!**revision date               (yymmdd)
!**keywords           time propagation
!**
!**author             schneider, b. i.(nsf)
!**source             
!**purpose            This module packages all of the variables needed directly
!***                  in the propagation code. This includes arrays as well as scalars.
!***                  It does not include variables which are associated with the
!***                  iterative and packing modules.
!***                  
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
!**end prologue       Time_Propagation_Input_Data_Module
!***********************************************************************
!***********************************************************************
                           MODULE Global_Time_Propagation_Module
!
                           USE accuracy
                           IMPLICIT NONE
!
!
  REAL(idp),         DIMENSION(:),             &
                     ALLOCATABLE               :: s_vec_in_d
  REAL(idp),         DIMENSION(:),             &
                     ALLOCATABLE               :: s_vec_out_d
  COMPLEX(idp),      DIMENSION(:),             &
                     ALLOCATABLE               :: s_vec_in_z
  COMPLEX(idp),      DIMENSION(:),             &
                     ALLOCATABLE               :: s_vec_out_z
!
!
     REAL(idp),      DIMENSION(:,:),           &
                     ALLOCATABLE               :: scratch_matrix
!
     REAL(idp),      DIMENSION(:),             &
                     ALLOCATABLE               :: scratch_tri
!
  REAL(idp)                                    :: smallest
  REAL(idp)                                    :: largest
  REAL(idp)                                    :: drop_overlap
  REAL(idp)                                    :: drop_hamiltonian
  REAL(idp)                                    :: drop_cholesky
  REAL(idp)                                    :: drop_dipole
  REAL(idp)                                    :: drop_vectors
  CHARACTER (LEN=16)                           :: type_pulse
  CHARACTER (LEN=16)                           :: peak_type = 'intensity'
  REAL(idp)                                    :: electric_field
  REAL(idp)                                    :: peak_electric_field=.25d-04
  REAL(idp)                                    :: peak_intensity
  REAL(idp)                                    :: pulse_duration = 30.d0
  REAL(idp)                                    :: ramp_time = 5.d0
  REAL(idp)                                    :: photon_energy
  REAL(idp)                                    :: time_one_optical_cycle
  REAL(idp)                                    :: energy_cutoff = 10.d0
  INTEGER                                      :: number_optical_cycles
  INTEGER                                      :: steps_per_optical_cycle
!***********************************************************************
!***********************************************************************
  END  MODULE Global_Time_Propagation_Module
!***********************************************************************
!***********************************************************************
