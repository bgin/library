!
MODULE dvr_Global
!***begin prologue     dvr_Global
!***date written       021231   (yymmdd)
!***revision date               (yymmdd)
!***keywords           time, finite element dvr, orthogonal polynomial
!***author             schneider, b. i.(nsf)
!***source             dvrlib
!***purpose            Global variables for dvr library
!***description        this routine defines the Global variables
!***                   and data needed for the dvrlib
!
!***references

!***routines called    
!***end prologue       dvr_Global
  USE accuracy
  USE input_output
  USE grid_Global
  IMPLICIT NONE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!
  CHARACTER (LEN=4)                     :: parity
  CHARACTER (LEN=80)                    :: potential_type
  INTEGER, PARAMETER                    :: maxreg=5000
  INTEGER, DIMENSION(maxreg)            :: n
  INTEGER, DIMENSION(maxreg)            :: npt
  INTEGER, DIMENSION(maxreg)            :: tri_reg
  INTEGER, DIMENSION(maxreg)            :: nrq
  INTEGER                               :: spdim
  INTEGER                               :: nfix
  INTEGER                               :: bcl
  INTEGER                               :: bcr
  INTEGER                               :: nreg
  INTEGER                               :: l_orb_max 
  INTEGER                               :: l_max 
  INTEGER                               :: m_max 
  INTEGER                               :: l_val 
  INTEGER                               :: m_val 
  INTEGER                               :: nphy_tri 
  REAL*8,  DIMENSION(maxreg+1)          :: edge
  REAL*8                                :: box
  REAL*8                                :: deltax
  REAL*8                                :: z_a
  REAL*8                                :: z_b
  REAL*8                                :: R_ab
  LOGICAL, DIMENSION(2)                 :: drop
  LOGICAL, DIMENSION(2)                 :: fix
  LOGICAL                               :: skip
  INTEGER, DIMENSION(:,:), ALLOCATABLE  :: nfun_reg
  INTEGER, DIMENSION(:), ALLOCATABLE    :: num_reg
  REAL*4,  DIMENSION(20)                :: del
  REAL*4,  DIMENSION(20)                :: tim
!
!
END MODULE dvr_Global
