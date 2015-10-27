!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  MODULE fd_Global
!***begin prologue     fd_Global
!***date written       021231   (yymmdd)
!***revision date               (yymmdd)
!***keywords           finite difference
!***author             schneider, b. i.(nsf)
!***source             dvrlib
!***purpose            Global variables for finite difference routines
!***references

!***routines called    
!***end prologue       fd_Global
  USE input_output
  USE grid_Global
  IMPLICIT NONE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!
  INTEGER                               :: nstep, ndiff
  REAL*8                                :: del, dscale
  REAL*8, DIMENSION(4)                  :: d
  REAL*8, DIMENSION(2)                  :: edge
!
!
END MODULE fd_Global
