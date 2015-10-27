!
MODULE accuracy
! Define floating-point working precision, isp, idp
!
  IMPLICIT NONE
  INTEGER, PARAMETER        :: idp    = SELECTED_REAL_KIND(15, 307)
  INTEGER, PARAMETER        :: isp    = SELECTED_REAL_KIND(6, 37)
  INTEGER, PARAMETER        :: int_sp = SELECTED_INT_KIND(9)
  INTEGER, PARAMETER        :: int_dp = SELECTED_INT_KIND(10)
!
!
END MODULE  accuracy
