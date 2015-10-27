!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  MODULE arnoldi_Global_rt
!deck arnoldi_Global.f
! \documentclass{article}
! \usepackage{graphicx}
! \setkeys{Gin}{width=\linewidth}
! \title{Arnoldi_Global: MODULE for Arnoldi Propagation}
! \author{Barry I. Schneider}
! \date{}
! \def \<{\langle}
! \def \>{\rangle}
! \begin{document}
! \maketitle
!**begin prologue     arnoldi_Global_rt
!**date written       010829   (yymmdd)
!**revision date      yymmdd   (yymmdd)
!**keywords
!**author             schneider, barry (nsf)
!**source
!**purpose            Global arrays for time propagator.
!**references
!**routines called
!**end prologue       arnoldi_Global_rt
  USE input_output
  USE prop_Global
  IMPLICIT NONE
  COMPLEX*16,   DIMENSION(:,:), &
                ALLOCATABLE              :: vec, hvec, u
  COMPLEX*16,   DIMENSION(:,:), &
                ALLOCATABLE              :: b, bwrk
  REAL*8,       DIMENSION(:),   &
                ALLOCATABLE              :: rwork 
  COMPLEX*16,   DIMENSION(:),   &
                ALLOCATABLE              :: psi0, chi, soln_0, work, vscr
  REAL*8,       DIMENSION(:),   &
                ALLOCATABLE              :: eig
  INTEGER                                :: iter
  INTEGER                                :: begin, end, size
  INTEGER                                :: mxsize, mx2add
  CHARACTER(LEN=8)                       :: cntrl
  CHARACTER(LEN=16)                      :: drct
  CHARACTER(LEN=8), DIMENSION(3)         :: code
  REAL*8                                 :: eig_old, error, maxerr
  DATA code / 'trial:', 'arn-ene:', 'arn-vec:' /
END MODULE arnoldi_Global_rt




