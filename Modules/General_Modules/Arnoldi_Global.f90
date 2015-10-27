!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  MODULE arnoldi_Global
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
!**begin prologue     arnoldi_Global
!**date written       010829   (yymmdd)
!**revision date      yymmdd   (yymmdd)
!**keywords
!**author             schneider, barry (nsf)
!**source
!**purpose            Global arrays for time propagator.
!**references
!**routines called
!**end prologue       arnoldi_Global
  USE input_output
  USE prop_Global
  IMPLICIT NONE
  REAL*8,       DIMENSION(:,:),     &
                ALLOCATABLE              :: vec_d, hvec_d, u_d
  REAL*8,       DIMENSION(:,:),     &
                ALLOCATABLE              :: b_d, bwrk_d
  REAL*8,       DIMENSION(:,:),     &
                ALLOCATABLE              :: svec_d
  REAL*8,       DIMENSION(:),       &
                ALLOCATABLE              :: work_d, vscr_d
  COMPLEX*16,   DIMENSION(:,:),     &
                ALLOCATABLE              :: vec_z, hvec_z, u_z
  COMPLEX*16,   DIMENSION(:,:),     &
                ALLOCATABLE              :: b_z, bwrk_z
  COMPLEX*16,   DIMENSION(:,:),     &
                ALLOCATABLE              :: svec_z
  COMPLEX*16,   DIMENSION(:),       &
                ALLOCATABLE              :: work_z, vscr_z
  REAL*8,       DIMENSION(:),       &
                ALLOCATABLE              :: rwork 
  REAL*8,       DIMENSION(:),       &
                ALLOCATABLE              :: eig
  REAL*8,       DIMENSION(:,:),     &
                ALLOCATABLE              :: s, ham, s_mat
  REAL*8,       DIMENSION(:),       &
                ALLOCATABLE              :: a, b
  INTEGER                                :: iter
  INTEGER                                :: begin, end, arn_size
  INTEGER                                :: mxsize, mx2add
  CHARACTER(LEN=8)                       :: cntrl
  CHARACTER(LEN=16)                      :: drct
  CHARACTER(LEN=8), DIMENSION(3)         :: code
  LOGICAL                                :: non_orth
  REAL*8                                 :: error, maxerr
  REAL*8                                 :: norm
  DATA code / 'trial:', 'arn-ene:', 'arn-vec:' /
END MODULE arnoldi_Global




