!***********************************************************************
! 3dGrid
!**begin prologue     3dGrid
!**date written       090119   (yymmdd)
!**revision date               (yymmdd)
!**keywords           FEDVR, grid, polynomials
!**
!**author             schneider, b. i.(nsf)
!**source             Time_Propagation
!**purpose            Calculate the (x,y,z) points for each shell
!***references
!***modules needed    See USE statements below
!***comments          
!***                  
!***                  
!***end prologue      3dGrid
!***********************************************************************
!***********************************************************************
                           MODULE 3dGrid
                           USE Grid_Defined_Types
!***********************************************************************
!***********************************************************************
                              CONTAINS
!***********************************************************************
!***********************************************************************
!deck Cartesion_Grid
!***begin prologue     Cartesion_Grid
!***date written       000702   (yymmdd)
!***revision date               (yymmdd)
!***keywords           dvr
!***
!***author             schneider, b. i.(nsf)
!***source             
!***purpose            
!***                   
!***description        
!***                   
!***                   
!***                   
!***                   

!***references         

!***routines called    iosys, util and mdutil
!***end prologue       Cartesion_Grid
  SUBROUTINE Make_XYZ(q,wt,edge,p,dp,ddp,type_quadrature,fixed_point,n,print)
  IMPLICIT NONE

  REAL(idp), DIMENSION(:)                :: q
  REAL(idp), DIMENSION(:)                :: wt
  REAL(idp), DIMENSION(:)                :: edge
  REAL(idp), OPTIONAL, DIMENSION(:,:)    :: p
  REAL(idp), OPTIONAL, DIMENSION(:,:)    :: dp
  REAL(idp), OPTIONAL, DIMENSION(:,:)    :: ddp
  REAL(idp),  DIMENSION(:), ALLOCATABLE  :: b
  CHARACTER(LEN=*)                       :: type_quadrature
  INTEGER                                :: fixed_point
  INTEGER                                :: n
  REAL(idp), DIMENSION(2)                :: endpts
  REAL(idp), DIMENSION(2)                :: ptfix
  LOGICAL, OPTIONAL                      :: print
  DATA ptfix / -1.d0, 1.d0 /
!
!
! If the weight function is a one of the classical weight functions the
! points and weights are known analytically and after computing them we
! go directly to getting the coordinate functions.
!
  ALLOCATE( b(1:n) )
  endpts(:)=edge(:)
  IF (type_quadrature == "gauss") THEN
      CALL gaussq('one',n,0.d0,0.d0,0,ptfix,b,q,wt)
  ELSE IF ( type_quadrature == "radau") THEN
      IF (fixed_point == 1) THEN
          ptfix(1) = -1.d0
      ELSE IF ( fixed_point == 2) THEN
         ptfix(1) = 1.d0
      END IF
      CALL gaussq('one',n,0.d0,0.d0,1,ptfix,b,q,wt)
  ELSE IF ( type_quadrature == "lobatto" ) THEN
      ptfix(1) = -1.d0
      ptfix(2) = 1.d0
      CALL gaussq('one',n,0.d0,0.d0,2,ptfix,b,q,wt)
  END IF
  CALL cnvtpt(q,wt,edge,n)
  IF(PRESENT(print) == .true.) THEN
     call Print_Matrix(type_real_vector,q,title='Final Nodes from Gauss')
     call Print_Matrix(type_real_vector,wt,title='Final Weights from Gauss')
  END IF
  DEALLOCATE(b)
!  
!  
! The DVR library assumes that the polynomials are $\delta$
! functions at the quadrature points.  Convert to this normalization
!
!
END SUBROUTINE Make_XYZ
!***********************************************************************
!***********************************************************************
          END MODULE 3dGrid
!***********************************************************************
!***********************************************************************
