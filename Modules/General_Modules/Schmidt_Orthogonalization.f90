!
  module Schmidt
  USE accuracy
  USE input_output
  REAL(idp), PARAMETER                    :: tol = 1.d-12
!***********************************************************************
!                          Explicit Interfaces
!***********************************************************************
!
!
                            INTERFACE Orthonormalize                       
                       MODULE PROCEDURE Orthonormalize_z,                  &
                                        Orthonormalize_d,                  &
                                        Normalize_d,                       &
                                        Normalize_z
                            END INTERFACE Orthonormalize
!********************************************************************************
                                Contains
!********************************************************************************
!********************************************************************************
  SUBROUTINE Orthonormalize_d( v_new, v_old, new, old, dim)
  IMPLICIT NONE
  REAL(idp), DIMENSION(:,:)                           :: v_new
  REAL(idp), DIMENSION(:,:), OPTIONAL                 :: v_old
  REAL(idp)                                           :: sdot
  REAL(idp)                                           :: overlap
  INTEGER                                             :: new
  INTEGER, OPTIONAL                                   :: old
  INTEGER                                             :: dim
  INTEGER                                             :: i
  INTEGER                                             :: j
  INTEGER                                             :: k
!
! Step one is to orthogonalize the new vectors to the old orthonormal set.
!
  IF ( PRESENT(v_old) ) THEN
       DO i=1,new
          DO j = 1, old
             overlap = sdot(dim,v_old(1,j),1,v_new(1,i),1)
             DO k = 1, dim
                v_new(k,i) = v_new(k,i) - v_old(k,j) * overlap
             END DO
          END DO
          overlap = 1.d0 / sqrt ( sdot(dim,v_new(1,i),1,v_new(1,i),1) )
          DO k = 1, dim
             v_new(k,i) = overlap * v_new(k,i)
          END DO
       END DO
  END IF
!
! Now orthonormalize the new vectors to each other
!

  DO i = 1, new
!
!               Normalize
! 
     overlap = 1.d0 / sqrt ( sdot(dim,v_new(1,i),1,v_new(1,i),1) )
     DO k = 1, dim
        v_new(k,i) = overlap * v_new(k,i)
     END DO
!              Done
!
!               Project recursively
!
     DO j = 1, i-1
        overlap = sdot(dim,v_new(1,j),1,v_new(1,i),1)
        DO k = 1, dim
           v_new(k,i) = v_new(k,i) - overlap * v_new(k,j)
        END DO
     END DO
!              Done
!
!               Normalize
!
     overlap = sqrt ( sdot(dim,v_new(1,i),1,v_new(1,i),1) )
     IF (overlap >= tol ) THEN
         overlap = 1.d0 / overlap
         DO k = 1, dim
            v_new(k,i) = overlap * v_new(k,i)
         END DO
     ELSE
         write(iout,*) 'overlap = ',overlap
         Call lnkerr('linearly dependent set')
     END IF
  END DO
!********************************************************************************
  END SUBROUTINE Orthonormalize_d
!********************************************************************************
!********************************************************************************
  SUBROUTINE Orthonormalize_z( v_new, v_old, new, old, dim)
  IMPLICIT NONE
  COMPLEX(idp), DIMENSION(:,:)                        :: v_new
  COMPLEX(idp), DIMENSION(:,:), OPTIONAL              :: v_old
  COMPLEX(idp)                                        :: cdotc
  COMPLEX(idp)                                        :: overlap
  INTEGER                                             :: new
  INTEGER, OPTIONAL                                   :: old
  INTEGER                                             :: dim
  INTEGER                                             :: i
  INTEGER                                             :: j
  INTEGER                                             :: k
!
! Step one is to orthogonalize the new vectors to the old ones
!
  IF ( PRESENT(v_old) ) THEN
     DO i=1,new
        DO j = 1, old
           overlap = cdotc(dim,v_old(1,j),1,v_new(1,i),1)
           DO k = 1, dim
              v_new(k,i) = v_new(k,i) - v_old(k,j) * overlap
           END DO
        END DO
        overlap = 1.d0 / sqrt ( cdotc(dim,v_new(1,i),1,v_new(1,i),1))
        DO k = 1, dim
           v_new(k,i) = overlap * v_new(k,i)
        END DO
     END DO
  END IF
!
! Now orthonormalize the new vectors to each other
!
  DO i = 1, new
!
!               Normalize
! 
     overlap = 1.d0 / sqrt ( cdotc(dim,v_new(1,i),1,v_new(1,i),1) )
     DO k = 1, dim
        v_new(k,i) = overlap * v_new(k,i)
     END DO
!
!               Project
!
     DO j = 1, i-1
        overlap = cdotc(dim,v_new(1,j),1,v_new(1,i),1)
        DO k = 1, dim
           v_new(k,i) = v_new(k,i) - overlap * v_new(k,j)
        END DO
     END DO
!
!               Normalize
!
     overlap = sqrt ( cdotc(dim,v_new(1,i),1,v_new(1,i),1) )
     IF (abs(overlap) >= tol ) THEN
         overlap = 1.d0 / overlap
         DO k = 1, dim
            v_new(k,i) = overlap * v_new(k,i)
         END DO
     ELSE
         write(iout,*) 'overlap = ',overlap
         Call lnkerr('linearly dependent set')
     END IF
  END DO
!********************************************************************************
  END SUBROUTINE Orthonormalize_z
!********************************************************************************
!********************************************************************************
  SUBROUTINE Normalize_d( v, dim)
  IMPLICIT NONE
  REAL(idp), DIMENSION(:)                             :: v
  REAL(idp)                                           :: sdot
  REAL(idp)                                           :: overlap
  INTEGER                                             :: dim
  overlap = 1.d0 / sqrt ( sdot(dim,v,1,v,1) )
  v = overlap * v
!********************************************************************************
  END SUBROUTINE Normalize_d
!********************************************************************************
!********************************************************************************
  SUBROUTINE Normalize_z( v, dim )
  IMPLICIT NONE
  COMPLEX(idp), DIMENSION(:)                          :: v
  COMPLEX(idp)                                        :: cdotc
  COMPLEX(idp)                                        :: overlap
  INTEGER                                             :: dim
  overlap = 1.d0 / sqrt ( cdotc(dim,v,1,v,1) )
  v = overlap * v
!********************************************************************************
  END SUBROUTINE Normalize_z
!********************************************************************************
  END MODULE Schmidt
!********************************************************************************
