!
  MODULE pass
  USE accuracy
        INTERFACE testdim
            MODULE PROCEDURE test_d, &
                             test_z
        END INTERFACE
!
        CONTAINS
!        
  Subroutine Passa(A,n,m)
  IMPLICIT NONE
  INTEGER                                   :: n
  INTEGER                                   :: m
  INTEGER                                   :: i
  INTEGER                                   :: j
  REAL(idp), DIMENSION(:,:)                 :: A 
  write(6,*) A(1:n,1:m)
  end subroutine Passa
!
  Subroutine test_d(B,n)
  IMPLICIT NONE
  INTEGER                                  :: n
  REAL(idp),  DIMENSION(:)                 :: B 
  write(6,*) B(1:n)
  return
  end subroutine test_d
!
  Subroutine test_z(B,n)
  IMPLICIT NONE
  INTEGER                                 :: n
  COMPLEX*16, DIMENSION(:)                :: B 
  write(6,*) B(1:n)
  return
  end subroutine test_z
  END MODULE pass
