PROGRAM LA_ZHBGVX_ET_EXAMPLE
!
!  -- LAPACK95 interface driver routine (version 3.0) --
!     UNI-C, Denmark; Univ. of Tennessee, USA; NAG Ltd., UK
!     September, 2000
!
!  .. USE STATEMENTS
   USE LA_PRECISION, ONLY: WP => DP
   USE F95_LAPACK, ONLY: LA_HBGVX
!  .. IMPLICIT STATEMENT ..
   IMPLICIT NONE
!  .. PARAMETERS ..
   CHARACTER(LEN=*), PARAMETER :: FMT = '(4(1X,1H(,F7.3,1H,,F7.3,1H):))'
   INTEGER, PARAMETER :: NIN=5, NOUT=6
!  .. LOCAL SCALARS ..
   CHARACTER(LEN=1) :: UPLO
   INTEGER :: I, J, INFO, N, KA, KB, M, IL, IU
   REAL(WP) ::  VL, VU 
!  .. LOCAL ARRAYS ..
   INTEGER, ALLOCATABLE :: IFAIL(:) 
   REAL(WP), ALLOCATABLE :: AA(:,:), BB(:,:), W(:)
   COMPLEX(WP), ALLOCATABLE :: A(:,:), B(:,:), Z(:,:), DUMMY(:,:), Q(:,:)
!  .. EXECUTABLE STATEMENTS ..
   WRITE (NOUT,*) 'ZHBGVX ET_Example Program Results.'
   READ ( NIN, * )   ! SKIP HEADING IN DATA FILE
   READ ( NIN, * ) N, KA, KB
   PRINT *, 'N = ', N, ' KA = ', KA, ' KB = ', KB
   ALLOCATE ( A(KA+1,N), AA(KA+1,N), B(KB+1,N), BB(KB+1,N), W(N), Z(N,N) )
   ALLOCATE (IFAIL(N), Q(N,N))
!
   VL = -10; VU=10; IL=1; IU=N; UPLO = 'U'
   AA = HUGE(1.0_WP); BB = HUGE(1.0_WP)
   DO I = 1, KA+1; READ (NIN, *) (AA(I, J), J = KA-I+2, N); ENDDO
   DO I = 1, KB+1; READ (NIN, *) (BB(I, J), J = KB-I+2, N); ENDDO
   A=AA; B=BB
   WRITE(NOUT,*) 'The matrix A:'
   DO I = 1, KA+1; WRITE (NOUT,*) 'I = ', I; WRITE (NOUT,FMT) AA(I,:); ENDDO
   WRITE(NOUT,*) 'The matrix B:'
   DO I = 1, KB+1; WRITE (NOUT,*) 'I = ', I; WRITE (NOUT,FMT) BB(I,:); ENDDO
!
   WRITE ( NOUT, * )'---------------------------------------------------------'
   WRITE ( NOUT, * )
   WRITE ( NOUT, * )'Details of LA_ZHBGVX LAPACK Subroutine Results.'
   WRITE ( NOUT, * )
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_HBGVX( A, B, W, ''U'', Z, INFO )'
   A=AA; B=BB
   CALL LA_HBGVX( A, B, W, UPLO, Z, INFO = INFO )
   WRITE(NOUT,*) 'INFO = ', INFO, ' EIGENVALUES:'
   WRITE(NOUT,FMT) W
   WRITE(NOUT,*) 'EIGENVECTORS:'
   DO I = 1, N; WRITE(NOUT,*) 'I = ', I; WRITE (NOUT,FMT) Z(:,I); END DO
!
     UPLO = 'U'
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_HBGVX( A, B, W, ''U'', Z )'
   A=AA; B=BB
   CALL LA_HBGVX( A, B, W, UPLO, Z )
   WRITE(NOUT,*) ' EIGENVALUES:'
   WRITE(NOUT,FMT) W
   WRITE(NOUT,*) 'EIGENVECTORS:'
   DO I = 1, N; WRITE(NOUT,*) 'I = ', I; WRITE (NOUT,FMT) Z(:,I); END DO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_HBGVX( A, B, W, Z=Z )'
   A=AA; B=BB
   CALL LA_HBGVX( A, B, W, Z=Z )
   WRITE(NOUT,*) ' EIGENVALUES:'
   WRITE(NOUT,FMT) W
   WRITE(NOUT,*) 'EIGENVECTORS:'
   DO I = 1, N; WRITE(NOUT,*) 'I = ', I; WRITE (NOUT,FMT) Z(:,I); END DO
!
     UPLO = 'L'
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_HBGVX( A, B, W, UPLO=''L'', Z=Z, INFO=INFO )'
   A=AA; B=BB; W = HUGE(1.0_WP); Z = HUGE(1.0_WP)
   CALL LA_HBGVX( A, B, W, UPLO, Z=Z, INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO, ' EIGENVALUES:'
   WRITE(NOUT,FMT) W
   WRITE(NOUT,*) 'EIGENVECTORS:'
   DO I = 1, N; WRITE(NOUT,*) 'I = ', I; WRITE (NOUT,FMT) Z(:,I); END DO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_HBGVX( A, B, W )'
   A=AA; B=BB; Z = HUGE(1.0_WP)
   CALL LA_HBGVX( A, B, W )
   WRITE(NOUT,*) ' EIGENVALUES:'
   WRITE(NOUT,FMT) W
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_HBGVX( A, B, W, UPLO, Z, VL, VU, M=M, IFAIL=IFAIL, Q=Q, INFO=INFO )'
   A=AA; B=BB; W = HUGE(1.0_WP); Z = HUGE(1.0_WP)
   CALL LA_HBGVX( A, B, W, UPLO, Z, VL, VU, M=M, IFAIL=IFAIL, Q=Q, INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO, ' EIGENVALUES:'
   WRITE(NOUT,FMT) W
   WRITE(NOUT,*) 'EIGENVECTORS:'
   DO I = 1, N; WRITE(NOUT,*) 'I = ', I; WRITE (NOUT,FMT) Z(:,I); END DO
   
! STARTING THE ERROR TESTS: 
! ERROR 3
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_HBGVX( A, B, W(1:N-1), INFO=INFO )'
   A=AA; B=BB; Z = HUGE(1.0_WP)
   CALL LA_HBGVX( A, B, W(1:N-1), INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
! ERROR 4
   UPLO = '9'
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_HBGVX( A, B, W, UPLO=''9'', INFO=INFO )'
   A=AA; B=BB; Z = HUGE(1.0_WP)
   CALL LA_HBGVX( A, B, W, UPLO, INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
! ERROR 5
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_HBGVX( A, B, W, UPLO, Z(:, 1:N-1), INFO=INFO )'
   A=AA; B=BB; Z = HUGE(1.0_WP)
   CALL LA_HBGVX( A, B, W, UPLO, Z(:, 1:N-1), INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
! ERROR 5
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_HBGVX( A, B, W, UPLO, Z(1:N-1,:), INFO=INFO )'
   A=AA; B=BB; Z = HUGE(1.0_WP)
   CALL LA_HBGVX( A, B, W, UPLO, Z(1:N-1,:), INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
! ERROR 6
   VL = 10; VU = -10
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_HBGVX( A, B, W, UPLO, Z, VL, VU, M, IFAIL, Q, INFO=INFO )'
   A=AA; B=BB; Z = HUGE(1.0_WP)
   CALL LA_HBGVX( A, B, W, UPLO, Z,  VL, VU, M=M, IFAIL=IFAIL, Q=Q, INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
! ERROR 7
   IL = 1; IU = N; VL=-10; VU=10
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_HBGVX( A, B, W, UPLO, Z, VL, VU, IL, IU, M, IFAIL, Q, INFO=INFO )' 
   A=AA; B=BB; Z = HUGE(1.0_WP)
   CALL LA_HBGVX( A, B, W, UPLO, Z, VL, VU, IL, IU, M, IFAIL, Q, INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
   IU=N; IL=1
! ERROR 8
   IL = -1; IU = N+1
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_HBGVX( A, B, W, UPLO, Z, IL=IL, IU=IU, M=M, IFAIL=IFAIL, Q=Q, INFO=INFO )' 
   A=AA; B=BB; Z = HUGE(1.0_WP)
   CALL LA_HBGVX( A, B, W, UPLO, Z, IL=IL, IU=IU, M=M, IFAIL=IFAIL, Q=Q, INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
   IU=N; IL=1
! ERROR 9
   IU = N+1
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_HBGVX( A, B, W, UPLO, Z, IL=IL, IU=IU, M=M, IFAIL=IFAIL, Q=Q, INFO=INFO )' 
   A=AA; B=BB; Z = HUGE(1.0_WP)
   CALL LA_HBGVX( A, B, W, UPLO, Z, IL=IL, IU=IU, M=M, IFAIL=IFAIL, Q=Q, INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
   IU=N
! ERROR 10
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_HBGVX( A, B, W,  UPLO, Z, VL, VU, M=M, IFAIL=IFAIL(1:N-1), Q=Q, INFO=INFO )' 
   A=AA; B=BB; Z = HUGE(1.0_WP)
   CALL LA_HBGVX( A, B, W, UPLO, Z, VL, VU, M=M, IFAIL=IFAIL(1:N-1), Q=Q, INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
! ERROR 10
   UPLO= 'L'
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_HBGVX( A, B, W,  UPLO, VL=VL, VU=VU, M=M, IFAIL=IFAIL, Q=Q, INFO=INFO )' 
   A=AA; B=BB; Z = HUGE(1.0_WP)
   CALL LA_HBGVX( A, B, W, UPLO, VL=VL, VU=VU, M=M, IFAIL=IFAIL, Q=Q, INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
! ERROR 11
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_HBGVX( A, B, W, UPLO, Z, VL, VU, M=M, IFAIL=IFAIL, Q=Q(1:N-1,:), INFO=INFO )'
   A=AA; B=BB; Z = HUGE(1.0_WP)
   CALL LA_HBGVX( A, B, W, UPLO, Z,  VL, VU, M=M, IFAIL=IFAIL, Q=Q(1:N-1,:), INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO 
! ERROR 11
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_HBGVX( A, B, W, UPLO, Z, VL, VU, M=M, IFAIL=IFAIL, Q=Q(:,1:N-1), INFO=INFO )'
   A=AA; B=BB; Z = HUGE(1.0_WP)
   CALL LA_HBGVX( A, B, W, UPLO, Z,  VL, VU, M=M, IFAIL=IFAIL, Q=Q(:,1:N-1), INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
! ERROR 11
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_HBGVX( A, B, W, UPLO, VL=VL, VU=VU, M=M, IFAIL=IFAIL, Q=Q, INFO=INFO )'
   A=AA; B=BB; Z = HUGE(1.0_WP)
   CALL LA_HBGVX( A, B, W, UPLO, VL=VL, VU=VU, M=M, IFAIL=IFAIL, Q=Q, INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO   
!
END PROGRAM LA_ZHBGVX_ET_EXAMPLE
