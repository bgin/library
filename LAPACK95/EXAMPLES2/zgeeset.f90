PROGRAM LA_ZGEES_ET_EXAMPLE
!
!  -- LAPACK95 interface driver routine (version 3.0) --
!     UNI-C, Denmark; Univ. of Tennessee, USA; NAG Ltd., UK
!     September, 2000
!
!  .. USE STATEMENTS
   USE LA_PRECISION, ONLY: WP => DP
   USE F95_LAPACK, ONLY: LA_GEES
!  .. IMPLICIT STATEMENT ..
   IMPLICIT NONE
   INTERFACE
     LOGICAL FUNCTION SELECT(W)
        USE LA_PRECISION, ONLY: WP => DP
        COMPLEX(WP), INTENT(IN) :: W
     END FUNCTION SELECT
   END INTERFACE
!  .. PARAMETERS ..
      CHARACTER(LEN=*), PARAMETER :: FMT = '(4(1X,1H(,F7.3,1H,,F7.3,1H):))'
   INTEGER, PARAMETER :: NIN=5, NOUT=6
!  .. LOCAL SCALARS ..
   INTEGER :: I, INFO, N, SDIM
!  .. LOCAL ARRAYS ..
   REAL(WP), ALLOCATABLE :: AA(:,:)
   COMPLEX(WP), ALLOCATABLE :: A(:,:), VS(:,:), DUMMY(:,:)
   COMPLEX(WP), ALLOCATABLE :: W(:)
!  .. EXECUTABLE STATEMENTS ..
   WRITE (NOUT,*) 'GEES ET_Example Program Results.'
   READ ( NIN, * )   ! SKIP HEADING IN DATA FILE
   READ ( NIN, * ) N
   PRINT *, 'N = ', N
   ALLOCATE ( A(N,N), AA(N,N), W(N), VS(N,N) )
!
      READ (NIN, *) AA
   A=AA
      WRITE(NOUT,*) 'The matrix A:'
      DO I = 1, N; WRITE (NOUT,*) 'I = ', I; WRITE (NOUT,FMT) A(I,:); ENDDO
!
   WRITE ( NOUT, * )'---------------------------------------------------------'
   WRITE ( NOUT, * )
   WRITE ( NOUT, * )'Details of LA_ZGEES LAPACK Subroutine Results.'
   WRITE ( NOUT, * )
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GEES( A, W, VS, SELECT, SDIM, INFO )'
   A=AA
   CALL LA_GEES( A, W, VS, SELECT, SDIM, INFO )
   WRITE(NOUT,*) 'INFO = ', INFO, ' SDIM = ', SDIM, ' Eigenvalues:'
   WRITE(NOUT,FMT) W
   WRITE(NOUT,*) 'Schur vectors:'
   DO I = 1, N; WRITE(NOUT,*) 'I = ', I; WRITE (NOUT,FMT) VS(:,I); END DO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GEES( A, W, VS )'
   A=AA; VS = HUGE(1.0_WP)
   CALL LA_GEES( A, W, VS )
   WRITE(NOUT,*) 'INFO = ', INFO, ' Eigenvalues:'
   WRITE(NOUT,FMT) W
   WRITE(NOUT,*) 'Schur vectors:'
   DO I = 1, N; WRITE(NOUT,*) 'I = ', I; WRITE (NOUT,FMT) VS(:,I); END DO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GEES( A, W, SELECT=SELECT, SDIM=SDIM, INFO=INFO )'
   A=AA
   CALL LA_GEES( A, W, SELECT=SELECT, SDIM = SDIM, INFO = INFO )
   WRITE(NOUT,*) 'INFO = ', INFO, ' SDIM = ', SDIM, ' Eigenvalues:'
   WRITE(NOUT,FMT) W
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GEES( A, W )'
   A=AA
   CALL LA_GEES( A, W )
   WRITE(NOUT,*) 'INFO = ', INFO, ' Eigenvalues:'
   WRITE(NOUT,FMT) W
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GEES( DUMMY, W, VS, SELECT, SDIM, INFO )'
   A=AA
   CALL LA_GEES( DUMMY, W, VS, SELECT, SDIM, INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GEES( A, WR(1:N-1), VS, SELECT, SDIM, INFO )'
   A=AA
   CALL LA_GEES( A, W(1:N-1), VS, SELECT, SDIM, INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GEES( A, W(1:N-1), VS, SELECT, SDIM, INFO )'
   A=AA
   CALL LA_GEES( A, W(1:N-1), VS, SELECT, SDIM, INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GEES( A, W, VS(1:N-1,:), SELECT, SDIM, INFO )'
   A=AA
   CALL LA_GEES( A, W, VS(1:N-1,:), SELECT, SDIM, INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GEES( A, W, VS(:,1:N-1), SELECT, SDIM, INFO )'
   A=AA
   CALL LA_GEES( A, W, VS(:,1:N-1), SELECT, SDIM, INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GEES( A, W, VS, SDIM=SDIM )'
   A=AA
   CALL LA_GEES( A, W, VS, SDIM=SDIM )
   WRITE(NOUT,*) 'INFO = ', INFO
!
END PROGRAM LA_ZGEES_ET_EXAMPLE
!  CONTAINS
   LOGICAL FUNCTION SELECT(W)
      USE LA_PRECISION, ONLY: WP => DP
      COMPLEX(WP), INTENT(IN) :: W
      INTRINSIC AIMAG
      IF( AIMAG(W) > 0.0_WP ) THEN; SELECT = .TRUE.
      ELSE; SELECT = .FALSE.; END IF 
   END FUNCTION SELECT
