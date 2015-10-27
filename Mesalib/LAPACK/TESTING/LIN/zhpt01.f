      SUBROUTINE ZHPT01( UPLO, N, A, AFAC, IPIV, C, LDC, RWORK, RESID )
*
*  -- LAPACK test routine (version 1.1) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
      CHARACTER          UPLO
      INTEGER            LDC, N
      DOUBLE PRECISION   RESID
*     ..
*     .. Array Arguments ..
      INTEGER            IPIV( * )
      DOUBLE PRECISION   RWORK( * )
      COMPLEX*16         A( * ), AFAC( * ), C( LDC, * )
*     ..
*
*  Purpose
*  =======
*
*  ZHPT01 reconstructs a Hermitian indefinite packed matrix A from its
*  block L*D*L' or U*D*U' factorization and computes the residual
*     norm( C - A ) / ( N * norm(A) * EPS ),
*  where C is the reconstructed matrix, EPS is the machine epsilon,
*  L' is the conjugate transpose of L, and U' is the conjugate transpose
*  of U.
*
*  Arguments
*  ==========
*
*  UPLO    (input) CHARACTER*1
*          Specifies whether the upper or lower triangular part of the
*          Hermitian matrix A is stored:
*          = 'U':  Upper triangular
*          = 'L':  Lower triangular
*
*  N       (input) INTEGER
*          The number of rows and columns of the matrix A.  N >= 0.
*
*  A       (input) COMPLEX*16 array, dimension (N*(N+1)/2)
*          The original Hermitian matrix A, stored as a packed
*          triangular matrix.
*
*  AFAC    (input) COMPLEX*16 array, dimension (N*(N+1)/2)
*          The factored form of the matrix A, stored as a packed
*          triangular matrix.  AFAC contains the block diagonal matrix D
*          and the multipliers used to obtain the factor L or U from the
*          block L*D*L' or U*D*U' factorization as computed by ZHPTRF.
*
*  IPIV    (input) INTEGER array, dimension (N)
*          The pivot indices from ZHPTRF.
*
*  C       (workspace) COMPLEX*16 array, dimension (LDC,N)
*
*  LDC     (integer) INTEGER
*          The leading dimension of the array C.  LDC >= max(1,N).
*
*  RWORK   (workspace) DOUBLE PRECISION array, dimension (N)
*
*  RESID   (output) DOUBLE PRECISION
*          If UPLO = 'L', norm(L*D*L' - A) / ( N * norm(A) * EPS )
*          If UPLO = 'U', norm(U*D*U' - A) / ( N * norm(A) * EPS )
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
      COMPLEX*16         CZERO, CONE
      PARAMETER          ( CZERO = 0.0D+0, CONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, INFO, J, JC
      DOUBLE PRECISION   ANORM, EPS
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DLAMCH, ZLANSP, ZLANSY
      EXTERNAL           LSAME, DLAMCH, ZLANSP, ZLANSY
*     ..
*     .. External Subroutines ..
      EXTERNAL           ZLAVHP, ZLAZRO
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE
*     ..
*     .. Executable Statements ..
*
*     Quick exit if N = 0.
*
      IF( N.LE.0 ) THEN
         RESID = ZERO
         RETURN
      END IF
*
*     Determine EPS and the norm of A.
*
      EPS = DLAMCH( 'Epsilon' )
      ANORM = ZLANSP( '1', UPLO, N, A, RWORK )
*
*     Initialize C to the identity matrix.
*
      CALL ZLAZRO( N, N, CZERO, CONE, C, LDC )
*
*     Call ZLAVHP to form the product D * U' (or D * L' ).
*
      CALL ZLAVHP( UPLO, 'Conjugate', 'Non-unit', N, N, AFAC, IPIV, C,
     $             LDC, INFO )
*
*     Call ZLAVHP again to multiply by U ( or L ).
*
      CALL ZLAVHP( UPLO, 'No transpose', 'Unit', N, N, AFAC, IPIV, C,
     $             LDC, INFO )
*
*     Compute the difference  C - A .
*
      IF( LSAME( UPLO, 'U' ) ) THEN
         JC = 0
         DO 20 J = 1, N
            DO 10 I = 1, J
               C( I, J ) = C( I, J ) - A( JC+I )
   10       CONTINUE
            JC = JC + J
   20    CONTINUE
      ELSE
         JC = 1
         DO 40 J = 1, N
            DO 30 I = J, N
               C( I, J ) = C( I, J ) - A( JC+I-J )
   30       CONTINUE
            JC = JC + N - J + 1
   40    CONTINUE
      END IF
*
*     Compute norm( C - A ) / ( N * norm(A) * EPS )
*
      RESID = ZLANSY( '1', UPLO, N, C, LDC, RWORK )
*
      IF( ANORM.LE.ZERO ) THEN
         IF( RESID.NE.ZERO )
     $      RESID = ONE / EPS
      ELSE
         RESID = ( ( RESID / DBLE( N ) ) / ANORM ) / EPS
      END IF
*
      RETURN
*
*     End of ZHPT01
*
      END