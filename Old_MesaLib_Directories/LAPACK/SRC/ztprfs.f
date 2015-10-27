      SUBROUTINE ZTPRFS( UPLO, TRANS, DIAG, N, NRHS, AP, B, LDB, X, LDX,
     $                   FERR, BERR, WORK, RWORK, INFO )
*
*  -- LAPACK routine (version 1.1) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     March 31, 1993
*
*     .. Scalar Arguments ..
      CHARACTER          DIAG, TRANS, UPLO
      INTEGER            INFO, LDB, LDX, N, NRHS
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   BERR( * ), FERR( * ), RWORK( * )
      COMPLEX*16         AP( * ), B( LDB, * ), WORK( * ), X( LDX, * )
*     ..
*
*  Purpose
*  =======
*
*  ZTPRFS provides error bounds and backward error estimates for the
*  solution to a system of linear equations with a triangular packed
*  coefficient matrix.
*
*  The solution matrix X must be computed by ZTPTRS or some other
*  means before entering this routine.  ZTPRFS does not do iterative
*  refinement because doing so cannot improve the backward error.
*
*  Arguments
*  =========
*
*  UPLO    (input) CHARACTER*1
*          = 'U':  A is upper triangular;
*          = 'L':  A is lower triangular.
*
*  TRANS   (input) CHARACTER*1
*          Specifies the form of the system of equations:
*          = 'N':  A * X = B     (No transpose)
*          = 'T':  A**T * X = B  (Transpose)
*          = 'C':  A**H * X = B  (Conjugate transpose)
*
*  DIAG    (input) CHARACTER*1
*          = 'N':  A is non-unit triangular;
*          = 'U':  A is unit triangular.
*
*  N       (input) INTEGER
*          The order of the matrix A.  N >= 0.
*
*  NRHS    (input) INTEGER
*          The number of right hand sides, i.e., the number of columns
*          of the matrices B and X.  NRHS >= 0.
*
*  AP      (input) COMPLEX*16 array, dimension (N*(N+1)/2)
*          The upper or lower triangular matrix A, packed columnwise in
*          a linear array.  The j-th column of A is stored in the array
*          AP as follows:
*          if UPLO = 'U', AP(i + (j-1)*j/2) = A(i,j) for 1<=i<=j;
*          if UPLO = 'L', AP(i + (j-1)*(2n-j)/2) = A(i,j) for j<=i<=n.
*          If DIAG = 'U', the diagonal elements of A are not referenced
*          and are assumed to be 1.
*
*  B       (input) COMPLEX*16 array, dimension (LDB,NRHS)
*          The right hand side matrix B.
*
*  LDB     (input) INTEGER
*          The leading dimension of the array B.  LDB >= max(1,N).
*
*  X       (input) COMPLEX*16 array, dimension (LDX,NRHS)
*          The solution matrix X.
*
*  LDX     (input) INTEGER
*          The leading dimension of the array X.  LDX >= max(1,N).
*
*  FERR    (output) DOUBLE PRECISION array, dimension (NRHS)
*          The estimated forward error bounds for each solution vector
*          X(j) (the j-th column of the solution matrix X).
*          If XTRUE is the true solution, FERR(j) bounds the magnitude
*          of the largest entry in (X(j) - XTRUE) divided by
*          the magnitude of the largest entry in X(j).  The quality of
*          the error bound depends on the quality of the estimate of
*          norm(inv(A)) computed in the code; if the estimate of
*          norm(inv(A)) is accurate, the error bound is guaranteed.
*
*  BERR    (output) DOUBLE PRECISION array, dimension (NRHS)
*          The componentwise relative backward error of each solution
*          vector X(j) (i.e., the smallest relative change in
*          any entry of A or B that makes X(j) an exact solution).
*
*  WORK    (workspace) COMPLEX*16 array, dimension (2*N)
*
*  RWORK   (workspace) DOUBLE PRECISION array, dimension (N)
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D+0 )
      COMPLEX*16         ONE
      PARAMETER          ( ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            NOTRAN, NOUNIT, UPPER
      CHARACTER          TRANSN, TRANST
      INTEGER            I, J, K, KASE, KC, NZ
      DOUBLE PRECISION   EPS, LSTRES, S, SAFE1, SAFE2, SAFMIN, XK
      COMPLEX*16         ZDUM
*     ..
*     .. External Subroutines ..
      EXTERNAL           XERBLA, ZAXPY, ZCOPY, ZLACON, ZTPMV, ZTPSV
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, DIMAG, MAX
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           LSAME, DLAMCH
*     ..
*     .. Statement Functions ..
      DOUBLE PRECISION   CABS1
*     ..
*     .. Statement Function definitions ..
      CABS1( ZDUM ) = ABS( DBLE( ZDUM ) ) + ABS( DIMAG( ZDUM ) )
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      UPPER = LSAME( UPLO, 'U' )
      NOTRAN = LSAME( TRANS, 'N' )
      NOUNIT = LSAME( DIAG, 'N' )
*
      IF( .NOT.UPPER .AND. .NOT.LSAME( UPLO, 'L' ) ) THEN
         INFO = -1
      ELSE IF( .NOT.NOTRAN .AND. .NOT.LSAME( TRANS, 'T' ) .AND. .NOT.
     $         LSAME( TRANS, 'C' ) ) THEN
         INFO = -2
      ELSE IF( .NOT.NOUNIT .AND. .NOT.LSAME( DIAG, 'U' ) ) THEN
         INFO = -3
      ELSE IF( N.LT.0 ) THEN
         INFO = -4
      ELSE IF( NRHS.LT.0 ) THEN
         INFO = -5
      ELSE IF( LDB.LT.MAX( 1, N ) ) THEN
         INFO = -8
      ELSE IF( LDX.LT.MAX( 1, N ) ) THEN
         INFO = -10
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'ZTPRFS', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( N.EQ.0 .OR. NRHS.EQ.0 ) THEN
         DO 10 J = 1, NRHS
            FERR( J ) = ZERO
            BERR( J ) = ZERO
   10    CONTINUE
         RETURN
      END IF
*
      IF( NOTRAN ) THEN
         TRANSN = 'N'
         TRANST = 'C'
      ELSE
         TRANSN = 'C'
         TRANST = 'N'
      END IF
*
*     NZ = maximum number of nonzero entries in each row of A, plus 1
*
      NZ = N + 1
      EPS = DLAMCH( 'Epsilon' )
      SAFMIN = DLAMCH( 'Safe minimum' )
      SAFE1 = NZ*SAFMIN
      SAFE2 = SAFE1 / EPS
*
*     Do for each right hand side
*
      DO 250 J = 1, NRHS
*
*        Compute residual R = B - op(A) * X,
*        where op(A) = A, A**T, or A**H, depending on TRANS.
*
         CALL ZCOPY( N, X( 1, J ), 1, WORK, 1 )
         CALL ZTPMV( UPLO, TRANS, DIAG, N, AP, WORK, 1 )
         CALL ZAXPY( N, -ONE, B( 1, J ), 1, WORK, 1 )
*
*        Compute componentwise relative backward error from formula
*
*        max(i) ( abs(R(i)) / ( abs(op(A))*abs(X) + abs(B) )(i) )
*
*        where abs(Z) is the componentwise absolute value of the matrix
*        or vector Z.  If the i-th component of the denominator is less
*        than SAFE2, then SAFE1 is added to the i-th components of the
*        numerator and denominator before dividing.
*
         DO 20 I = 1, N
            RWORK( I ) = CABS1( B( I, J ) )
   20    CONTINUE
*
         IF( NOTRAN ) THEN
*
*           Compute abs(A)*abs(X) + abs(B).
*
            IF( UPPER ) THEN
               KC = 1
               IF( NOUNIT ) THEN
                  DO 40 K = 1, N
                     XK = CABS1( X( K, J ) )
                     DO 30 I = 1, K
                        RWORK( I ) = RWORK( I ) +
     $                               CABS1( AP( KC+I-1 ) )*XK
   30                CONTINUE
                     KC = KC + K
   40             CONTINUE
               ELSE
                  DO 60 K = 1, N
                     XK = CABS1( X( K, J ) )
                     DO 50 I = 1, K - 1
                        RWORK( I ) = RWORK( I ) +
     $                               CABS1( AP( KC+I-1 ) )*XK
   50                CONTINUE
                     RWORK( K ) = RWORK( K ) + XK
                     KC = KC + K
   60             CONTINUE
               END IF
            ELSE
               KC = 1
               IF( NOUNIT ) THEN
                  DO 80 K = 1, N
                     XK = CABS1( X( K, J ) )
                     DO 70 I = K, N
                        RWORK( I ) = RWORK( I ) +
     $                               CABS1( AP( KC+I-K ) )*XK
   70                CONTINUE
                     KC = KC + N - K + 1
   80             CONTINUE
               ELSE
                  DO 100 K = 1, N
                     XK = CABS1( X( K, J ) )
                     DO 90 I = K + 1, N
                        RWORK( I ) = RWORK( I ) +
     $                               CABS1( AP( KC+I-K ) )*XK
   90                CONTINUE
                     RWORK( K ) = RWORK( K ) + XK
                     KC = KC + N - K + 1
  100             CONTINUE
               END IF
            END IF
         ELSE
*
*           Compute abs(A**H)*abs(X) + abs(B).
*
            IF( UPPER ) THEN
               KC = 1
               IF( NOUNIT ) THEN
                  DO 120 K = 1, N
                     S = ZERO
                     DO 110 I = 1, K
                        S = S + CABS1( AP( KC+I-1 ) )*CABS1( X( I, J ) )
  110                CONTINUE
                     RWORK( K ) = RWORK( K ) + S
                     KC = KC + K
  120             CONTINUE
               ELSE
                  DO 140 K = 1, N
                     S = CABS1( X( K, J ) )
                     DO 130 I = 1, K - 1
                        S = S + CABS1( AP( KC+I-1 ) )*CABS1( X( I, J ) )
  130                CONTINUE
                     RWORK( K ) = RWORK( K ) + S
                     KC = KC + K
  140             CONTINUE
               END IF
            ELSE
               KC = 1
               IF( NOUNIT ) THEN
                  DO 160 K = 1, N
                     S = ZERO
                     DO 150 I = K, N
                        S = S + CABS1( AP( KC+I-K ) )*CABS1( X( I, J ) )
  150                CONTINUE
                     RWORK( K ) = RWORK( K ) + S
                     KC = KC + N - K + 1
  160             CONTINUE
               ELSE
                  DO 180 K = 1, N
                     S = CABS1( X( K, J ) )
                     DO 170 I = K + 1, N
                        S = S + CABS1( AP( KC+I-K ) )*CABS1( X( I, J ) )
  170                CONTINUE
                     RWORK( K ) = RWORK( K ) + S
                     KC = KC + N - K + 1
  180             CONTINUE
               END IF
            END IF
         END IF
         S = ZERO
         DO 190 I = 1, N
            IF( RWORK( I ).GT.SAFE2 ) THEN
               S = MAX( S, CABS1( WORK( I ) ) / RWORK( I ) )
            ELSE
               S = MAX( S, ( CABS1( WORK( I ) )+SAFE1 ) /
     $             ( RWORK( I )+SAFE1 ) )
            END IF
  190    CONTINUE
         BERR( J ) = S
*
*        Bound error from formula
*
*        norm(X - XTRUE) / norm(X) .le. FERR =
*        norm( abs(inv(op(A)))*
*           ( abs(R) + NZ*EPS*( abs(op(A))*abs(X)+abs(B) ))) / norm(X)
*
*        where
*          norm(Z) is the magnitude of the largest component of Z
*          inv(op(A)) is the inverse of op(A)
*          abs(Z) is the componentwise absolute value of the matrix or
*             vector Z
*          NZ is the maximum number of nonzeros in any row of A, plus 1
*          EPS is machine epsilon
*
*        The i-th component of abs(R)+NZ*EPS*(abs(op(A))*abs(X)+abs(B))
*        is incremented by SAFE1 if the i-th component of
*        abs(op(A))*abs(X) + abs(B) is less than SAFE2.
*
*        Use ZLACON to estimate the infinity-norm of the matrix
*           inv(op(A)) * diag(W),
*        where W = abs(R) + NZ*EPS*( abs(op(A))*abs(X)+abs(B) )))
*
         DO 200 I = 1, N
            IF( RWORK( I ).GT.SAFE2 ) THEN
               RWORK( I ) = CABS1( WORK( I ) ) + NZ*EPS*RWORK( I )
            ELSE
               RWORK( I ) = CABS1( WORK( I ) ) + NZ*EPS*RWORK( I ) +
     $                      SAFE1
            END IF
  200    CONTINUE
*
         KASE = 0
  210    CONTINUE
         CALL ZLACON( N, WORK( N+1 ), WORK, FERR( J ), KASE )
         IF( KASE.NE.0 ) THEN
            IF( KASE.EQ.1 ) THEN
*
*              Multiply by diag(W)*inv(op(A)**H).
*
               CALL ZTPSV( UPLO, TRANST, DIAG, N, AP, WORK, 1 )
               DO 220 I = 1, N
                  WORK( I ) = RWORK( I )*WORK( I )
  220          CONTINUE
            ELSE
*
*              Multiply by inv(op(A))*diag(W).
*
               DO 230 I = 1, N
                  WORK( I ) = RWORK( I )*WORK( I )
  230          CONTINUE
               CALL ZTPSV( UPLO, TRANSN, DIAG, N, AP, WORK, 1 )
            END IF
            GO TO 210
         END IF
*
*        Normalize error.
*
         LSTRES = ZERO
         DO 240 I = 1, N
            LSTRES = MAX( LSTRES, CABS1( X( I, J ) ) )
  240    CONTINUE
         IF( LSTRES.NE.ZERO )
     $      FERR( J ) = FERR( J ) / LSTRES
*
  250 CONTINUE
*
      RETURN
*
*     End of ZTPRFS
*
      END
