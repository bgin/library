      SUBROUTINE DTBT05( UPLO, TRANS, DIAG, N, KD, NRHS, AB, LDAB, B,
     $                   LDB, X, LDX, XACT, LDXACT, FERR, BERR, RESLTS )
*
*  -- LAPACK test routine (version 1.1) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
      CHARACTER          DIAG, TRANS, UPLO
      INTEGER            KD, LDAB, LDB, LDX, LDXACT, N, NRHS
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   AB( LDAB, * ), B( LDB, * ), BERR( * ),
     $                   FERR( * ), RESLTS( * ), X( LDX, * ),
     $                   XACT( LDXACT, * )
*     ..
*
*  Purpose
*  =======
*
*  DTBT05 tests the error bounds from iterative refinement for the
*  computed solution to a system of equations A*X = B, where A is a
*  triangular band matrix.
*
*  RESLTS(1) = test of the error bound
*            = norm(X - XACT) / ( norm(X) * FERR )
*
*  A large value is returned if this ratio is not less than one.
*
*  RESLTS(2) = residual from the iterative refinement routine
*            = the maximum of BERR / ( NZ*EPS + (*) ), where
*              (*) = NZ*UNFL / (min_i (abs(A)*abs(X) +abs(b))_i )
*              and NZ = max. number of nonzeros in any row of A, plus 1
*
*  Arguments
*  =========
*
*  UPLO    (input) CHARACTER*1
*          Specifies whether the matrix A is upper or lower triangular.
*          = 'U':  Upper triangular
*          = 'L':  Lower triangular
*
*  TRANS   (input) CHARACTER*1
*          Specifies the form of the system of equations.
*          = 'N':  A * X = B  (No transpose)
*          = 'T':  A'* X = B  (Transpose)
*          = 'C':  A'* X = B  (Conjugate transpose = Transpose)
*
*  DIAG    (input) CHARACTER*1
*          Specifies whether or not the matrix A is unit triangular.
*          = 'N':  Non-unit triangular
*          = 'U':  Unit triangular
*
*  N       (input) INTEGER
*          The number of rows of the matrices X, B, and XACT, and the
*          order of the matrix A.  N >= 0.
*
*  KD      (input) INTEGER
*          The number of super-diagonals of the matrix A if UPLO = 'U',
*          or the number of sub-diagonals if UPLO = 'L'.  KD >= 0.
*
*  NRHS    (input) INTEGER
*          The number of columns of the matrices X, B, and XACT.
*          NRHS >= 0.
*
*  AB      (input) DOUBLE PRECISION array, dimension (LDAB,N)
*          The upper or lower triangular band matrix A, stored in the
*          first kd+1 rows of the array. The j-th column of A is stored
*          in the j-th column of the array AB as follows:
*          if UPLO = 'U', AB(kd+1+i-j,j) = A(i,j) for max(1,j-kd)<=i<=j;
*          if UPLO = 'L', AB(1+i-j,j)    = A(i,j) for j<=i<=min(n,j+kd).
*          If DIAG = 'U', the diagonal elements of A are not referenced
*          and are assumed to be 1.
*
*  LDAB    (input) INTEGER
*          The leading dimension of the array AB.  LDAB >= KD+1.
*
*  B       (input) DOUBLE PRECISION array, dimension (LDB,NRHS)
*          The right hand side vectors for the system of linear
*          equations.
*
*  LDB     (input) INTEGER
*          The leading dimension of the array B.  LDB >= max(1,N).
*
*  X       (input) DOUBLE PRECISION array, dimension (LDX,NRHS)
*          The computed solution vectors.  Each vector is stored as a
*          column of the matrix X.
*
*  LDX     (input) INTEGER
*          The leading dimension of the array X.  LDX >= max(1,N).
*
*  XACT    (input) DOUBLE PRECISION array, dimension (LDX,NRHS)
*          The exact solution vectors.  Each vector is stored as a
*          column of the matrix XACT.
*
*  LDXACT  (input) INTEGER
*          The leading dimension of the array XACT.  LDXACT >= max(1,N).
*
*  FERR    (input) DOUBLE PRECISION array, dimension (NRHS)
*          The estimated forward error bounds for each solution vector
*          X.  If XTRUE is the true solution, FERR bounds the magnitude
*          of the largest entry in (X - XTRUE) divided by the magnitude
*          of the largest entry in X.
*
*  BERR    (input) DOUBLE PRECISION array, dimension (NRHS)
*          The componentwise relative backward error of each solution
*          vector (i.e., the smallest relative change in any entry of A
*          or B that makes X an exact solution).
*
*  RESLTS  (output) DOUBLE PRECISION array, dimension (2)
*          The maximum over the NRHS solution vectors of the ratios:
*          RESLTS(1) = norm(X - XACT) / ( norm(X) * FERR )
*          RESLTS(2) = BERR / ( NZ*EPS + (*) )
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            NOTRAN, UNIT, UPPER
      INTEGER            I, IFU, IMAX, J, K, NZ
      DOUBLE PRECISION   AXBI, DIFF, EPS, ERRBND, OVFL, TMP, UNFL, XNORM
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            IDAMAX
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           LSAME, IDAMAX, DLAMCH
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, MIN
*     ..
*     .. Executable Statements ..
*
*     Quick exit if N = 0 or NRHS = 0.
*
      IF( N.LE.0 .OR. NRHS.LE.0 ) THEN
         RESLTS( 1 ) = ZERO
         RESLTS( 2 ) = ZERO
         RETURN
      END IF
*
      EPS = DLAMCH( 'Epsilon' )
      UNFL = DLAMCH( 'Safe minimum' )
      OVFL = ONE / UNFL
      UPPER = LSAME( UPLO, 'U' )
      NOTRAN = LSAME( TRANS, 'N' )
      UNIT = LSAME( DIAG, 'U' )
      NZ = MIN( KD, N-1 ) + 1
*
*     Test 1:  Compute the maximum of
*        norm(X - XACT) / ( norm(X) * FERR )
*     over all the vectors X and XACT using the infinity-norm.
*
      ERRBND = ZERO
      DO 30 J = 1, NRHS
         IMAX = IDAMAX( N, X( 1, J ), 1 )
         XNORM = MAX( ABS( X( IMAX, J ) ), UNFL )
         DIFF = ZERO
         DO 10 I = 1, N
            DIFF = MAX( DIFF, ABS( X( I, J )-XACT( I, J ) ) )
   10    CONTINUE
*
         IF( XNORM.GT.ONE ) THEN
            GO TO 20
         ELSE IF( DIFF.LE.OVFL*XNORM ) THEN
            GO TO 20
         ELSE
            ERRBND = ONE / EPS
            GO TO 30
         END IF
*
   20    CONTINUE
         IF( DIFF / XNORM.LE.FERR( J ) ) THEN
            ERRBND = MAX( ERRBND, ( DIFF / XNORM ) / FERR( J ) )
         ELSE
            ERRBND = ONE / EPS
         END IF
   30 CONTINUE
      RESLTS( 1 ) = ERRBND
*
*     Test 2:  Compute the maximum of BERR / ( NZ*EPS + (*) ), where
*     (*) = NZ*UNFL / (min_i (abs(A)*abs(X) +abs(b))_i )
*
      IFU = 0
      IF( UNIT )
     $   IFU = 1
      DO 90 K = 1, NRHS
         DO 80 I = 1, N
            TMP = ABS( B( I, K ) )
            IF( UPPER ) THEN
               IF( .NOT.NOTRAN ) THEN
                  DO 40 J = MAX( I-KD, 1 ), I - IFU
                     TMP = TMP + ABS( AB( KD+1-I+J, I ) )*
     $                     ABS( X( J, K ) )
   40             CONTINUE
                  IF( UNIT )
     $               TMP = TMP + ABS( X( I, K ) )
               ELSE
                  IF( UNIT )
     $               TMP = TMP + ABS( X( I, K ) )
                  DO 50 J = I + IFU, MIN( I+KD, N )
                     TMP = TMP + ABS( AB( KD+1+I-J, J ) )*
     $                     ABS( X( J, K ) )
   50             CONTINUE
               END IF
            ELSE
               IF( NOTRAN ) THEN
                  DO 60 J = MAX( I-KD, 1 ), I - IFU
                     TMP = TMP + ABS( AB( 1+I-J, J ) )*ABS( X( J, K ) )
   60             CONTINUE
                  IF( UNIT )
     $               TMP = TMP + ABS( X( I, K ) )
               ELSE
                  IF( UNIT )
     $               TMP = TMP + ABS( X( I, K ) )
                  DO 70 J = I + IFU, MIN( I+KD, N )
                     TMP = TMP + ABS( AB( 1+J-I, I ) )*ABS( X( J, K ) )
   70             CONTINUE
               END IF
            END IF
            IF( I.EQ.1 ) THEN
               AXBI = TMP
            ELSE
               AXBI = MIN( AXBI, TMP )
            END IF
   80    CONTINUE
         TMP = BERR( K ) / ( NZ*EPS+NZ*UNFL / MAX( AXBI, NZ*UNFL ) )
         IF( K.EQ.1 ) THEN
            RESLTS( 2 ) = TMP
         ELSE
            RESLTS( 2 ) = MAX( RESLTS( 2 ), TMP )
         END IF
   90 CONTINUE
*
      RETURN
*
*     End of DTBT05
*
      END
