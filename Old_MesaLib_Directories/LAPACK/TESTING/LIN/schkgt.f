      SUBROUTINE SCHKGT( DOTYPE, NN, NVAL, NRHS, THRESH, TSTERR, A, AF,
     $                   B, X, XACT, WORK, RWORK, IWORK, NOUT )
*
*  -- LAPACK test routine (version 1.1) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
      LOGICAL            TSTERR
      INTEGER            NN, NOUT, NRHS
      REAL               THRESH
*     ..
*     .. Array Arguments ..
      LOGICAL            DOTYPE( * )
      INTEGER            IWORK( * ), NVAL( * )
      REAL               A( * ), AF( * ), B( * ), RWORK( * ), WORK( * ),
     $                   X( * ), XACT( * )
*     ..
*
*  Purpose
*  =======
*
*  SCHKGT tests SGTTRF, -TRS, -RFS, and -CON
*
*  Arguments
*  =========
*
*  DOTYPE  (input) LOGICAL array, dimension (NTYPES)
*          The matrix types to be used for testing.  Matrices of type j
*          (for 1 <= j <= NTYPES) are used for testing if DOTYPE(j) =
*          .TRUE.; if DOTYPE(j) = .FALSE., then type j is not used.
*
*  NN      (input) INTEGER
*          The number of values of N contained in the vector NVAL.
*
*  NVAL    (input) INTEGER array, dimension (NN)
*          The values of the matrix dimension N.
*
*  THRESH  (input) REAL
*          The threshold value for the test ratios.  A result is
*          included in the output file if RESULT >= THRESH.  To have
*          every test ratio printed, use THRESH = 0.
*
*  TSTERR  (input) LOGICAL
*          Flag that indicates whether error exits are to be tested.
*
*  A       (workspace) REAL array, dimension (NMAX*4)
*
*  AF      (workspace) REAL array, dimension (NMAX*4)
*
*  B       (workspace) REAL array, dimension (NMAX*NRHS)
*
*  X       (workspace) REAL array, dimension (NMAX*NRHS)
*
*  XACT    (workspace) REAL array, dimension (NMAX*NRHS)
*
*  WORK    (workspace) REAL array, dimension
*                      (NMAX*max(3,NRHS))
*
*  RWORK   (workspace) REAL array, dimension
*                      (max(NMAX,2*NRHS))
*
*  IWORK   (workspace) INTEGER array, dimension (2*NMAX)
*
*  NOUT    (input) INTEGER
*          The unit number for output.
*
*  =====================================================================
*
*     .. Parameters ..
      REAL               ONE, ZERO
      PARAMETER          ( ONE = 1.0E+0, ZERO = 0.0E+0 )
      INTEGER            NTYPES
      PARAMETER          ( NTYPES = 12 )
      INTEGER            NTESTS
      PARAMETER          ( NTESTS = 7 )
*     ..
*     .. Local Scalars ..
      LOGICAL            TRFCON, ZEROT
      CHARACTER          DIST, NORM, TRANS, TYPE
      CHARACTER*3        PATH
      INTEGER            I, IMAT, IN, INFO, ITRAN, IX, IZERO, J, K, K1,
     $                   KL, KOFF, KU, LDA, MODE, N, NERRS, NFAIL,
     $                   NIMAT, NRUN, NT
      REAL               AINVNM, ANORM, COND, RCOND, RCONDC
*     ..
*     .. Local Arrays ..
      CHARACTER          TRANSS( 3 )
      INTEGER            ISEED( 4 ), ISEEDY( 4 )
      REAL               RESULT( NTESTS ), Z( 3 )
*     ..
*     .. External Functions ..
      REAL               SASUM, SGET06, SLANGT
      EXTERNAL           SASUM, SGET06, SLANGT
*     ..
*     .. External Subroutines ..
      EXTERNAL           ALAERH, ALAHD, ALASUM, SCOPY, SERRGE, SGET04,
     $                   SGTCON, SGTRFS, SGTT01, SGTT02, SGTT05, SGTTRF,
     $                   SGTTRS, SLACPY, SLAGTM, SLARNV, SLATB4, SLATMS,
     $                   SSCAL
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Scalars in Common ..
      LOGICAL            LERR, OK
      CHARACTER*6        SRNAMT
      INTEGER            INFOT, NUNIT
*     ..
*     .. Common blocks ..
      COMMON             / INFOC / INFOT, NUNIT, OK, LERR
      COMMON             / SRNAMC / SRNAMT
*     ..
*     .. Data statements ..
      DATA               ISEEDY / 0, 0, 0, 1 / , TRANSS / 'N', 'T',
     $                   'C' /
*     ..
*     .. Executable Statements ..
*
      PATH( 1: 1 ) = 'Single precision'
      PATH( 2: 3 ) = 'GT'
      NRUN = 0
      NFAIL = 0
      NERRS = 0
      DO 10 I = 1, 4
         ISEED( I ) = ISEEDY( I )
   10 CONTINUE
*
*     Test the error exits
*
      IF( TSTERR )
     $   CALL SERRGE( PATH, NOUT )
      INFOT = 0
*
      DO 100 IN = 1, NN
*
*        Do for each value of N in NVAL.
*
         N = NVAL( IN )
         LDA = MAX( 1, N )
         NIMAT = NTYPES
         IF( N.LE.0 )
     $      NIMAT = 1
*
         DO 90 IMAT = 1, NIMAT
*
*           Do the tests only if DOTYPE( IMAT ) is true.
*
            IF( .NOT.DOTYPE( IMAT ) )
     $         GO TO 90
*
*           Set up parameters with SLATB4.
*
            CALL SLATB4( PATH, IMAT, N, N, TYPE, KL, KU, ANORM, MODE,
     $                   COND, DIST )
*
            ZEROT = IMAT.GE.8 .AND. IMAT.LE.10
            IF( IMAT.LE.6 ) THEN
*
*              Types 1-6:  generate matrices of known condition number.
*
               KOFF = MAX( 2-KU, 3-MAX( 1, N ) )
               SRNAMT = 'SLATMS'
               CALL SLATMS( N, N, DIST, ISEED, TYPE, RWORK, MODE, COND,
     $                      ANORM, KL, KU, 'Z', AF( KOFF ), 3, WORK,
     $                      INFO )
*
*              Check the error code from SLATMS.
*
               IF( INFO.NE.0 ) THEN
                  CALL ALAERH( PATH, 'SLATMS', INFO, 0, ' ', N, N, KL,
     $                         KU, -1, IMAT, NFAIL, NERRS, NOUT )
                  GO TO 90
               END IF
               IZERO = 0
*
               IF( N.GT.1 ) THEN
                  CALL SCOPY( N-1, AF( 4 ), 3, A, 1 )
                  CALL SCOPY( N-1, AF( 3 ), 3, A( 2*N ), 1 )
               END IF
               CALL SCOPY( N, AF( 2 ), 3, A( N ), 1 )
            ELSE
*
*              Types 7-12:  generate tridiagonal matrices with
*              unknown condition numbers.
*
               IF( .NOT.ZEROT .OR. .NOT.DOTYPE( 7 ) ) THEN
*
*                 Generate a matrix with elements from [-1,1].
*
                  CALL SLARNV( 2, ISEED, 3*N-2, A )
                  IF( ANORM.NE.ONE )
     $               CALL SSCAL( 3*N-2, ANORM, A, 1 )
               ELSE IF( IZERO.GT.0 ) THEN
*
*                 Reuse the last matrix by copying back the zeroed out
*                 elements.
*
                  IF( IZERO.EQ.1 ) THEN
                     A( N ) = Z( 2 )
                     IF( N.GT.1 )
     $                  A( 1 ) = Z( 3 )
                  ELSE IF( IZERO.EQ.N ) THEN
                     A( 3*N-2 ) = Z( 1 )
                     A( 2*N-1 ) = Z( 2 )
                  ELSE
                     A( 2*N-2+IZERO ) = Z( 1 )
                     A( N-1+IZERO ) = Z( 2 )
                     A( IZERO ) = Z( 3 )
                  END IF
               END IF
*
*              If IMAT > 7, set one column of the matrix to 0.
*
               IF( .NOT.ZEROT ) THEN
                  IZERO = 0
               ELSE IF( IMAT.EQ.8 ) THEN
                  IZERO = 1
                  Z( 2 ) = A( N )
                  A( N ) = ZERO
                  IF( N.GT.1 ) THEN
                     Z( 3 ) = A( 1 )
                     A( 1 ) = ZERO
                  END IF
               ELSE IF( IMAT.EQ.9 ) THEN
                  IZERO = N
                  Z( 1 ) = A( 3*N-2 )
                  Z( 2 ) = A( 2*N-1 )
                  A( 3*N-2 ) = ZERO
                  A( 2*N-1 ) = ZERO
               ELSE
                  IZERO = ( N+1 ) / 2
                  DO 20 I = IZERO, N - 1
                     A( 2*N-2+I ) = ZERO
                     A( N-1+I ) = ZERO
                     A( I ) = ZERO
   20             CONTINUE
                  A( 3*N-2 ) = ZERO
                  A( 2*N-1 ) = ZERO
               END IF
            END IF
*
*+    TEST 1
*           Factor A as L*U and compute the ratio
*              norm(L*U - A) / (n * norm(A) * EPS )
*
            IF( N.GT.0 )
     $         CALL SCOPY( 3*N-2, A, 1, AF, 1 )
            SRNAMT = 'SGTTRF'
            CALL SGTTRF( N, AF, AF( N ), AF( 2*N ), AF( 3*N-1 ), IWORK,
     $                   INFO )
*
*           Check error code from SGTTRF.
*
            IF( INFO.NE.IZERO )
     $         CALL ALAERH( PATH, 'SGTTRF', INFO, IZERO, ' ', N, N, 1,
     $                      1, -1, IMAT, NFAIL, NERRS, NOUT )
            TRFCON = INFO.NE.0
*
            CALL SGTT01( N, A, A( N ), A( 2*N ), AF, AF( N ), AF( 2*N ),
     $                   AF( 3*N-1 ), IWORK, WORK, LDA, RWORK,
     $                   RESULT( 1 ) )
*
*           Print information about the tests so far that did not
*           pass the threshold.
*
            IF( RESULT( 1 ).GE.THRESH ) THEN
               IF( NFAIL.EQ.0 .AND. NERRS.EQ.0 )
     $            CALL ALAHD( NOUT, PATH )
               WRITE( NOUT, FMT = 9999 )N, IMAT, 1, RESULT( 1 )
               NFAIL = NFAIL + 1
            END IF
            NRUN = NRUN + 1
*
*           Generate NRHS random solution vectors.
*
            IF( .NOT.TRFCON ) THEN
               IX = 1
               DO 30 J = 1, NRHS
                  CALL SLARNV( 2, ISEED, N, XACT( IX ) )
                  IX = IX + LDA
   30          CONTINUE
            END IF
*
            DO 80 ITRAN = 1, 3
               TRANS = TRANSS( ITRAN )
               IF( ITRAN.EQ.1 ) THEN
                  NORM = 'O'
               ELSE
                  NORM = 'I'
               END IF
*
*              Skip all but the condition estimate if TRFCON is true.
*
               IF( TRFCON ) THEN
                  K1 = 7
                  RCONDC = ZERO
                  GO TO 60
               END IF
*
               K1 = 2
               IF( ITRAN.LE.2 ) THEN
*
*                 Compute RCONDC = 1 / (norm(A) * norm(inv(A))
*
                  ANORM = SLANGT( NORM, N, A, A( N ), A( 2*N ) )
*
*                 Use SGTTRS to solve for one column at a time of
*                 inv(A), computing the maximum column sum as we go.
*
                  AINVNM = ZERO
                  DO 50 I = 1, N
                     DO 40 J = 1, N
                        X( J ) = ZERO
   40                CONTINUE
                     X( I ) = ONE
                     CALL SGTTRS( TRANS, N, 1, AF, AF( N ), AF( 2*N ),
     $                            AF( 3*N-1 ), IWORK, X, LDA, INFO )
                     AINVNM = MAX( AINVNM, SASUM( N, X, 1 ) )
   50             CONTINUE
                  IF( ANORM.LE.ZERO .OR. AINVNM.LE.ZERO ) THEN
                     RCONDC = ONE
                  ELSE
                     RCONDC = ( ONE / ANORM ) / AINVNM
                  END IF
               END IF
*
*              Set the right hand side.
*
               CALL SLAGTM( TRANS, N, NRHS, ONE, A, A( N ), A( 2*N ),
     $                      XACT, LDA, ZERO, B, LDA )
*
*+    TEST 2
*              Solve op(A) * X = B and compute the residual.
*
               CALL SLACPY( 'Full', N, NRHS, B, LDA, X, LDA )
               SRNAMT = 'SGTTRS'
               CALL SGTTRS( TRANS, N, NRHS, AF, AF( N ), AF( 2*N ),
     $                      AF( 3*N-1 ), IWORK, X, LDA, INFO )
*
*              Check error code from SGTTRS.
*
               IF( INFO.NE.0 )
     $            CALL ALAERH( PATH, 'SGTTRS', INFO, 0, TRANS, N, N, -1,
     $                         -1, NRHS, IMAT, NFAIL, NERRS, NOUT )
*
               CALL SLACPY( 'Full', N, NRHS, B, LDA, WORK, LDA )
               CALL SGTT02( TRANS, N, NRHS, A, A( N ), A( 2*N ), X, LDA,
     $                      WORK, LDA, RWORK, RESULT( 2 ) )
*
*+    TEST 3
*              Check solution from generated exact solution.
*
               CALL SGET04( N, NRHS, X, LDA, XACT, LDA, RCONDC,
     $                      RESULT( 3 ) )
*
*+    TESTS 4, 5, and 6
*              Use iterative refinement to improve the solution.
*
               SRNAMT = 'SGTRFS'
               CALL SGTRFS( TRANS, N, NRHS, A, A( N ), A( 2*N ), AF,
     $                      AF( N ), AF( 2*N ), AF( 3*N-1 ), IWORK, B,
     $                      LDA, X, LDA, RWORK, RWORK( NRHS+1 ), WORK,
     $                      IWORK( N+1 ), INFO )
*
*              Check error code from SGTRFS.
*
               IF( INFO.NE.0 )
     $            CALL ALAERH( PATH, 'SGTRFS', INFO, 0, TRANS, N, N, -1,
     $                         -1, NRHS, IMAT, NFAIL, NERRS, NOUT )
*
               CALL SGET04( N, NRHS, X, LDA, XACT, LDA, RCONDC,
     $                      RESULT( 4 ) )
               CALL SGTT05( TRANS, N, NRHS, A, A( N ), A( 2*N ), B, LDA,
     $                      X, LDA, XACT, LDA, RWORK, RWORK( NRHS+1 ),
     $                      RESULT( 5 ) )
               NT = 6
   60          CONTINUE
*
*+    TEST 7
*              Estimate the reciprocal of the condition number of the
*              matrix.
*
               IF( ITRAN.LE.2 ) THEN
                  SRNAMT = 'SGTCON'
                  CALL SGTCON( NORM, N, AF, AF( N ), AF( 2*N ),
     $                         AF( 3*N-1 ), IWORK, ANORM, RCOND, WORK,
     $                         IWORK( N+1 ), INFO )
*
*                 Check error code from SGTCON.
*
                  IF( INFO.NE.0 )
     $               CALL ALAERH( PATH, 'SGTCON', INFO, 0, NORM, N, N,
     $                            -1, -1, -1, IMAT, NFAIL, NERRS, NOUT )
*
                  RESULT( 7 ) = SGET06( RCOND, RCONDC )
                  NT = 7
               END IF
*
*              Print information about the tests that did not pass the
*              threshold.
*
               DO 70 K = K1, NT
                  IF( RESULT( K ).GE.THRESH ) THEN
                     IF( NFAIL.EQ.0 .AND. NERRS.EQ.0 )
     $                  CALL ALAHD( NOUT, PATH )
                     IF( K.LT.7 ) THEN
                        WRITE( NOUT, FMT = 9998 )TRANS, N, IMAT, K,
     $                     RESULT( K )
                     ELSE
                        WRITE( NOUT, FMT = 9997 )NORM, N, IMAT, K,
     $                     RESULT( K )
                     END IF
                     NFAIL = NFAIL + 1
                  END IF
   70          CONTINUE
               NRUN = NRUN + NT - K1 + 1
   80       CONTINUE
   90    CONTINUE
  100 CONTINUE
*
*     Print a summary of the results.
*
      CALL ALASUM( PATH, NOUT, NFAIL, NRUN, NERRS )
*
 9999 FORMAT( ' N =', I5, ', type ', I2, ', test ', I2, ', ratio = ',
     $      G12.5 )
 9998 FORMAT( ' TRANS=''', A1, ''', N =', I5, ', type ', I2, ', test ',
     $      I2, ', ratio = ', G12.5 )
 9997 FORMAT( ' NORM =''', A1, ''', N =', I5, ', type ', I2, ', test ',
     $      I2, ', ratio = ', G12.5 )
      RETURN
*
*     End of SCHKGT
*
      END
