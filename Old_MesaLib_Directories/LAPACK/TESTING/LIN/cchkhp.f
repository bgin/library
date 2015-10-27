      SUBROUTINE CCHKHP( DOTYPE, NN, NVAL, NRHS, THRESH, TSTERR, NMAX,
     $                   A, AFAC, AINV, B, X, XACT, WORK, RWORK, IWORK,
     $                   NOUT )
*
*  -- LAPACK test routine (version 1.1) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
      LOGICAL            TSTERR
      INTEGER            NMAX, NN, NOUT, NRHS
      REAL               THRESH
*     ..
*     .. Array Arguments ..
      LOGICAL            DOTYPE( * )
      INTEGER            IWORK( * ), NVAL( * )
      REAL               RWORK( * )
      COMPLEX            A( * ), AFAC( * ), AINV( * ), B( * ),
     $                   WORK( * ), X( * ), XACT( * )
*     ..
*
*  Purpose
*  =======
*
*  CCHKHP tests CHPTRF, -TRI, -TRS, -RFS, and -CON
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
*  NRHS    (input) INTEGER
*          The number of right hand side vectors to be generated for
*          each linear system.
*
*  THRESH  (input) REAL
*          The threshold value for the test ratios.  A result is
*          included in the output file if RESULT >= THRESH.  To have
*          every test ratio printed, use THRESH = 0.
*
*  TSTERR  (input) LOGICAL
*          Flag that indicates whether error exits are to be tested.
*
*  NMAX    (input) INTEGER
*          The maximum value permitted for N, used in dimensioning the
*          work arrays.
*
*  A       (workspace) COMPLEX array, dimension
*                      (NMAX*(NMAX+1)/2)
*
*  AFAC    (workspace) COMPLEX array, dimension
*                      (NMAX*(NMAX+1)/2)
*
*  AINV    (workspace) COMPLEX array, dimension
*                      (NMAX*(NMAX+1)/2)
*
*  B       (workspace) COMPLEX array, dimension (NMAX*NRHS)
*
*  X       (workspace) COMPLEX array, dimension (NMAX*NRHS)
*
*  XACT    (workspace) COMPLEX array, dimension (NMAX*NRHS)
*
*  WORK    (workspace) COMPLEX array, dimension
*                      (NMAX*max(2,NRHS))
*
*  RWORK   (workspace) REAL array, dimension (NMAX+2*NRHS)
*
*  IWORK   (workspace) INTEGER array, dimension (NMAX)
*
*  NOUT    (input) INTEGER
*          The unit number for output.
*
*  =====================================================================
*
*     .. Parameters ..
      REAL               ZERO
      PARAMETER          ( ZERO = 0.0E+0 )
      INTEGER            NTYPES
      PARAMETER          ( NTYPES = 10 )
      INTEGER            NTESTS
      PARAMETER          ( NTESTS = 8 )
*     ..
*     .. Local Scalars ..
      LOGICAL            TRFCON, ZEROT
      CHARACTER          DIST, PACKIT, TYPE, UPLO, XTYPE
      CHARACTER*3        PATH
      INTEGER            I, I1, I2, IMAT, IN, INFO, IOFF, IUPLO, IZERO,
     $                   J, K, KL, KTEST, KU, LDA, MODE, N, NERRS,
     $                   NFAIL, NIMAT, NPP, NRUN, NT
      REAL               ANORM, CNDNUM, RCOND, RCONDC
*     ..
*     .. Local Arrays ..
      CHARACTER          UPLOS( 2 )
      INTEGER            ISEED( 4 ), ISEEDY( 4 )
      REAL               RESULT( NTESTS )
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      REAL               CLANSP, SGET06
      EXTERNAL           LSAME, CLANSP, SGET06
*     ..
*     .. External Subroutines ..
      EXTERNAL           ALAERH, ALAHD, ALASUM, CCOPY, CERRSY, CGET04,
     $                   CHPCON, CHPRFS, CHPT01, CHPTRF, CHPTRI, CHPTRS,
     $                   CLACPY, CLARHS, CLATB4, CLATMS, CPPT02, CPPT03,
     $                   CPPT05
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
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
      DATA               ISEEDY / 1988, 1989, 1990, 1991 /
      DATA               UPLOS / 'U', 'L' /
*     ..
*     .. Executable Statements ..
*
*     Initialize constants and the random number seed.
*
      PATH( 1: 1 ) = 'Complex precision'
      PATH( 2: 3 ) = 'HP'
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
     $   CALL CERRSY( PATH, NOUT )
      INFOT = 0
*
*     Do for each value of N in NVAL
*
      DO 150 IN = 1, NN
         N = NVAL( IN )
         LDA = MAX( N, 1 )
         XTYPE = 'N'
         NIMAT = NTYPES
         IF( N.LE.0 )
     $      NIMAT = 1
*
         IZERO = 0
         DO 140 IMAT = 1, NIMAT
*
*           Do the tests only if DOTYPE( IMAT ) is true.
*
            IF( .NOT.DOTYPE( IMAT ) )
     $         GO TO 140
*
*           Skip types 3, 4, 5, or 6 if the matrix size is too small.
*
            ZEROT = IMAT.GE.3 .AND. IMAT.LE.6
            IF( ZEROT .AND. N.LT.IMAT-2 )
     $         GO TO 140
*
*           Do first for UPLO = 'U', then for UPLO = 'L'
*
            DO 130 IUPLO = 1, 2
               UPLO = UPLOS( IUPLO )
               IF( LSAME( UPLO, 'U' ) ) THEN
                  PACKIT = 'C'
               ELSE
                  PACKIT = 'R'
               END IF
*
*              Set up parameters with CLATB4 and generate a test matrix
*              with CLATMS.
*
               CALL CLATB4( PATH, IMAT, N, N, TYPE, KL, KU, ANORM, MODE,
     $                      CNDNUM, DIST )
*
               SRNAMT = 'CLATMS'
               CALL CLATMS( N, N, DIST, ISEED, TYPE, RWORK, MODE,
     $                      CNDNUM, ANORM, KL, KU, PACKIT, A, LDA, WORK,
     $                      INFO )
*
*              Check error code from CLATMS.
*
               IF( INFO.NE.0 ) THEN
                  CALL ALAERH( PATH, 'CLATMS', INFO, 0, UPLO, N, N, -1,
     $                         -1, -1, IMAT, NFAIL, NERRS, NOUT )
                  GO TO 130
               END IF
*
*              For types 3-6, zero one or more rows and columns of
*              the matrix to test that INFO is returned correctly.
*
               IF( ZEROT ) THEN
                  IF( IMAT.EQ.3 ) THEN
                     IZERO = 1
                  ELSE IF( IMAT.EQ.4 ) THEN
                     IZERO = N
                  ELSE
                     IZERO = N / 2 + 1
                  END IF
*
                  IF( IMAT.LT.6 ) THEN
*
*                    Set row and column IZERO to zero.
*
                     IF( IUPLO.EQ.1 ) THEN
                        IOFF = ( IZERO-1 )*IZERO / 2
                        DO 20 I = 1, IZERO - 1
                           A( IOFF+I ) = ZERO
   20                   CONTINUE
                        IOFF = IOFF + IZERO
                        DO 30 I = IZERO, N
                           A( IOFF ) = ZERO
                           IOFF = IOFF + I
   30                   CONTINUE
                     ELSE
                        IOFF = IZERO
                        DO 40 I = 1, IZERO - 1
                           A( IOFF ) = ZERO
                           IOFF = IOFF + N - I
   40                   CONTINUE
                        IOFF = IOFF - IZERO
                        DO 50 I = IZERO, N
                           A( IOFF+I ) = ZERO
   50                   CONTINUE
                     END IF
                  ELSE
                     IOFF = 0
                     IF( IUPLO.EQ.1 ) THEN
*
*                       Set the first IZERO rows and columns to zero.
*
                        DO 70 J = 1, N
                           I2 = MIN( J, IZERO )
                           DO 60 I = 1, I2
                              A( IOFF+I ) = ZERO
   60                      CONTINUE
                           IOFF = IOFF + J
   70                   CONTINUE
                     ELSE
*
*                       Set the last IZERO rows and columns to zero.
*
                        DO 90 J = 1, N
                           I1 = MAX( J, IZERO )
                           DO 80 I = I1, N
                              A( IOFF+I ) = ZERO
   80                      CONTINUE
                           IOFF = IOFF + N - J
   90                   CONTINUE
                     END IF
                  END IF
               ELSE
                  IZERO = 0
               END IF
*
*              Compute the L*D*L' or U*D*U' factorization of the matrix.
*
               NPP = N*( N+1 ) / 2
               CALL CCOPY( NPP, A, 1, AFAC, 1 )
               SRNAMT = 'CHPTRF'
               CALL CHPTRF( UPLO, N, AFAC, IWORK, INFO )
*
*              Adjust the expected value of INFO to account for
*              pivoting.
*
               K = IZERO
               IF( K.GT.0 ) THEN
  100             CONTINUE
                  IF( IWORK( K ).LT.0 ) THEN
                     IF( IWORK( K ).NE.-K ) THEN
                        K = -IWORK( K )
                        GO TO 100
                     END IF
                  ELSE IF( IWORK( K ).NE.K ) THEN
                     K = IWORK( K )
                     GO TO 100
                  END IF
               END IF
*
*              Check error code from CHPTRF.
*
               IF( INFO.NE.K )
     $            CALL ALAERH( PATH, 'CHPTRF', INFO, K, UPLO, N, N, -1,
     $                         -1, -1, IMAT, NFAIL, NERRS, NOUT )
               TRFCON = .FALSE.
*
*+    TEST 1
*              Reconstruct matrix from factors and compute residual.
*
               CALL CHPT01( UPLO, N, A, AFAC, IWORK, AINV, LDA, RWORK,
     $                      RESULT( 1 ) )
               NT = 1
*
*              Do only the condition estimate if INFO is not 0.
*
               IF( INFO.NE.0 ) THEN
                  TRFCON = .TRUE.
                  RCONDC = ZERO
                  GO TO 110
               END IF
*
*+    TEST 2
*              Form the inverse and compute the residual.
*
               CALL CCOPY( NPP, AFAC, 1, AINV, 1 )
               SRNAMT = 'CHPTRI'
               CALL CHPTRI( UPLO, N, AINV, IWORK, WORK, INFO )
*
*              Check error code from CHPTRI.
*
               IF( INFO.NE.0 )
     $            CALL ALAERH( PATH, 'CHPTRI', INFO, 0, UPLO, N, N, -1,
     $                         -1, -1, IMAT, NFAIL, NERRS, NOUT )
*
               CALL CPPT03( UPLO, N, A, AINV, WORK, LDA, RWORK, RCONDC,
     $                      RESULT( 2 ) )
*
*+    TEST 3
*              Solve and compute residual for  A * X = B.
*
               SRNAMT = 'CLARHS'
               CALL CLARHS( PATH, XTYPE, UPLO, ' ', N, N, KL, KU, NRHS,
     $                      A, LDA, XACT, LDA, B, LDA, ISEED, INFO )
               XTYPE = 'C'
               CALL CLACPY( 'Full', N, NRHS, B, LDA, X, LDA )
*
               SRNAMT = 'CHPTRS'
               CALL CHPTRS( UPLO, N, NRHS, AFAC, IWORK, X, LDA, INFO )
*
*              Check error code from CHPTRS.
*
               IF( INFO.NE.0 )
     $            CALL ALAERH( PATH, 'CHPTRS', INFO, 0, UPLO, N, N, -1,
     $                         -1, NRHS, IMAT, NFAIL, NERRS, NOUT )
*
               CALL CLACPY( 'Full', N, NRHS, B, LDA, WORK, LDA )
               CALL CPPT02( UPLO, N, NRHS, A, X, LDA, WORK, LDA, RWORK,
     $                      RESULT( 3 ) )
*
*+    TEST 4
*              Check solution from generated exact solution.
*
               CALL CGET04( N, NRHS, X, LDA, XACT, LDA, RCONDC,
     $                      RESULT( 4 ) )
*
*+    TESTS 5, 6, and 7
*              Use iterative refinement to improve the solution.
*
               SRNAMT = 'CHPRFS'
               CALL CHPRFS( UPLO, N, NRHS, A, AFAC, IWORK, B, LDA, X,
     $                      LDA, RWORK, RWORK( NRHS+1 ), WORK,
     $                      RWORK( 2*NRHS+1 ), INFO )
*
*              Check error code from CHPRFS.
*
               IF( INFO.NE.0 )
     $            CALL ALAERH( PATH, 'CHPRFS', INFO, 0, UPLO, N, N, -1,
     $                         -1, NRHS, IMAT, NFAIL, NERRS, NOUT )
*
               CALL CGET04( N, NRHS, X, LDA, XACT, LDA, RCONDC,
     $                      RESULT( 5 ) )
               CALL CPPT05( UPLO, N, NRHS, A, B, LDA, X, LDA, XACT, LDA,
     $                      RWORK, RWORK( NRHS+1 ), RESULT( 6 ) )
               NT = 7
*
*+    TEST 8
*              Get an estimate of RCOND = 1/CNDNUM.
*
  110          CONTINUE
               ANORM = CLANSP( '1', UPLO, N, A, RWORK )
               SRNAMT = 'CHPCON'
               CALL CHPCON( UPLO, N, AFAC, IWORK, ANORM, RCOND, WORK,
     $                      INFO )
*
*              Check error code from CHPCON.
*
               IF( INFO.NE.0 )
     $            CALL ALAERH( PATH, 'CHPCON', INFO, 0, UPLO, N, N, -1,
     $                         -1, -1, IMAT, NFAIL, NERRS, NOUT )
*
               RESULT( 8 ) = SGET06( RCOND, RCONDC )
               NT = NT + 1
*
*              Print information about the tests that did not pass
*              the threshold.
*
               DO 120 K = 1, NT
                  KTEST = K
                  IF( TRFCON .AND. K.EQ.2 )
     $               KTEST = NTESTS
                  IF( RESULT( KTEST ).GE.THRESH ) THEN
                     IF( NFAIL.EQ.0 .AND. NERRS.EQ.0 )
     $                  CALL ALAHD( NOUT, PATH )
                     WRITE( NOUT, FMT = 9999 )UPLO, N, IMAT, KTEST,
     $                  RESULT( KTEST )
                     NFAIL = NFAIL + 1
                  END IF
  120          CONTINUE
               NRUN = NRUN + NT
*
  130       CONTINUE
  140    CONTINUE
  150 CONTINUE
*
*     Print a summary of the results.
*
      CALL ALASUM( PATH, NOUT, NFAIL, NRUN, NERRS )
*
 9999 FORMAT( ' UPLO = ''', A1, ''', N =', I5, ', type ', I2, ', test ',
     $      I2, ', ratio =', G12.5 )
      RETURN
*
*     End of CCHKHP
*
      END