      SUBROUTINE SCHKGB( DOTYPE, NM, MVAL, NN, NVAL, NNB, NBVAL, NRHS,
     $                   THRESH, TSTERR, A, LA, AFAC, LAFAC, B, X, XACT,
     $                   WORK, RWORK, IWORK, NOUT )
*
*  -- LAPACK test routine (version 1.1) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
      LOGICAL            TSTERR
      INTEGER            LA, LAFAC, NM, NN, NNB, NOUT, NRHS
      REAL               THRESH
*     ..
*     .. Array Arguments ..
      LOGICAL            DOTYPE( * )
      INTEGER            IWORK( * ), MVAL( * ), NBVAL( * ), NVAL( * )
      REAL               A( * ), AFAC( * ), B( * ), RWORK( * ),
     $                   WORK( * ), X( * ), XACT( * )
*     ..
*
*  Purpose
*  =======
*
*  SCHKGB tests SGBTRF, -TRS, -RFS, and -CON
*
*  Arguments
*  =========
*
*  DOTYPE  (input) LOGICAL array, dimension (NTYPES)
*          The matrix types to be used for testing.  Matrices of type j
*          (for 1 <= j <= NTYPES) are used for testing if DOTYPE(j) =
*          .TRUE.; if DOTYPE(j) = .FALSE., then type j is not used.
*
*  NM      (input) INTEGER
*          The number of values of M contained in the vector MVAL.
*
*  MVAL    (input) INTEGER array, dimension (NM)
*          The values of the matrix row dimension M.
*
*  NN      (input) INTEGER
*          The number of values of N contained in the vector NVAL.
*
*  NVAL    (input) INTEGER array, dimension (NN)
*          The values of the matrix column dimension N.
*
*  NNB     (input) INTEGER
*          The number of values of NB contained in the vector NBVAL.
*
*  NBVAL   (input) INTEGER array, dimension (NBVAL)
*          The values of the blocksize NB.
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
*  A       (workspace) REAL array, dimension (LA)
*
*  LA      (input) INTEGER
*          The length of the array A.  LA >= (KLMAX+KUMAX+1)*NMAX
*          where KLMAX is the largest entry in the local array KLVAL,
*                KUMAX is the largest entry in local array KUVAL and
*                NMAX is the largest entry in input array NVAL.
*
*  AFAC    (workspace) REAL array, dimension (LAFAC)
*
*  LAFAC   (input) INTEGER
*          The length of the array AFAC. LAFAC >= (2*KLMAX+KUMAX+1)*NMAX
*          where KLMAX is the largest entry in local array KLVAL,
*                KUMAX is the largest entry in local array KUVAL and
*                NMAX is the largest entry in input array NVAL.
*
*  B       (workspace) REAL array, dimension (NMAX*NRHS)
*
*  X       (workspace) REAL array, dimension (NMAX*NRHS)
*
*  XACT    (workspace) REAL array, dimension (NMAX*NRHS)
*
*  WORK    (workspace) REAL array, dimension
*                      (NMAX*max(3,NRHS,NMAX))
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
      INTEGER            NTYPES, NTESTS
      PARAMETER          ( NTYPES = 8, NTESTS = 7 )
      INTEGER            NBW, NTRAN
      PARAMETER          ( NBW = 4, NTRAN = 3 )
*     ..
*     .. Local Scalars ..
      LOGICAL            TRFCON, ZEROT
      CHARACTER          DIST, NORM, TRANS, TYPE, XTYPE
      CHARACTER*3        PATH
      INTEGER            I, I1, I2, IKL, IKU, IM, IMAT, IN, INB, INFO,
     $                   IOFF, ITRAN, IZERO, J, K, K1, KL, KOFF, KU,
     $                   LDA, LDAFAC, LDB, M, MODE, N, NB, NERRS, NFAIL,
     $                   NIMAT, NKL, NKU, NRUN, NT
      REAL               AINVNM, ANORM, ANORMI, ANORMO, CNDNUM, RCOND,
     $                   RCONDC, RCONDI, RCONDO
*     ..
*     .. Local Arrays ..
      CHARACTER          TRANSS( NTRAN )
      INTEGER            ISEED( 4 ), ISEEDY( 4 ), KLVAL( NBW ),
     $                   KUVAL( NBW )
      REAL               RESULT( NTESTS )
*     ..
*     .. External Functions ..
      REAL               SGET06, SLANGB, SLANGE
      EXTERNAL           SGET06, SLANGB, SLANGE
*     ..
*     .. External Subroutines ..
      EXTERNAL           ALAERH, ALAHD, ALASUM, SCOPY, SERRGE, SGBCON,
     $                   SGBRFS, SGBT01, SGBT02, SGBT05, SGBTRF, SGBTRS,
     $                   SGET04, SLACPY, SLARHS, SLASET, SLATB4, SLATMS,
     $                   XLAENV
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
      DATA               ISEEDY / 1988, 1989, 1990, 1991 / ,
     $                   TRANSS / 'N', 'T', 'C' /
*     ..
*     .. Executable Statements ..
*
*     Initialize constants and the random number seed.
*
      PATH( 1: 1 ) = 'Single precision'
      PATH( 2: 3 ) = 'GB'
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
      CALL XLAENV( 2, 2 )
*
*     Initialize the first value for the lower and upper bandwidths.
*
      KLVAL( 1 ) = 0
      KUVAL( 1 ) = 0
*
*     Do for each value of M in MVAL
*
      DO 130 IM = 1, NM
         M = MVAL( IM )
*
*        Set values to use for the lower bandwidth.
*
         KLVAL( 2 ) = M + ( M+1 ) / 4
*
*        KLVAL( 2 ) = MAX( M-1, 0 )
*
         KLVAL( 3 ) = ( 3*M-1 ) / 4
         KLVAL( 4 ) = ( M+1 ) / 4
*
*        Do for each value of N in NVAL
*
         DO 120 IN = 1, NN
            N = NVAL( IN )
            XTYPE = 'N'
*
*           Set values to use for the upper bandwidth.
*
            KUVAL( 2 ) = N + ( N+1 ) / 4
*
*           KUVAL( 2 ) = MAX( N-1, 0 )
*
            KUVAL( 3 ) = ( 3*N-1 ) / 4
            KUVAL( 4 ) = ( N+1 ) / 4
*
*           Set limits on the number of loop iterations.
*
            NKL = MIN( M+1, 4 )
            IF( N.EQ.0 )
     $         NKL = 2
            NKU = MIN( N+1, 4 )
            IF( M.EQ.0 )
     $         NKU = 2
            NIMAT = NTYPES
            IF( M.LE.0 .OR. N.LE.0 )
     $         NIMAT = 1
*
            DO 110 IKL = 1, NKL
*
*              Do for KL = 0, (5*M+1)/4, (3M-1)/4, and (M+1)/4. This
*              order makes it easier to skip redundant values for small
*              values of M.
*
               KL = KLVAL( IKL )
               DO 100 IKU = 1, NKU
*
*                 Do for KU = 0, (5*N+1)/4, (3N-1)/4, and (N+1)/4. This
*                 order makes it easier to skip redundant values for
*                 small values of N.
*
                  KU = KUVAL( IKU )
*
*                 Check that A and AFAC are big enough to generate this
*                 matrix.
*
                  LDA = KL + KU + 1
                  LDAFAC = 2*KL + KU + 1
                  IF( ( LDA*N ).GT.LA .OR. ( LDAFAC*N ).GT.LAFAC ) THEN
                     IF( NFAIL.EQ.0 .AND. NERRS.EQ.0 )
     $                  CALL ALAHD( NOUT, PATH )
                     IF( N*( KL+KU+1 ).GT.LA ) THEN
                        WRITE( NOUT, FMT = 9999 )LA, M, N, KL, KU,
     $                     N*( KL+KU+1 )
                        NERRS = NERRS + 1
                     END IF
                     IF( N*( 2*KL+KU+1 ).GT.LAFAC ) THEN
                        WRITE( NOUT, FMT = 9998 )LAFAC, M, N, KL, KU,
     $                     N*( 2*KL+KU+1 )
                        NERRS = NERRS + 1
                     END IF
                     GO TO 100
                  END IF
*
                  DO 90 IMAT = 1, NIMAT
*
*                    Do the tests only if DOTYPE( IMAT ) is true.
*
                     IF( .NOT.DOTYPE( IMAT ) )
     $                  GO TO 90
*
*                    Skip types 2, 3, or 4 if the matrix size is too
*                    small.
*
                     ZEROT = IMAT.GE.2 .AND. IMAT.LE.4
                     IF( ZEROT .AND. N.LT.IMAT-1 )
     $                  GO TO 90
*
                     IF( .NOT.ZEROT .OR. .NOT.DOTYPE( 1 ) ) THEN
*
*                       Set up parameters with SLATB4 and generate a
*                       test matrix with SLATMS.
*
                        CALL SLATB4( PATH, IMAT, M, N, TYPE, KL, KU,
     $                               ANORM, MODE, CNDNUM, DIST )
*
                        KOFF = MAX( 1, KU+2-N )
                        DO 20 I = 1, KOFF - 1
                           A( I ) = ZERO
   20                   CONTINUE
                        SRNAMT = 'SLATMS'
                        CALL SLATMS( M, N, DIST, ISEED, TYPE, RWORK,
     $                               MODE, CNDNUM, ANORM, KL, KU, 'Z',
     $                               A( KOFF ), LDA, WORK, INFO )
*
*                       Check the error code from SLATMS.
*
                        IF( INFO.NE.0 ) THEN
                           CALL ALAERH( PATH, 'SLATMS', INFO, 0, ' ', M,
     $                                  N, KL, KU, -1, IMAT, NFAIL,
     $                                  NERRS, NOUT )
                           GO TO 90
                        END IF
                     ELSE IF( IZERO.GT.0 ) THEN
*
*                       Use the same matrix for types 3 and 4 as for
*                       type 2 by copying back the zeroed out column.
*
                        CALL SCOPY( I2-I1+1, B, 1, A( IOFF+I1 ), 1 )
                     END IF
*
*                    For types 2, 3, and 4, zero one or more columns of
*                    the matrix to test that INFO is returned correctly.
*
                     IZERO = 0
                     IF( ZEROT ) THEN
                        IF( IMAT.EQ.2 ) THEN
                           IZERO = 1
                        ELSE IF( IMAT.EQ.3 ) THEN
                           IZERO = MIN( M, N )
                        ELSE
                           IZERO = MIN( M, N ) / 2 + 1
                        END IF
                        IOFF = ( IZERO-1 )*LDA
                        IF( IMAT.LT.4 ) THEN
*
*                          Store the column to be zeroed out in B.
*
                           I1 = MAX( 1, KU+2-IZERO )
                           I2 = MIN( KL+KU+1, KU+1+( M-IZERO ) )
                           CALL SCOPY( I2-I1+1, A( IOFF+I1 ), 1, B, 1 )
*
                           DO 30 I = I1, I2
                              A( IOFF+I ) = ZERO
   30                      CONTINUE
                        ELSE
                           DO 50 J = IZERO, N
                              DO 40 I = MAX( 1, KU+2-J ),
     $                                MIN( KL+KU+1, KU+1+( M-J ) )
                                 A( IOFF+I ) = ZERO
   40                         CONTINUE
                              IOFF = IOFF + LDA
   50                      CONTINUE
                        END IF
                     END IF
*
*                    These lines, if used in place of the calls in the
*                    loop over INB, cause the code to bomb on a Sun
*                    SPARCstation.
*
*                     ANORMO = SLANGB( 'O', N, KL, KU, A, LDA, RWORK )
*                     ANORMI = SLANGB( 'I', N, KL, KU, A, LDA, RWORK )
*
*                    Do for each blocksize in NBVAL
*
                     DO 80 INB = 1, NNB
                        NB = NBVAL( INB )
                        CALL XLAENV( 1, NB )
*
*                       Compute the LU factorization of the band matrix.
*
                        IF( M.GT.0 .AND. N.GT.0 )
     $                     CALL SLACPY( 'Full', KL+KU+1, N, A, LDA,
     $                                  AFAC( KL+1 ), LDAFAC )
                        SRNAMT = 'SGBTRF'
                        CALL SGBTRF( M, N, KL, KU, AFAC, LDAFAC, IWORK,
     $                               INFO )
*
*                       Check error code from SGBTRF.
*
                        IF( INFO.NE.IZERO )
     $                     CALL ALAERH( PATH, 'SGBTRF', INFO, IZERO,
     $                                  ' ', M, N, KL, KU, NB, IMAT,
     $                                  NFAIL, NERRS, NOUT )
                        TRFCON = .FALSE.
*
*+    TEST 1
*                       Reconstruct matrix from factors and compute
*                       residual.
*
                        CALL SGBT01( M, N, KL, KU, A, LDA, AFAC, LDAFAC,
     $                               IWORK, WORK, RESULT( 1 ) )
                        NT = 1
*
*                       Print information about the tests so far that
*                       did not pass the threshold.
*
                        IF( RESULT( 1 ).GE.THRESH ) THEN
                           IF( NFAIL.EQ.0 .AND. NERRS.EQ.0 )
     $                        CALL ALAHD( NOUT, PATH )
                           WRITE( NOUT, FMT = 9997 )M, N, KL, KU, NB,
     $                        IMAT, 1, RESULT( 1 )
                           NFAIL = NFAIL + 1
                        END IF
                        NRUN = NRUN + 1
*
*                       Skip the remaining tests if this is not the
*                       first block size or if M .ne. N.
*
                        IF( INB.GT.1 .OR. M.NE.N )
     $                     GO TO 80
*
                        ANORMO = SLANGB( 'O', N, KL, KU, A, LDA, RWORK )
                        ANORMI = SLANGB( 'I', N, KL, KU, A, LDA, RWORK )
*
                        IF( INFO.EQ.0 ) THEN
*
*                          Form the inverse of A so we can get a good
*                          estimate of CNDNUM = norm(A) * norm(inv(A)).
*
                           LDB = MAX( 1, N )
                           CALL SLASET( 'Full', N, N, ZERO, ONE, WORK,
     $                                  LDB )
                           SRNAMT = 'SGBTRS'
                           CALL SGBTRS( 'No transpose', N, KL, KU, N,
     $                                  AFAC, LDAFAC, IWORK, WORK, LDB,
     $                                  INFO )
*
*                          Compute the 1-norm condition number of A.
*
                           AINVNM = SLANGE( 'O', N, N, WORK, LDB,
     $                              RWORK )
                           IF( ANORMO.LE.ZERO .OR. AINVNM.LE.ZERO ) THEN
                              RCONDO = ONE
                           ELSE
                              RCONDO = ( ONE / ANORMO ) / AINVNM
                           END IF
*
*                          Compute the infinity-norm condition number of
*                          A.
*
                           AINVNM = SLANGE( 'I', N, N, WORK, LDB,
     $                              RWORK )
                           IF( ANORMI.LE.ZERO .OR. AINVNM.LE.ZERO ) THEN
                              RCONDI = ONE
                           ELSE
                              RCONDI = ( ONE / ANORMI ) / AINVNM
                           END IF
                        ELSE
*
*                          Do only the condition estimate if INFO.NE.0.
*
                           TRFCON = .TRUE.
                           RCONDO = ZERO
                           RCONDI = ZERO
                        END IF
*
                        DO 70 ITRAN = 1, NTRAN
                           TRANS = TRANSS( ITRAN )
                           IF( ITRAN.EQ.1 ) THEN
                              ANORM = ANORMO
                              RCONDC = RCONDO
                              NORM = 'O'
                           ELSE
                              ANORM = ANORMI
                              RCONDC = RCONDI
                              NORM = 'I'
                           END IF
                           IF( TRFCON ) THEN
                              K1 = 7
                           ELSE
                              K1 = 2
*
*+    TEST 2:
*                             Solve and compute residual for A * X = B.
*
                              SRNAMT = 'SLARHS'
                              CALL SLARHS( PATH, XTYPE, ' ', TRANS, N,
     $                                     N, KL, KU, NRHS, A, LDA,
     $                                     XACT, LDB, B, LDB, ISEED,
     $                                     INFO )
                              XTYPE = 'C'
                              CALL SLACPY( 'Full', N, NRHS, B, LDB, X,
     $                                     LDB )
*
                              SRNAMT = 'SGBTRS'
                              CALL SGBTRS( TRANS, N, KL, KU, NRHS, AFAC,
     $                                     LDAFAC, IWORK, X, LDB, INFO )
*
*                             Check error code from SGBTRS.
*
                              IF( INFO.NE.0 )
     $                           CALL ALAERH( PATH, 'SGBTRS', INFO, 0,
     $                                        TRANS, N, N, KL, KU, -1,
     $                                        IMAT, NFAIL, NERRS, NOUT )
*
                              CALL SLACPY( 'Full', N, NRHS, B, LDB,
     $                                     WORK, LDB )
                              CALL SGBT02( TRANS, M, N, KL, KU, NRHS, A,
     $                                     LDA, X, LDB, WORK, LDB,
     $                                     RESULT( 2 ) )
*
*+    TEST 3:
*                             Check solution from generated exact
*                             solution.
*
                              CALL SGET04( N, NRHS, X, LDB, XACT, LDB,
     $                                     RCONDC, RESULT( 3 ) )
*
*+    TESTS 4, 5, 6:
*                             Use iterative refinement to improve the
*                             solution.
*
                              SRNAMT = 'SGBRFS'
                              CALL SGBRFS( TRANS, N, KL, KU, NRHS, A,
     $                                     LDA, AFAC, LDAFAC, IWORK, B,
     $                                     LDB, X, LDB, RWORK,
     $                                     RWORK( NRHS+1 ), WORK,
     $                                     IWORK( N+1 ), INFO )
*
*                             Check error code from SGBRFS.
*
                              IF( INFO.NE.0 )
     $                           CALL ALAERH( PATH, 'SGBRFS', INFO, 0,
     $                                        TRANS, N, N, KL, KU, NRHS,
     $                                        IMAT, NFAIL, NERRS, NOUT )
*
                              CALL SGET04( N, NRHS, X, LDB, XACT, LDB,
     $                                     RCONDC, RESULT( 4 ) )
                              CALL SGBT05( TRANS, N, KL, KU, NRHS, A,
     $                                     LDA, B, LDB, X, LDB, XACT,
     $                                     LDB, RWORK, RWORK( NRHS+1 ),
     $                                     RESULT( 5 ) )
                              NT = 6
                           END IF
*
*+    TEST 7:
*                          Get an estimate of RCOND = 1/CNDNUM.
*
                           IF( ITRAN.LE.2 ) THEN
                              SRNAMT = 'SGBCON'
                              CALL SGBCON( NORM, N, KL, KU, AFAC,
     $                                     LDAFAC, IWORK, ANORM, RCOND,
     $                                     WORK, IWORK( N+1 ), INFO )
*
*                             Check error code from SGBCON.
*
                              IF( INFO.NE.0 )
     $                           CALL ALAERH( PATH, 'SGBCON', INFO, 0,
     $                                        NORM, N, N, KL, KU, -1,
     $                                        IMAT, NFAIL, NERRS, NOUT )
*
                              RESULT( 7 ) = SGET06( RCOND, RCONDC )
                              NT = 7
                           END IF
*
*                          Print information about the tests that did
*                          not pass the threshold.
*
                           DO 60 K = K1, NT
                              IF( RESULT( K ).GE.THRESH ) THEN
                                 IF( NFAIL.EQ.0 .AND. NERRS.EQ.0 )
     $                              CALL ALAHD( NOUT, PATH )
                                 IF( K.LT.7 ) THEN
                                    WRITE( NOUT, FMT = 9996 )TRANS, N,
     $                                 KL, KU, IMAT, K, RESULT( K )
                                 ELSE
                                    WRITE( NOUT, FMT = 9995 )NORM, N,
     $                                 KL, KU, IMAT, K, RESULT( K )
                                 END IF
                                 NFAIL = NFAIL + 1
                              END IF
   60                      CONTINUE
                           NRUN = NRUN + NT - K1 + 1
   70                   CONTINUE
   80                CONTINUE
*
   90             CONTINUE
  100          CONTINUE
  110       CONTINUE
  120    CONTINUE
  130 CONTINUE
*
*     Print a summary of the results.
*
      CALL ALASUM( PATH, NOUT, NFAIL, NRUN, NERRS )
*
 9999 FORMAT( ' *** In SCHKGB, LA=', I5, ' is too small for M=', I5,
     $      ', N=', I5, ', KL=', I4, ', KU=', I4,
     $      / ' ==> Increase LA to at least ', I5 )
 9998 FORMAT( ' *** In SCHKGB, LAFAC=', I5, ' is too small for M=', I5,
     $      ', N=', I5, ', KL=', I4, ', KU=', I4,
     $      / ' ==> Increase LAFAC to at least ', I5 )
 9997 FORMAT( ' M=', I5, ',N=', I5, ',KL=', I5, ',KU=', I5, ', NB=', I4,
     $      ', type ', I1, ', test ', I1, ', ratio=', G12.5 )
 9996 FORMAT( ' TRANS= ''', A1, ''', N =', I5, ', KL =', I5, ', KU =',
     $      I5, ', type ', I1, ', test ', I1, ', ratio=', G12.5 )
 9995 FORMAT( ' NORM = ''', A1, ''', N =', I5, ', KL =', I5, ', KU =',
     $      I5, ', type ', I1, ', test ', I1, ', ratio=', G12.5 )
*
      RETURN
*
*     End of SCHKGB
*
      END
