      SUBROUTINE CHGEQZ( JOB, COMPQ, COMPZ, N, ILO, IHI, A, LDA, B, LDB,
     $                   ALPHA, BETA, Q, LDQ, Z, LDZ, WORK, LWORK,
     $                   RWORK, INFO )
*
*  -- LAPACK routine (instrumented to count operations, version 1.1) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     March 31, 1993
*
*     .. Scalar Arguments ..
      CHARACTER          COMPQ, COMPZ, JOB
      INTEGER            IHI, ILO, INFO, LDA, LDB, LDQ, LDZ, LWORK, N
*     ..
*     .. Array Arguments ..
      REAL               RWORK( * )
      COMPLEX            A( LDA, * ), ALPHA( * ), B( LDB, * ),
     $                   BETA( * ), Q( LDQ, * ), WORK( * ), Z( LDZ, * )
*     ..
*
*     ----------------------- Begin Timing Code ------------------------
*     Common block to return operation count and iteration count
*     ITCNT is initialized to 0, OPS is only incremented
*     OPST is used to accumulate small contributions to OPS
*     to avoid roundoff error
*     .. Common blocks ..
      COMMON             / LATIME / OPS, ITCNT
*     ..
*     .. Scalars in Common ..
      REAL               ITCNT, OPS
*     ..
*     ------------------------ End Timing Code -------------------------
*
*
*  Purpose
*  =======
*
*  CHGEQZ implements a single-shift version of the QZ
*  method for finding the generalized eigenvalues w(i)=ALPHA(i)/BETA(i)
*  of the equation
*
*       det( A - w(i) B ) = 0
*
*  If JOB='S', then the pair (A,B) is simultaneously
*  reduced to Schur form (i.e., A and B are both upper triangular)
*  using one unitary tranformation (usually called Q) on the left and
*  another (usually called Z) on the right.  The diagonal entries of
*  A are then ALPHA(1),...,ALPHA(N), and of B are BETA(1),...,BETA(N).
*
*  If JOB='S' and COMPQ and COMPZ are 'V' or 'I', then the unitary
*  transformations used to reduce (A,B) are accumulated into the arrays
*  Q and Z s.t.:
*
*       Q(in) A(in) Z(in)* = Q(out) A(out) Z(out)*
*       Q(in) B(in) Z(in)* = Q(out) B(out) Z(out)*
*
*  Ref: C.B. Moler & G.W. Stewart, "An Algorithm for Generalized Matrix
*       Eigenvalue Problems", SIAM J. Numer. Anal., 10(1973),
*       pp. 241--256.
*
*  Arguments
*  =========
*
*  JOB     (input) CHARACTER*1
*          = 'E': compute only ALPHA and BETA.  A and B will not
*                 necessarily be put into generalized Schur form.
*          = 'S': put A and B into generalized Schur form, as well
*                 as computing ALPHA and BETA.
*
*  COMPQ   (input) CHARACTER*1
*          = 'N': do not modify Q.
*          = 'V': multiply the array Q on the right by the conjugate
*                 transpose of the unitary tranformation that is
*                 applied to the left side of A and B to reduce them
*                 to Schur form.
*          = 'I': like COMPQ='V', except that Q will be initialized to
*                 the identity first.
*
*  COMPZ   (input) CHARACTER*1
*          = 'N': do not modify Z.
*          = 'V': multiply the array Z on the right by the unitary
*                 tranformation that is applied to the right side of
*                 A and B to reduce them to Schur form.
*          = 'I': like COMPZ='V', except that Z will be initialized to
*                 the identity first.
*
*  N       (input) INTEGER
*          The number of rows and columns in the matrices A, B, Q, and
*          Z.  N must be at least 0.
*
*  ILO     (input) INTEGER
*          Columns 1 through ILO-1 are assumed to be in triangular form
*          already, and will not be modified.  ILO must be at least
*          1.
*
*  IHI     (input) INTEGER
*          Rows IHI+1 through N are assumed to be in triangular form
*          already, and will not be touched.  IHI may not be greater
*          than N.
*
*  A       (input/output) COMPLEX array, dimension (LDA, N)
*          On entry, the N x N upper Hessenberg matrix A.  Entries below
*          the subdiagonal must be zero.
*          If JOB='S', then on exit A and B will have been
*             simultaneously reduced to upper triangular form.
*          If JOB='E', then on exit A will have been destroyed.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max( 1, N ).
*
*  B       (input/output) COMPLEX array, dimension (LDB, N)
*          On entry, the N x N upper triangular matrix B.  Entries below
*          the diagonal must be zero.
*          If JOB='S', then on exit A and B will have been
*             simultaneously reduced to upper triangular form.
*          If JOB='E', then on exit B will have been destroyed.
*
*  LDB     (input) INTEGER
*          The leading dimension of the array B.  LDB >= max( 1, N ).
*
*  ALPHA   (output) COMPLEX array, dimension (N)
*          The diagonal entries of A when the pair (A,B) has been
*          reduced to Schur form.  ALPHA(i)/BETA(i) i=1,...,N
*          are the generalized eigenvalues.
*
*  BETA    (output) COMPLEX array, dimension (N)
*          The diagonal entries of B when the pair (A,B) has been
*          reduced to Schur form.  ALPHA(i)/BETA(i) i=1,...,N
*          are the generalized eigenvalues.  A and B are normalized
*          so that BETA(1),...,BETA(N) are non-negative real numbers.
*
*  Q       (input/output) COMPLEX array, dimension (LDQ, N)
*          If COMPQ='N', then Q will not be referenced.
*          If COMPQ='V' or 'I', then the conjugate transpose of the
*             unitary transformations which are applied to A and B on
*             the left will be applied to the array Q on the right.
*
*  LDQ     (input) INTEGER
*          The leading dimension of the array Q.  LDQ must be at least
*          1.  If COMPQ='V' or 'I', then LDQ must also be at least N.
*
*  Z       (input/output) COMPLEX array, dimension (LDZ, N)
*          If COMPZ='N', then Z will not be referenced.
*          If COMPZ='V' or 'I', then the unitary transformations which
*             are applied to A and B on the right will be applied to the
*             array Z on the right.
*
*  LDZ     (input) INTEGER
*          The leading dimension of the array Z.  LDZ must be at least
*          1.  If COMPZ='V' or 'I', then LDZ must also be at least N.
*
*  WORK    (workspace) COMPLEX array, dimension (LWORK)
*          On exit, if INFO is not negative, WORK(1) will be set to the
*          optimal size of the array WORK.
*
*  LWORK   (input) INTEGER
*          The number of elements in WORK.
*          It must be at least 1.
*
*  RWORK   (workspace) REAL array, dimension (N)
*
*  INFO    (output) INTEGER
*          < 0: if INFO = -i, the i-th argument had an illegal value
*          = 0: normal return.
*          = 1,...,N: the QZ iteration did not converge.  (A,B) is not
*                     in Schur form, but ALPHA(i) and BETA(i),
*                     i=INFO+1,...,N should be correct.
*          = N+1,...,2*N: the shift calculation failed.  (A,B) is not
*                     in Schur form, but ALPHA(i) and BETA(i),
*                     i=INFO-N+1,...,N should be correct.
*          > 2*N:     various "impossible" errors.
*
*  Further Details
*  ===============
*
*  We assume that complex ABS works as long as its value is less than
*  overflow.
*
*  =====================================================================
*
*     .. Parameters ..
      COMPLEX            CZERO, CONE
      PARAMETER          ( CZERO = 0.0E+0, CONE = 1.0E+0 )
      REAL               ZERO, ONE
      PARAMETER          ( ZERO = 0.0E+0, ONE = 1.0E+0 )
      REAL               HALF
      PARAMETER          ( HALF = 0.5E+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            ILAZR2, ILAZRO, ILQ, ILSCHR, ILZ
      INTEGER            ICOMPQ, ICOMPZ, IFIRST, IFRSTM, IITER, ILAST,
     $                   ILASTM, IN, ISCHUR, ISTART, J, JC, JCH, JITER,
     $                   JR, MAXIT, NQ, NZ
      REAL               ABSB, ANORM, ASCALE, ATOL, BNORM, BSCALE, BTOL,
     $                   C, OPST, SAFMIN, TEMP, ULP
      COMPLEX            ABI22, AD11, AD12, AD21, AD22, CTEMP, CTEMP2,
     $                   CTEMP3, ESHIFT, RTDISC, S, SHIFT, SIGNBC, T,
     $                   U12, X
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      REAL               CLANHS, SLAMCH
      EXTERNAL           LSAME, CLANHS, SLAMCH
*     ..
*     .. External Subroutines ..
      EXTERNAL           CLARTG, CLAZRO, CROT, CSCAL, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, AIMAG, CMPLX, CONJG, MAX, MIN, REAL, SQRT
*     ..
*     .. Statement Functions ..
      REAL               ABS1
*     ..
*     .. Statement Function definitions ..
      ABS1( X ) = ABS( REAL( X ) ) + ABS( AIMAG( X ) )
*     ..
*     .. Executable Statements ..
*     ----------------------- Begin Timing Code ------------------------
      ITCNT = ZERO
*     ------------------------ End Timing Code -------------------------
*
*     Decode JOB, COMPQ, COMPZ
*
      IF( LSAME( JOB, 'E' ) ) THEN
         ILSCHR = .FALSE.
         ISCHUR = 1
      ELSE IF( LSAME( JOB, 'S' ) ) THEN
         ILSCHR = .TRUE.
         ISCHUR = 2
      ELSE
         ISCHUR = 0
      END IF
*
      IF( LSAME( COMPQ, 'N' ) ) THEN
         ILQ = .FALSE.
         ICOMPQ = 1
         NQ = 0
      ELSE IF( LSAME( COMPQ, 'V' ) ) THEN
         ILQ = .TRUE.
         ICOMPQ = 2
         NQ = N
      ELSE IF( LSAME( COMPQ, 'I' ) ) THEN
         ILQ = .TRUE.
         ICOMPQ = 3
         NQ = N
      ELSE
         ICOMPQ = 0
      END IF
*
      IF( LSAME( COMPZ, 'N' ) ) THEN
         ILZ = .FALSE.
         ICOMPZ = 1
         NZ = 0
      ELSE IF( LSAME( COMPZ, 'V' ) ) THEN
         ILZ = .TRUE.
         ICOMPZ = 2
         NZ = N
      ELSE IF( LSAME( COMPZ, 'I' ) ) THEN
         ILZ = .TRUE.
         ICOMPZ = 3
         NZ = N
      ELSE
         ICOMPZ = 0
      END IF
*
*     Check Argument Values
*
      INFO = 0
      IF( ISCHUR.EQ.0 ) THEN
         INFO = -1
      ELSE IF( ICOMPQ.EQ.0 ) THEN
         INFO = -2
      ELSE IF( ICOMPZ.EQ.0 ) THEN
         INFO = -3
      ELSE IF( N.LT.0 ) THEN
         INFO = -4
      ELSE IF( ILO.LT.1 ) THEN
         INFO = -5
      ELSE IF( IHI.GT.N .OR. IHI.LT.ILO-1 ) THEN
         INFO = -6
      ELSE IF( LDA.LT.N ) THEN
         INFO = -8
      ELSE IF( LDB.LT.N ) THEN
         INFO = -10
      ELSE IF( LDQ.LT.1 .OR. ( ILQ .AND. LDQ.LT.N ) ) THEN
         INFO = -14
      ELSE IF( LDZ.LT.1 .OR. ( ILZ .AND. LDZ.LT.N ) ) THEN
         INFO = -16
      ELSE IF( LWORK.LT.MAX( 1, N ) ) THEN
         INFO = -18
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'CHGEQZ', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      WORK( 1 ) = CMPLX( 1 )
      IF( N.LE.0 ) THEN
         RETURN
      END IF
*
*     Initialize Q and Z
*
      IF( ICOMPQ.EQ.3 )
     $   CALL CLAZRO( N, N, CZERO, CONE, Q, LDQ )
      IF( ICOMPZ.EQ.3 )
     $   CALL CLAZRO( N, N, CZERO, CONE, Z, LDZ )
*
*     Machine Constants
*
      IN = IHI + 1 - ILO
      SAFMIN = SLAMCH( 'S' )
      ULP = SLAMCH( 'E' )*SLAMCH( 'B' )
      ANORM = CLANHS( 'F', IN, A( ILO, ILO ), LDA, RWORK )
      BNORM = CLANHS( 'F', IN, B( ILO, ILO ), LDB, RWORK )
      ATOL = MAX( SAFMIN, ULP*ANORM )
      BTOL = MAX( SAFMIN, ULP*BNORM )
      ASCALE = ONE / MAX( SAFMIN, ANORM )
      BSCALE = ONE / MAX( SAFMIN, BNORM )
*
*     ---------------------- Begin Timing Code -------------------------
*     Count ops for norms, etc.
      OPST = ZERO
      OPS = OPS + REAL( 4*N**2+12*N-5 )
*     ----------------------- End Timing Code --------------------------
*
*
*
*     Set Eigenvalues IHI+1:N
*
      DO 10 J = IHI + 1, N
         ABSB = ABS( B( J, J ) )
         IF( ABSB.GT.SAFMIN ) THEN
            SIGNBC = CONJG( B( J, J ) / ABSB )
            B( J, J ) = ABSB
            IF( ILSCHR ) THEN
               CALL CSCAL( J-1, SIGNBC, B( 1, J ), 1 )
               CALL CSCAL( J, SIGNBC, A( 1, J ), 1 )
*              ----------------- Begin Timing Code ---------------------
               OPST = OPST + REAL( 12*( J-1 ) )
*              ------------------ End Timing Code ----------------------
            ELSE
               A( J, J ) = A( J, J )*SIGNBC
            END IF
            IF( ILZ )
     $         CALL CSCAL( N, SIGNBC, Z( 1, J ), 1 )
*           ------------------- Begin Timing Code ----------------------
            OPST = OPST + REAL( 6*NZ+13 )
*           -------------------- End Timing Code -----------------------
         ELSE
            B( J, J ) = CZERO
         END IF
         ALPHA( J ) = A( J, J )
         BETA( J ) = B( J, J )
   10 CONTINUE
*
*     If IHI < ILO, skip QZ steps
*
      IF( IHI.LT.ILO )
     $   GO TO 190
*
*     MAIN QZ ITERATION LOOP
*
*     Initialize dynamic indices
*
*     Eigenvalues ILAST+1:N have been found.
*        Column operations modify rows IFRSTM:whatever
*        Row operations modify columns whatever:ILASTM
*
*     If only eigenvalues are being computed, then
*        IFRSTM is the row of the last splitting row above row ILAST;
*        this is always at least ILO.
*     IITER counts iterations since the last eigenvalue was found,
*        to tell when to use an extraordinary shift.
*     MAXIT is the maximum number of QZ sweeps allowed.
*
      ILAST = IHI
      IF( ILSCHR ) THEN
         IFRSTM = 1
         ILASTM = N
      ELSE
         IFRSTM = ILO
         ILASTM = IHI
      END IF
      IITER = 0
      ESHIFT = CZERO
      MAXIT = 30*( IHI-ILO+1 )
*
      DO 170 JITER = 1, MAXIT
*
*        Check for too many iterations.
*
         IF( JITER.GT.MAXIT )
     $      GO TO 180
*
*        Split the matrix if possible.
*
*        Two tests:
*           1: A(j,j-1)=0  or  j=ILO
*           2: B(j,j)=0
*
*        Special case: j=ILAST
*
         IF( ILAST.EQ.ILO ) THEN
            GO TO 60
         ELSE
            IF( ABS1( A( ILAST, ILAST-1 ) ).LE.ATOL ) THEN
               A( ILAST, ILAST-1 ) = CZERO
               GO TO 60
            END IF
         END IF
*
         IF( ABS( B( ILAST, ILAST ) ).LE.BTOL ) THEN
            B( ILAST, ILAST ) = CZERO
            GO TO 50
         END IF
*
*        General case: j<ILAST
*
         DO 40 J = ILAST - 1, ILO, -1
*
*           Test 1: for A(j,j-1)=0 or j=ILO
*
            IF( J.EQ.ILO ) THEN
               ILAZRO = .TRUE.
            ELSE
               IF( ABS1( A( J, J-1 ) ).LE.ATOL ) THEN
                  A( J, J-1 ) = CZERO
                  ILAZRO = .TRUE.
               ELSE
                  ILAZRO = .FALSE.
               END IF
            END IF
*
*           Test 2: for B(j,j)=0
*
            IF( ABS( B( J, J ) ).LT.BTOL ) THEN
               B( J, J ) = CZERO
*
*              Test 1a: Check for 2 consecutive small subdiagonals in A
*
               ILAZR2 = .FALSE.
               IF( .NOT.ILAZRO ) THEN
                  IF( ABS1( A( J, J-1 ) )*( ASCALE*ABS1( A( J+1,
     $                J ) ) ).LE.ABS1( A( J, J ) )*( ASCALE*ATOL ) )
     $                ILAZR2 = .TRUE.
               END IF
*
*              If both tests pass (1 & 2), i.e., the leading diagonal
*              entry of B in the block is zero, split a 1x1 block off
*              at the top. (I.e., at the J-th row/column) The leading
*              diagonal entry of the remainder can also be zero, so
*              this may have to be done repeatedly.
*
               IF( ILAZRO .OR. ILAZR2 ) THEN
                  DO 20 JCH = J, ILAST - 1
                     CTEMP = A( JCH, JCH )
                     CALL CLARTG( CTEMP, A( JCH+1, JCH ), C, S,
     $                            A( JCH, JCH ) )
                     A( JCH+1, JCH ) = CZERO
                     CALL CROT( ILASTM-JCH, A( JCH, JCH+1 ), LDA,
     $                          A( JCH+1, JCH+1 ), LDA, C, S )
                     CALL CROT( ILASTM-JCH, B( JCH, JCH+1 ), LDB,
     $                          B( JCH+1, JCH+1 ), LDB, C, S )
                     IF( ILQ )
     $                  CALL CROT( N, Q( 1, JCH ), 1, Q( 1, JCH+1 ), 1,
     $                             C, CONJG( S ) )
                     IF( ILAZR2 )
     $                  A( JCH, JCH-1 ) = A( JCH, JCH-1 )*C
                     ILAZR2 = .FALSE.
*                    --------------- Begin Timing Code -----------------
                     OPST = OPST + REAL( 32+40*( ILASTM-JCH )+20*NQ )
*                    ---------------- End Timing Code ------------------
                     IF( ABS1( B( JCH+1, JCH+1 ) ).GE.BTOL ) THEN
                        IF( JCH+1.GE.ILAST ) THEN
                           GO TO 60
                        ELSE
                           IFIRST = JCH + 1
                           GO TO 70
                        END IF
                     END IF
                     B( JCH+1, JCH+1 ) = CZERO
   20             CONTINUE
                  GO TO 50
               ELSE
*
*                 Only test 2 passed -- chase the zero to B(ILAST,ILAST)
*                 Then process as in the case B(ILAST,ILAST)=0
*
                  DO 30 JCH = J, ILAST - 1
                     CTEMP = B( JCH, JCH+1 )
                     CALL CLARTG( CTEMP, B( JCH+1, JCH+1 ), C, S,
     $                            B( JCH, JCH+1 ) )
                     B( JCH+1, JCH+1 ) = CZERO
                     IF( JCH.LT.ILASTM-1 )
     $                  CALL CROT( ILASTM-JCH-1, B( JCH, JCH+2 ), LDB,
     $                             B( JCH+1, JCH+2 ), LDB, C, S )
                     CALL CROT( ILASTM-JCH+2, A( JCH, JCH-1 ), LDA,
     $                          A( JCH+1, JCH-1 ), LDA, C, S )
                     IF( ILQ )
     $                  CALL CROT( N, Q( 1, JCH ), 1, Q( 1, JCH+1 ), 1,
     $                             C, CONJG( S ) )
                     CTEMP = A( JCH+1, JCH )
                     CALL CLARTG( CTEMP, A( JCH+1, JCH-1 ), C, S,
     $                            A( JCH+1, JCH ) )
                     A( JCH+1, JCH-1 ) = CZERO
                     CALL CROT( JCH+1-IFRSTM, A( IFRSTM, JCH ), 1,
     $                          A( IFRSTM, JCH-1 ), 1, C, S )
                     CALL CROT( JCH-IFRSTM, B( IFRSTM, JCH ), 1,
     $                          B( IFRSTM, JCH-1 ), 1, C, S )
                     IF( ILZ )
     $                  CALL CROT( N, Z( 1, JCH ), 1, Z( 1, JCH-1 ), 1,
     $                             C, S )
   30             CONTINUE
*
*                 ---------------- Begin Timing Code -------------------
                  OPST = OPST + REAL( 64+40*( ILASTM+1-IFRSTM )+20*
     $                   ( NQ+NZ ) )*REAL( ILAST-J )
*                 ----------------- End Timing Code --------------------
*
                  GO TO 50
               END IF
            ELSE IF( ILAZRO ) THEN
*
*              Only test 1 passed -- work on J:ILAST
*
               IFIRST = J
               GO TO 70
            END IF
*
*           Neither test passed -- try next J
*
   40    CONTINUE
*
*        (Drop-through is "impossible")
*
         INFO = 2*N + 1
         GO TO 210
*
*        B(ILAST,ILAST)=0 -- clear A(ILAST,ILAST-1) to split off a
*        1x1 block.
*
   50    CONTINUE
         CTEMP = A( ILAST, ILAST )
         CALL CLARTG( CTEMP, A( ILAST, ILAST-1 ), C, S,
     $                A( ILAST, ILAST ) )
         A( ILAST, ILAST-1 ) = CZERO
         CALL CROT( ILAST-IFRSTM, A( IFRSTM, ILAST ), 1,
     $              A( IFRSTM, ILAST-1 ), 1, C, S )
         CALL CROT( ILAST-IFRSTM, B( IFRSTM, ILAST ), 1,
     $              B( IFRSTM, ILAST-1 ), 1, C, S )
         IF( ILZ )
     $      CALL CROT( N, Z( 1, ILAST ), 1, Z( 1, ILAST-1 ), 1, C, S )
*        --------------------- Begin Timing Code -----------------------
         OPST = OPST + REAL( 32+40*( ILAST-IFRSTM )+20*NZ )
*        ---------------------- End Timing Code ------------------------
*
*        A(ILAST,ILAST-1)=0 -- Standardize B, set ALPHA and BETA
*
   60    CONTINUE
         ABSB = ABS( B( ILAST, ILAST ) )
         IF( ABSB.GT.SAFMIN ) THEN
            SIGNBC = CONJG( B( ILAST, ILAST ) / ABSB )
            B( ILAST, ILAST ) = ABSB
            IF( ILSCHR ) THEN
               CALL CSCAL( ILAST-IFRSTM, SIGNBC, B( IFRSTM, ILAST ), 1 )
               CALL CSCAL( ILAST+1-IFRSTM, SIGNBC, A( IFRSTM, ILAST ),
     $                     1 )
*              ----------------- Begin Timing Code ---------------------
               OPST = OPST + REAL( 12*( ILAST-IFRSTM ) )
*              ------------------ End Timing Code ----------------------
            ELSE
               A( ILAST, ILAST ) = A( ILAST, ILAST )*SIGNBC
            END IF
            IF( ILZ )
     $         CALL CSCAL( N, SIGNBC, Z( 1, ILAST ), 1 )
*           ------------------- Begin Timing Code ----------------------
            OPST = OPST + REAL( 6*NZ+13 )
*           -------------------- End Timing Code -----------------------
         ELSE
            B( ILAST, ILAST ) = CZERO
         END IF
         ALPHA( ILAST ) = A( ILAST, ILAST )
         BETA( ILAST ) = B( ILAST, ILAST )
*
*        Go to next block -- exit if finished.
*
         ILAST = ILAST - 1
         IF( ILAST.LT.ILO )
     $      GO TO 190
*
*        Reset counters
*
         IITER = 0
         ESHIFT = CZERO
         IF( .NOT.ILSCHR ) THEN
            ILASTM = ILAST
            IF( IFRSTM.GT.ILAST )
     $         IFRSTM = ILO
         END IF
         GO TO 160
*
*        QZ step
*
*        This iteration only involves rows/columns IFIRST:ILAST.  We
*        assume IFIRST < ILAST, and that the diagonal of B is non-zero.
*
   70    CONTINUE
         IITER = IITER + 1
         IF( .NOT.ILSCHR ) THEN
            IFRSTM = IFIRST
         END IF
*
*        Compute the Shift.
*
*        At this point, IFIRST < ILAST, and the diagonal entries of
*        B(IFIRST:ILAST,IFIRST,ILAST) are larger than BTOL (in
*        magnitude)
*
         IF( ( IITER / 10 )*10.NE.IITER ) THEN
*
*           The Wilkinson shift (AEP p.512), i.e., the eigenvalue of
*           the bottom-right 2x2 block of A inv(B) which is nearest to
*           the bottom-right entry.
*
*           We factor B as U*D, where U has unit diagonals, and
*           compute (A*inv(D))*inv(U).
*
            U12 = ( BSCALE*B( ILAST-1, ILAST ) ) /
     $            ( BSCALE*B( ILAST, ILAST ) )
            AD11 = ( ASCALE*A( ILAST-1, ILAST-1 ) ) /
     $             ( BSCALE*B( ILAST-1, ILAST-1 ) )
            AD21 = ( ASCALE*A( ILAST, ILAST-1 ) ) /
     $             ( BSCALE*B( ILAST-1, ILAST-1 ) )
            AD12 = ( ASCALE*A( ILAST-1, ILAST ) ) /
     $             ( BSCALE*B( ILAST, ILAST ) )
            AD22 = ( ASCALE*A( ILAST, ILAST ) ) /
     $             ( BSCALE*B( ILAST, ILAST ) )
            ABI22 = AD22 - U12*AD21
*
            T = HALF*( AD11+ABI22 )
            RTDISC = SQRT( T**2+AD12*AD21-AD11*AD22 )
            TEMP = REAL( T-ABI22 )*REAL( RTDISC ) +
     $             AIMAG( T-ABI22 )*AIMAG( RTDISC )
            IF( TEMP.LE.ZERO ) THEN
               SHIFT = T + RTDISC
            ELSE
               SHIFT = T - RTDISC
            END IF
*
*           ------------------- Begin Timing Code ----------------------
            OPST = OPST + REAL( 116 )
*           -------------------- End Timing Code -----------------------
*
         ELSE
*
*           Exceptional shift.  Chosen for no particularly good reason.
*
            ESHIFT = ESHIFT + CONJG( ( ASCALE*A( ILAST-1, ILAST ) ) /
     $               ( BSCALE*B( ILAST-1, ILAST-1 ) ) )
            SHIFT = ESHIFT
*
*           ------------------- Begin Timing Code ----------------------
            OPST = OPST + REAL( 15 )
*           -------------------- End Timing Code -----------------------
*
         END IF
*
*        Now check for two consecutive small subdiagonals.
*
         DO 80 J = ILAST - 1, IFIRST + 1, -1
            ISTART = J
            CTEMP = ASCALE*A( J, J ) - SHIFT*( BSCALE*B( J, J ) )
            IF( ABS1( A( J, J-1 ) )*( ASCALE*ABS1( A( J+1, J ) ) ).LE.
     $          ABS1( CTEMP )*ATOL )GO TO 90
   80    CONTINUE
*
         ISTART = IFIRST
         CTEMP = ASCALE*A( IFIRST, IFIRST ) -
     $           SHIFT*( BSCALE*B( IFIRST, IFIRST ) )
*
*        --------------------- Begin Timing Code -----------------------
         OPST = OPST - REAL( 6 )
*        ---------------------- End Timing Code ------------------------
*
   90    CONTINUE
*
*        Do an implicit-shift QZ sweep.
*
*        Initial Q
*
         CTEMP2 = ASCALE*A( ISTART+1, ISTART )
*
*        --------------------- Begin Timing Code -----------------------
         OPST = OPST + REAL( 2+( ILAST-ISTART )*18 )
*        ---------------------- End Timing Code ------------------------
*
         CALL CLARTG( CTEMP, CTEMP2, C, S, CTEMP3 )
*
*        Sweep
*
         DO 150 J = ISTART, ILAST - 1
            IF( J.GT.ISTART ) THEN
               CTEMP = A( J, J-1 )
               CALL CLARTG( CTEMP, A( J+1, J-1 ), C, S, A( J, J-1 ) )
               A( J+1, J-1 ) = CZERO
            END IF
*
            DO 100 JC = J, ILASTM
               CTEMP = C*A( J, JC ) + S*A( J+1, JC )
               A( J+1, JC ) = -CONJG( S )*A( J, JC ) + C*A( J+1, JC )
               A( J, JC ) = CTEMP
               CTEMP2 = C*B( J, JC ) + S*B( J+1, JC )
               B( J+1, JC ) = -CONJG( S )*B( J, JC ) + C*B( J+1, JC )
               B( J, JC ) = CTEMP2
  100       CONTINUE
            IF( ILQ ) THEN
               DO 110 JR = 1, N
                  CTEMP = C*Q( JR, J ) + CONJG( S )*Q( JR, J+1 )
                  Q( JR, J+1 ) = -S*Q( JR, J ) + C*Q( JR, J+1 )
                  Q( JR, J ) = CTEMP
  110          CONTINUE
            END IF
*
            CTEMP = B( J+1, J+1 )
            CALL CLARTG( CTEMP, B( J+1, J ), C, S, B( J+1, J+1 ) )
            B( J+1, J ) = CZERO
*
            DO 120 JR = IFRSTM, MIN( J+2, ILAST )
               CTEMP = C*A( JR, J+1 ) + S*A( JR, J )
               A( JR, J ) = -CONJG( S )*A( JR, J+1 ) + C*A( JR, J )
               A( JR, J+1 ) = CTEMP
  120       CONTINUE
            DO 130 JR = IFRSTM, J
               CTEMP = C*B( JR, J+1 ) + S*B( JR, J )
               B( JR, J ) = -CONJG( S )*B( JR, J+1 ) + C*B( JR, J )
               B( JR, J+1 ) = CTEMP
  130       CONTINUE
            IF( ILZ ) THEN
               DO 140 JR = 1, N
                  CTEMP = C*Z( JR, J+1 ) + S*Z( JR, J )
                  Z( JR, J ) = -CONJG( S )*Z( JR, J+1 ) + C*Z( JR, J )
                  Z( JR, J+1 ) = CTEMP
  140          CONTINUE
            END IF
  150    CONTINUE
*
*        --------------------- Begin Timing Code -----------------------
         OPST = OPST + ( REAL( ILAST-ISTART )*
     $          REAL( 120+64+40*( ILASTM-IFRSTM )+20*( NQ+NZ ) )-20 )
*        ---------------------- End Timing Code ------------------------
*
  160    CONTINUE
*
*        --------------------- Begin Timing Code -----------------------
*        End of iteration -- add in "small" contributions.
         OPS = OPS + OPST
         OPST = ZERO
*        ---------------------- End Timing Code ------------------------
*
*
  170 CONTINUE
*
*     Drop-through = non-convergence
*
  180 CONTINUE
      INFO = ILAST
*
*     ---------------------- Begin Timing Code -------------------------
      OPS = OPS + OPST
      OPST = ZERO
*     ----------------------- End Timing Code --------------------------
*
      GO TO 210
*
*     Successful completion of all QZ steps
*
  190 CONTINUE
*
*     Set Eigenvalues 1:ILO-1
*
      DO 200 J = 1, ILO - 1
         ABSB = ABS( B( J, J ) )
         IF( ABSB.GT.SAFMIN ) THEN
            SIGNBC = CONJG( B( J, J ) / ABSB )
            B( J, J ) = ABSB
            IF( ILSCHR ) THEN
               CALL CSCAL( J-1, SIGNBC, B( 1, J ), 1 )
               CALL CSCAL( J, SIGNBC, A( 1, J ), 1 )
*              ----------------- Begin Timing Code ---------------------
               OPST = OPST + REAL( 12*( J-1 ) )
*              ------------------ End Timing Code ----------------------
            ELSE
               A( J, J ) = A( J, J )*SIGNBC
            END IF
            IF( ILZ )
     $         CALL CSCAL( N, SIGNBC, Z( 1, J ), 1 )
*           ------------------- Begin Timing Code ----------------------
            OPST = OPST + REAL( 6*NZ+13 )
*           -------------------- End Timing Code -----------------------
         ELSE
            B( J, J ) = CZERO
         END IF
         ALPHA( J ) = A( J, J )
         BETA( J ) = B( J, J )
  200 CONTINUE
*
*     Normal Termination
*
      INFO = 0
*
*     Exit (other than argument error) -- return optimal workspace size
*
  210 CONTINUE
*
*     ---------------------- Begin Timing Code -------------------------
      OPS = OPS + OPST
      OPST = ZERO
      ITCNT = JITER
*     ----------------------- End Timing Code --------------------------
*
      WORK( 1 ) = CMPLX( 1 )
      RETURN
*
*     End of CHGEQZ
*
      END
