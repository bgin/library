      SUBROUTINE SSBEVX( JOBZ, RANGE, UPLO, N, KD, AB, LDAB, Q, LDQ, VL,
     $                   VU, IL, IU, ABSTOL, M, W, Z, LDZ, WORK, IWORK,
     $                   IFAIL, INFO )
*
*  -- LAPACK driver routine (version 1.1) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     March 31, 1993 
*
*     .. Scalar Arguments ..
      CHARACTER          JOBZ, RANGE, UPLO
      INTEGER            IL, INFO, IU, KD, LDAB, LDQ, LDZ, M, N
      REAL               ABSTOL, VL, VU
*     ..
*     .. Array Arguments ..
      INTEGER            IFAIL( * ), IWORK( * )
      REAL               AB( LDAB, * ), Q( LDQ, * ), W( * ), WORK( * ),
     $                   Z( LDZ, * )
*     ..
*
*  Purpose
*  =======
*
*  SSBEVX computes selected eigenvalues and, optionally, eigenvectors
*  of a real symmetric band matrix A.  Eigenvalues/vectors can be
*  selected by specifying either a range of values or a range of indices
*  for the desired eigenvalues.
*
*  Arguments
*  =========
*
*  JOBZ    (input) CHARACTER*1
*          = 'N':  Compute eigenvalues only;
*          = 'V':  Compute eigenvalues and eigenvectors.
*
*  RANGE   (input) CHARACTER*1
*          = 'A': all eigenvalues will be found;
*          = 'V': all eigenvalues in the half-open interval (VL,VU]
*                 will be found;
*          = 'I': the IL-th through IU-th eigenvalues will be found.
*
*  UPLO    (input) CHARACTER*1
*          = 'U':  Upper triangle of A is stored;
*          = 'L':  Lower triangle of A is stored.
*
*  N       (input) INTEGER
*          The order of the matrix A.  N >= 0.
*
*  KD      (input) INTEGER
*          The number of superdiagonals of the matrix A if UPLO = 'U',
*          or the number of subdiagonals if UPLO = 'L'.  KD >= 0.
*
*  AB      (input/output) REAL array, dimension (LDAB, N)
*          On entry, the upper or lower triangle of the symmetric band
*          matrix A, stored in the first KD+1 rows of the array.  The
*          j-th column of A is stored in the j-th column of the array AB
*          as follows:
*          if UPLO = 'U', AB(kd+1+i-j,j) = A(i,j) for max(1,j-kd)<=i<=j;
*          if UPLO = 'L', AB(1+i-j,j)    = A(i,j) for j<=i<=min(n,j+kd).
*
*          On exit, AB is overwritten by values generated during the
*          reduction to tridiagonal form.  If UPLO = 'U', the first
*          superdiagonal and the diagonal of the tridiagonal matrix T
*          are returned in rows KD and KD+1 of AB, and if UPLO = 'L',
*          the diagonal and first subdiagonal of T are returned in the
*          first two rows of AB.
*
*  LDAB    (input) INTEGER
*          The leading dimension of the array AB.  LDAB >= KD + 1.
*
*  Q       (output) REAL array, dimension (LDQ, N)
*          If JOBZ = 'V', the N-by-N orthogonal matrix used in the
*                         reduction to tridiagonal form.
*          If JOBZ = 'N', the array Q is not referenced.
*
*  LDQ     (input) INTEGER
*          The leading dimension of the array Q.  If JOBZ = 'V', then
*          LDQ >= max(1,N).
*
*  VL      (input) REAL
*          If RANGE='V', the lower bound of the interval to be searched
*          for eigenvalues.  Not referenced if RANGE = 'A' or 'I'.
*
*  VU      (input) REAL
*          If RANGE='V', the upper bound of the interval to be searched
*          for eigenvalues.  Not referenced if RANGE = 'A' or 'I'.
*
*  IL      (input) INTEGER
*          If RANGE='I', the index (from smallest to largest) of the
*          smallest eigenvalue to be returned.  IL >= 1.
*          Not referenced if RANGE = 'A' or 'V'.
*
*  IU      (input) INTEGER
*          If RANGE='I', the index (from smallest to largest) of the
*          largest eigenvalue to be returned.  min(IL,N) <= IU <= N.
*          Not referenced if RANGE = 'A' or 'V'.
*
*  ABSTOL  (input) REAL
*          The absolute error tolerance for the eigenvalues.
*          An approximate eigenvalue is accepted as converged
*          when it is determined to lie in an interval [a,b]
*          of width less than or equal to
*
*                  ABSTOL + EPS *   max( |a|,|b| ) ,
*
*          where EPS is the machine precision.  If ABSTOL is less than
*          or equal to zero, then  EPS*|T|  will be used in its place,
*          where |T| is the 1-norm of the tridiagonal matrix obtained
*          by reducing AB to tridiagonal form.
*
*          See "Computing Small Singular Values of Bidiagonal Matrices
*          with Guaranteed High Relative Accuracy," by Demmel and
*          Kahan, LAPACK Working Note #3.
*
*  M       (output) INTEGER
*          The total number of eigenvalues found.  0 <= M <= N.
*          If RANGE = 'A', M = N, and if RANGE = 'I', M = IU-IL+1.
*
*  W       (output) REAL array, dimension (N)
*          The first M elements contain the selected eigenvalues in
*          ascending order.
*
*  Z       (output) REAL array, dimension (LDZ, max(1,M))
*          If JOBZ = 'V', then if INFO = 0, the first M columns of Z
*          contain the orthonormal eigenvectors of the matrix
*          corresponding to the selected eigenvalues.  If an eigenvector
*          fails to converge, then that column of Z contains the latest
*          approximation to the eigenvector, and the index of the
*          eigenvector is returned in IFAIL.
*          If JOBZ = 'N', then Z is not referenced.
*          Note: the user must ensure that at least max(1,M) columns are
*          supplied in the array Z; if RANGE = 'V', the exact value of M
*          is not known in advance and an upper bound must be used.
*
*  LDZ     (input) INTEGER
*          The leading dimension of the array Z.  LDZ >= 1, and if
*          JOBZ = 'V', LDZ >= max(1,N).
*
*  WORK    (workspace) REAL array, dimension (7*N)
*
*  IWORK   (workspace) INTEGER array, dimension (5*N)
*
*  IFAIL   (output) INTEGER array, dimension (N)
*          If JOBZ = 'V', then if INFO = 0, the first M elements of
*          IFAIL are zero.  If INFO > 0, then IFAIL contains the
*          indices of the eigenvectors that failed to converge.
*          If JOBZ = 'N', then IFAIL is not referenced.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit.
*          < 0:  if INFO = -i, the i-th argument had an illegal value.
*          > 0:  if INFO = i, then i eigenvectors failed to converge.
*                Their indices are stored in array IFAIL.
*
*  =====================================================================
*
*     .. Parameters ..
      REAL               ZERO, ONE
      PARAMETER          ( ZERO = 0.0E0, ONE = 1.0E0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            ALLEIG, INDEIG, LOWER, VALEIG, WANTZ
      CHARACTER          ORDER
      INTEGER            I, IINFO, ILEN, IMAX, INDD, INDE, INDEE,
     $                   INDIBL, INDISP, INDIWO, INDWRK, ISCALE, ITMP1,
     $                   J, JJ, NSPLIT
      REAL               ABSTLL, ANRM, BIGNUM, EPS, RMAX, RMIN, SAFMIN,
     $                   SIGMA, SMLNUM, TMP1, VLL, VUU
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      REAL               SLAMCH, SLANSB
      EXTERNAL           LSAME, SLAMCH, SLANSB
*     ..
*     .. External Subroutines ..
      EXTERNAL           SCOPY, SGEMV, SLACPY, SSBTRD, SSCAL, SSTEBZ,
     $                   SSTEIN, SSTEQR, SSTERF, SSWAP, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MIN, SQRT
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      WANTZ = LSAME( JOBZ, 'V' )
      ALLEIG = LSAME( RANGE, 'A' )
      VALEIG = LSAME( RANGE, 'V' )
      INDEIG = LSAME( RANGE, 'I' )
      LOWER = LSAME( UPLO, 'L' )
*
      INFO = 0
      IF( .NOT.( WANTZ .OR. LSAME( JOBZ, 'N' ) ) ) THEN
         INFO = -1
      ELSE IF( .NOT.( ALLEIG .OR. VALEIG .OR. INDEIG ) ) THEN
         INFO = -2
      ELSE IF( .NOT.( LOWER .OR. LSAME( UPLO, 'U' ) ) ) THEN
         INFO = -3
      ELSE IF( N.LT.0 ) THEN
         INFO = -4
      ELSE IF( KD.LT.0 ) THEN
         INFO = -5
      ELSE IF( LDAB.LT.KD+1 ) THEN
         INFO = -7
      ELSE IF( LDQ.LT.N ) THEN
         INFO = -9
      ELSE IF( VALEIG .AND. N.GT.0 .AND. VU.LE.VL ) THEN
         INFO = -11
      ELSE IF( INDEIG .AND. IL.LT.1 ) THEN
         INFO = -12
      ELSE IF( INDEIG .AND. ( IU.LT.MIN( N, IL ) .OR. IU.GT.N ) ) THEN
         INFO = -13
      ELSE IF( LDZ.LT.1 .OR. ( WANTZ .AND. LDZ.LT.N ) ) THEN
         INFO = -18
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'SSBEVX', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      M = 0
      IF( N.EQ.0 )
     $   RETURN
*
      IF( N.EQ.1 ) THEN
         IF( ALLEIG .OR. INDEIG ) THEN
            M = 1
            W( 1 ) = AB( 1, 1 )
         ELSE
            IF( VL.LT.AB( 1, 1 ) .AND. VU.GE.AB( 1, 1 ) ) THEN
               M = 1
               W( 1 ) = AB( 1, 1 )
            END IF
         END IF
         IF( WANTZ )
     $      Z( 1, 1 ) = ONE
         RETURN
      END IF
*
*     Get machine constants.
*
      SAFMIN = SLAMCH( 'Safe minimum' )
      EPS = SLAMCH( 'Precision' )
      SMLNUM = SAFMIN / EPS
      BIGNUM = ONE / SMLNUM
      RMIN = SQRT( SMLNUM )
      RMAX = MIN( SQRT( BIGNUM ), ONE / SQRT( SQRT( SAFMIN ) ) )
*
*     Scale matrix to allowable range, if necessary.
*
      ISCALE = 0
      ABSTLL = ABSTOL
      IF( VALEIG ) THEN
         VLL = VL
         VUU = VU
      END IF
      ANRM = SLANSB( 'M', UPLO, N, KD, AB, LDAB, WORK )
      IF( ANRM.GT.ZERO .AND. ANRM.LT.RMIN ) THEN
         ISCALE = 1
         SIGMA = RMIN / ANRM
      ELSE IF( ANRM.GT.RMAX ) THEN
         ISCALE = 1
         SIGMA = RMAX / ANRM
      END IF
      IF( ISCALE.EQ.1 ) THEN
         IF( LOWER ) THEN
            DO 10 I = 1, N
               CALL SSCAL( MIN( KD+1, N-I+1 ), SIGMA, AB( 1, I ), 1 )
   10       CONTINUE
         ELSE
            DO 20 I = 1, N
               ILEN = MIN( I, KD+1 )
               CALL SSCAL( ILEN, SIGMA, AB( KD+2-ILEN, I ), 1 )
   20       CONTINUE
         END IF
         IF( ABSTOL.GT.0 )
     $      ABSTLL = ABSTOL*SIGMA
         IF( VALEIG ) THEN
            VLL = VL*SIGMA
            VUU = VU*SIGMA
         END IF
      END IF
*
*     Call SSBTRD to reduce symmetric band matrix to tridiagonal form.
*
      INDD = 1
      INDE = INDD + N
      INDWRK = INDE + N
      CALL SSBTRD( JOBZ, UPLO, N, KD, AB, LDAB, WORK( INDD ),
     $             WORK( INDE ), Q, LDQ, WORK( INDWRK ), IINFO )
*
*     If all eigenvalues are desired and ABSTOL is less than or equal
*     to zero, then call SSTERF or SSTEQR.  If this fails for some
*     eigenvalue, then try SSTEBZ.
*
      IF( ( ALLEIG .OR. ( INDEIG .AND. IL.EQ.1 .AND. IU.EQ.N ) ) .AND.
     $    ( ABSTOL.LE.ZERO ) ) THEN
         CALL SCOPY( N, WORK( INDD ), 1, W, 1 )
         INDEE = INDWRK + 2*N
         IF( .NOT.WANTZ ) THEN
            CALL SCOPY( N-1, WORK( INDE ), 1, WORK( INDEE ), 1 )
            CALL SSTERF( N, W, WORK( INDEE ), INFO )
         ELSE
            CALL SLACPY( 'A', N, N, Q, LDQ, Z, LDZ )
            CALL SCOPY( N-1, WORK( INDE ), 1, WORK( INDEE ), 1 )
            CALL SSTEQR( JOBZ, N, W, WORK( INDEE ), Z, LDZ,
     $                   WORK( INDWRK ), INFO )
            IF( INFO.EQ.0 ) THEN
               DO 30 I = 1, N
                  IFAIL( I ) = 0
   30          CONTINUE
            END IF
         END IF
         IF( INFO.EQ.0 ) THEN
            M = N
            GO TO 50
         END IF
         INFO = 0
      END IF
*
*     Otherwise, call SSTEBZ and, if eigenvectors are desired, SSTEIN.
*
      IF( WANTZ ) THEN
         ORDER = 'B'
      ELSE
         ORDER = 'E'
      END IF
      INDIBL = 1
      INDISP = INDIBL + N
      INDIWO = INDISP + N
      CALL SSTEBZ( RANGE, ORDER, N, VLL, VUU, IL, IU, ABSTLL,
     $             WORK( INDD ), WORK( INDE ), M, NSPLIT, W,
     $             IWORK( INDIBL ), IWORK( INDISP ), WORK( INDWRK ),
     $             IWORK( INDIWO ), INFO )
*
      IF( WANTZ ) THEN
         CALL SSTEIN( N, WORK( INDD ), WORK( INDE ), M, W,
     $                IWORK( INDIBL ), IWORK( INDISP ), Z, LDZ,
     $                WORK( INDWRK ), IWORK( INDIWO ), IFAIL, INFO )
*
*        Apply orthogonal matrix used in reduction to tridiagonal
*        form to eigenvectors returned by SSTEIN.
*
         DO 40 J = 1, M
            CALL SCOPY( N, Z( 1, J ), 1, WORK( 1 ), 1 )
            CALL SGEMV( 'N', N, N, ONE, Q, LDQ, WORK, 1, ZERO,
     $                  Z( 1, J ), 1 )
   40    CONTINUE
      END IF
*
*     If matrix was scaled, then rescale eigenvalues appropriately.
*
   50 CONTINUE
      IF( ISCALE.EQ.1 ) THEN
         IF( INFO.EQ.0 ) THEN
            IMAX = M
         ELSE
            IMAX = INFO - 1
         END IF
         CALL SSCAL( IMAX, ONE / SIGMA, W, 1 )
      END IF
*
*     If eigenvalues are not in order, then sort them, along with
*     eigenvectors.
*
      IF( WANTZ ) THEN
         DO 70 J = 1, M - 1
            I = 0
            TMP1 = W( J )
            DO 60 JJ = J + 1, M
               IF( W( JJ ).LT.TMP1 ) THEN
                  I = JJ
                  TMP1 = W( JJ )
               END IF
   60       CONTINUE
*
            IF( I.NE.0 ) THEN
               ITMP1 = IWORK( INDIBL+I-1 )
               W( I ) = W( J )
               IWORK( INDIBL+I-1 ) = IWORK( INDIBL+J-1 )
               W( J ) = TMP1
               IWORK( INDIBL+J-1 ) = ITMP1
               CALL SSWAP( N, Z( 1, I ), 1, Z( 1, J ), 1 )
               IF( INFO.NE.0 ) THEN
                  ITMP1 = IFAIL( I )
                  IFAIL( I ) = IFAIL( J )
                  IFAIL( J ) = ITMP1
               END IF
            END IF
   70    CONTINUE
      END IF
*
      RETURN
*
*     End of SSBEVX
*
      END