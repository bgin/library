      SUBROUTINE DLAQZH( ILQ, ILZ, N, ILO, IHI, A, LDA, B, LDB, Q, LDQ,
     $                   Z, LDZ, WORK, INFO )
*
*  -- LAPACK timing routine (version 1.1) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     March 31, 1993
*
*     .. Scalar Arguments ..
      LOGICAL            ILQ, ILZ
      INTEGER            IHI, ILO, INFO, LDA, LDB, LDQ, LDZ, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), Q( LDQ, * ),
     $                   WORK( N ), Z( LDZ, * )
*     ..
*
*  Purpose
*  =======
*
*  This calls the LAPACK routines to perform the function of
*  QZHES.  It is similar in function to DGGHRD, except that
*  B is not assumed to be upper-triangular.
*
*  It reduces a pair of matrices (A,B) to a Hessenberg-triangular
*  pair (H,T).  More specifically, it computes orthogonal matrices
*  Q and Z, an (upper) Hessenberg matrix H, and an upper triangular
*  matrix T such that:
*
*    A = Q H Z'    and   B = Q T Z'
*
*
*  Arguments
*  =========
*
*  ILQ     (input) LOGICAL
*          = .FALSE. do not compute Q.
*          = .TRUE.  compute Q.
*
*  ILZ     (input) LOGICAL
*          = .FALSE. do not compute Z.
*          = .TRUE.  compute Z.
*
*  N       (input) INTEGER
*          The number of rows and columns in the matrices A, B, Q, and
*          Z.  N must be at least 0.
*
*  ILO     (input) INTEGER
*          Columns 1 through ILO-1 of A and B are assumed to be in
*          upper triangular form already, and will not be modified.
*          ILO must be at least 1.
*
*  IHI     (input) INTEGER
*          Rows IHI+1 through N of A and B are assumed to be in upper
*          triangular form already, and will not be touched.  IHI may
*          not be greater than N.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA, N)
*          On entry, the first of the pair of N x N general matrices to
*          be reduced.
*          On exit, the upper triangle and the first subdiagonal of A
*          are overwritten with the Hessenberg matrix H, and the rest
*          is set to zero.
*
*  LDA     (input) INTEGER
*          The leading dimension of A as declared in the calling
*          program. LDA must be at least max ( 1, N ) .
*
*  B       (input/output) DOUBLE PRECISION array, dimension (LDB, N)
*          On entry, the second of the pair of N x N general matrices to
*          be reduced.
*          On exit, the transformed matrix T = Q' B Z, which is upper
*          triangular.
*
*  LDB     (input) INTEGER
*          The leading dimension of B as declared in the calling
*          program. LDB must be at least max ( 1, N ) .
*
*  Q       (output) DOUBLE PRECISION array, dimension (LDQ,N)
*          If ILQ = .TRUE., Q will contain the orthogonal matrix Q.
*          (See "Purpose", above.)
*          Will not be referenced if ILQ = .FALSE.
*
*  LDQ     (input) INTEGER
*          The leading dimension of the matrix Q. LDQ must be at
*          least 1 and at least N.
*
*  Z       (output) DOUBLE PRECISION array, dimension (LDZ,N)
*          If ILZ = .TRUE., Z will contain the orthogonal matrix Z.
*          (See "Purpose", above.)
*          May be referenced even if ILZ = .FALSE.
*
*  LDZ     (input) INTEGER
*          The leading dimension of the matrix Z. LDZ must be at
*          least 1 and at least N.
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (N)
*          Workspace.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit.
*          < 0:  if INFO = -i, the i-th argument had an illegal value.
*          > 0:  errors that usually indicate LAPACK problems:
*                = 2: error return from DGEQRF;
*                = 3: error return from DORMQR;
*                = 4: error return from DORGQR;
*                = 5: error return from DGGHRD.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0 )
*     ..
*     .. Local Scalars ..
      CHARACTER          COMPQ, COMPZ
      INTEGER            ICOLS, IINFO, IROWS
*     ..
*     .. External Subroutines ..
      EXTERNAL           DGEQRF, DGGHRD, DLACPY, DLAZRO, DORGQR, DORMQR
*     ..
*     .. Executable Statements ..
*
*     Quick return if possible
*
      IF( N.EQ.0 )
     $   RETURN
*
*     Reduce B to triangular form, and initialize Q and/or Z
*
      IROWS = IHI + 1 - ILO
      ICOLS = N + 1 - ILO
      CALL DGEQRF( IROWS, ICOLS, B( ILO, ILO ), LDB, WORK, Z, N*LDZ,
     $             IINFO )
      IF( IINFO.NE.0 ) THEN
         INFO = 2
         GO TO 10
      END IF
*
      CALL DORMQR( 'L', 'T', IROWS, ICOLS, IROWS, B( ILO, ILO ), LDB,
     $             WORK, A( ILO, ILO ), LDA, Z, N*LDZ, IINFO )
      IF( IINFO.NE.0 ) THEN
         INFO = 3
         GO TO 10
      END IF
*
      IF( ILQ ) THEN
         CALL DLAZRO( N, N, ZERO, ONE, Q, LDQ )
         CALL DLACPY( 'L', IROWS-1, IROWS-1, B( ILO+1, ILO ), LDB,
     $                Q( ILO+1, ILO ), LDQ )
         CALL DORGQR( IROWS, IROWS, IROWS, Q( ILO, ILO ), LDQ, WORK, Z,
     $                N*LDZ, IINFO )
         IF( IINFO.NE.0 ) THEN
            INFO = 4
            GO TO 10
         END IF
      END IF
*
*     Reduce to generalized Hessenberg form
*
      IF( ILQ ) THEN
         COMPQ = 'V'
      ELSE
         COMPQ = 'N'
      END IF
*
      IF( ILZ ) THEN
         COMPZ = 'I'
      ELSE
         COMPZ = 'N'
      END IF
*
      CALL DGGHRD( COMPQ, COMPZ, N, ILO, IHI, A, LDA, B, LDB, Q, LDQ, Z,
     $             LDZ, IINFO )
      IF( IINFO.NE.0 ) THEN
         INFO = 5
         GO TO 10
      END IF
*
*     End
*
   10 CONTINUE
*
      RETURN
*
*     End of DLAQZH
*
      END
