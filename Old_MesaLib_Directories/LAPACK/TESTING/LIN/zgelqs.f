      SUBROUTINE ZGELQS( M, N, NRHS, A, LDA, TAU, B, LDB, WORK, LWORK,
     $                   INFO )
*
*  -- LAPACK routine (version 1.1) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
      INTEGER            INFO, LDA, LDB, LWORK, M, N, NRHS
*     ..
*     .. Array Arguments ..
      COMPLEX*16         A( LDA, * ), B( LDB, * ), TAU( * ),
     $                   WORK( LWORK )
*     ..
*
*  Purpose
*  =======
*
*  Compute a minimum-norm solution
*      min || A*X - B ||
*  using the LQ factorization
*      A = L*Q
*  computed by ZGELQF.
*
*  Arguments
*  =========
*
*  M       (input) INTEGER
*          The number of rows of the matrix A.  M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix A.  N >= M >= 0.
*
*  NRHS    (input) INTEGER
*          The number of columns of B.  NRHS >= 0.
*
*  A       (input) COMPLEX*16 array, dimension (LDA,N)
*          Details of the LQ factorization of the original matrix A as
*          returned by ZGELQF.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= M.
*
*  TAU     (input) COMPLEX*16 array, dimension (M)
*          Details of the orthogonal matrix Q.
*
*  B       (input/output) COMPLEX*16 array, dimension (LDB,NRHS)
*          On entry, the m-by-nrhs right hand side matrix B.
*          On exit, the n-by-nrhs solution matrix X.
*
*  LDB     (input) INTEGER
*          The leading dimension of the array B. LDB >= N.
*
*  WORK    (workspace) COMPLEX*16 array, dimension (LWORK)
*
*  LWORK   (input) INTEGER
*          The length of the array WORK.  LWORK must be at least NRHS,
*          and should be at least NRHS*NB, where NB is the block size
*          for this environment.
*
*  INFO    (output) INTEGER
*          = 0: successful exit
*          < 0: if INFO = -i, the i-th argument had an illegal value
*
*  =====================================================================
*
*     .. Parameters ..
      COMPLEX*16         ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
*     ..
*     .. External Subroutines ..
      EXTERNAL           XERBLA, ZLAZRO, ZTRSM, ZUNMLQ
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 .OR. M.GT.N ) THEN
         INFO = -2
      ELSE IF( NRHS.LT.0 ) THEN
         INFO = -3
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -5
      ELSE IF( LDB.LT.MAX( 1, N ) ) THEN
         INFO = -8
      ELSE IF( LWORK.LT.1 .OR. LWORK.LT.NRHS .AND. M.GT.0 .AND. N.GT.0 )
     $          THEN
         INFO = -10
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'ZGELQS', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( N.EQ.0 .OR. NRHS.EQ.0 .OR. M.EQ.0 )
     $   RETURN
*
*     Solve L*X = B(1:m,:)
*
      CALL ZTRSM( 'Left', 'Lower', 'No transpose', 'Non-unit', M, NRHS,
     $            ONE, A, LDA, B, LDB )
*
*     Set B(m+1:n,:) to zero
*
      IF( M.LT.N )
     $   CALL ZLAZRO( N-M, NRHS, ZERO, ZERO, B( M+1, 1 ), LDB )
*
*     B := Q' * B
*
      CALL ZUNMLQ( 'Left', 'Conjugate transpose', N, NRHS, M, A, LDA,
     $             TAU, B, LDB, WORK, LWORK, INFO )
*
      RETURN
*
*     End of ZGELQS
*
      END