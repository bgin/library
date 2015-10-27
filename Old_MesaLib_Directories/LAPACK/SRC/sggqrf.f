      SUBROUTINE SGGQRF( N, M, P, A, LDA, TAUA, B, LDB, TAUB, WORK,
     $                   LWORK, INFO )
*
*  -- LAPACK routine (version  1.1) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     March 31, 1993
*
*     .. Scalar Arguments ..
      INTEGER            INFO, LDA, LDB, LWORK, M, N, P
*     ..
*     .. Array Arguments ..
      REAL               A( LDA, * ), B( LDB, * ), TAUA( * ), TAUB( * ),
     $                   WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  SGGQRF computes a generalized QR factorization of an N-by-M matrix A
*  and an N-by-P matrix B:
*
*              A = Q*R,        B = Q*T*Z,
*
*  where Q is an N-by-N orthogonal matrix, Z is a P-by-P orthogonal
*  matrix, and R and T assumes one of the forms:
*
*  if N >= M,   R = ( R11 ) M  ,   or if N < M, R = ( R11  R12 ) N
*                   (  0  ) N-M                        N   M-N
*                      M
*
*  where R11 is an upper triangular matrix, and
*
*  if N <= P,  T = ( 0  T12 ) N,   or if N > P, T = ( T11 ) N-P
*                   P-N  N                          ( T21 ) P
*                                                      P
*  where T12 or T21 is a P-by-P upper triangular matrix.
*
*  In particular, if B is square and nonsingular, the GQR factorization
*  of A and B implicitly gives the QR factorization of inv(B)*A:
*
*               inv(B)*A = Z'*(inv(T)*R)
*
*  where inv(B) denotes the inverse of the matrix B, Z' denotes the
*  transpose of matrix Z.
*
*  Arguments
*  =========
*
*  N       (input) INTEGER
*          The number of rows of the matrices A and B. N >= 0.
*
*  M       (input) INTEGER
*          The number of columns of the matrix A.  M >= 0.
*
*  P       (input) INTEGER
*          The number of columns of the matrix B.  P >= 0.
*
*  A       (input/output) REAL array, dimension (LDA,M)
*          On entry, the N-by-M matrix A.
*          On exit, the elements on and above the diagonal of the array
*          contain the min(N,M)-by-M upper trapezoidal matrix R (R is
*          upper triangular if N >= M); the elements below the diagonal,
*          with the array TAUA, represent the orthogonal matrix Q as a
*          product of min(N,M) elementary reflectors (see Further
*          Details).
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A. LDA >= MAX(1,N).
*
*  TAUA    (output) REAL array, dimension (MIN(N,M))
*          The scalar factors of the elementary reflectors (see Further
*          Details).
*
*  B       (input/output) REAL array, dimension (LDB,P)
*          On entry, the N-by-P matrix B.
*          On exit, if N <= P, the upper triangle of the subarray
*          B(1:N,P-N+1:P) contains the N-by-N upper triangular matrix T;
*          if N > P, the elements on and above the (N-P)-th subdiagonal
*          contain the N-by-P upper trapezoidal matrix T; the remaining
*          elements, with the array TAUB, represent the orthogonal
*          matrix Z as a product of elementary reflectors (see Further
*          Details).
*
*  LDB     (input) INTEGER
*          The leading dimension of the array B. LDB >= max(1,N).
*
*  TAUB    (output) REAL array, dimension (MIN(N,P))
*          The scalar factors of the elementary reflectors (see Further
*          Details).
*
*  WORK    (workspace) REAL array, dimension (LWORK)
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The dimension fo the array WORK. LWORK >= MAX(1,N,M,P).
*          For optimum performance LWORK >=
*          MAX(1,N,M,P)*MAX(NB1,NB2,NB3), where NB1 is the optimal
*          blocksize for the QR factorization of an N-by-M matrix A.
*          NB2 is the optimal blocksize for the RQ factorization of an
*          N-by-P matrix B.  NB3 is the optimal blocksize for calling
*          SORMQR.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value.
*
*  Further Details
*  ===============
*
*  The matrix Q is represented as a product of elementary reflectors
*
*     Q = H(1) H(2) . . . H(k), where k = min(N,M).
*
*  Each H(i) has the form
*
*     H(i) = I - taua * v * v'
*
*  where taua is a real scalar, and v is a real vector with
*  v(1:i-1) = 0 and v(i) = 1; v(i+1:N) is stored on exit in A(i+1:N,i),
*  and taua in TAUA(i).
*  To form Q explicitly, use LAPACK subroutine SORGQR.
*  To use Q to update another matrix, use LAPACK subroutine SORMQR.
*
*  The matrix Z is represented as a product of elementary reflectors
*
*     Z = H(1) H(2) . . . H(k), where k = min(N,P).
*
*  Each H(i) has the form
*
*     H(i) = I - taub * v * v'
*
*  where taub is a real scalar, and v is a real vector with
*  v(P-k+i+1:P) = 0 and v(P-k+i) = 1; v(1:P-k+i-1) is stored on exit in
*  B(N-k+i,1:P-k+i-1), and taub in TAUB(i).
*  To form Z explicitly, use LAPACK subroutine SORGRQ.
*  To use Z to update another matrix, use LAPACK subroutine SORMRQ.
*
*  ====================================================================
*
*     .. External Subroutines ..
      EXTERNAL           SGEQRF, SGERQF, SORMQR, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters
*
      INFO = 0
      IF( N.LT.0 ) THEN
         INFO = -1
      ELSE IF( M.LT.0 ) THEN
         INFO = -2
      ELSE IF( P.LT.0 ) THEN
         INFO = -3
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -5
      ELSE IF( LDB.LT.MAX( 1, N ) ) THEN
         INFO = -8
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'SGGQRF', -INFO )
         RETURN
      END IF
*
*     QR factorization of N by M matrix A: A = Q*R
*
      CALL SGEQRF( N, M, A, LDA, TAUA, WORK, LWORK, INFO )
*
*     Multiply Q' to B from the left: B = Q'*B.
*
      CALL SORMQR( 'Left', 'Transpose', N, P, MIN( N, M ), A, LDA, TAUA,
     $             B, LDB, WORK, LWORK, INFO )
*
*     RQ factorization of N by P matrix B: B = T*Z.
*
      CALL SGERQF( N, P, B, LDB, TAUB, WORK, LWORK, INFO )
*
      RETURN
*
*     End of SGGQRF
*
      END
