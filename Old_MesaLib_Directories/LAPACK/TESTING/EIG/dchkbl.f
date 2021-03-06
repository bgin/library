      SUBROUTINE DCHKBL( NIN, NOUT )
*
*  -- LAPACK test routine (version 1.1) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
      INTEGER            NIN, NOUT
*     ..
*
*  Purpose
*  =======
*
*  DCHKBL tests DGEBAL, a routine for balancing a general real
*  matrix and isolating some of its eigenvalues.
*
*  Arguments
*  =========
*
*  NIN     (input) INTEGER
*          The logical unit number for input.  NIN > 0.
*
*  NOUT    (input) INTEGER
*          The logical unit number for output.  NOUT > 0.
*
* ======================================================================
*
*     .. Parameters ..
      INTEGER            LDA
      PARAMETER          ( LDA = 20 )
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, IHI, IHIIN, ILO, ILOIN, INFO, J, KNT, N,
     $                   NINFO
      DOUBLE PRECISION   ANORM, RMAX, VMAX
*     ..
*     .. Local Arrays ..
      INTEGER            LMAX( 3 )
      DOUBLE PRECISION   A( LDA, LDA ), AIN( LDA, LDA ), DUMMY( 1 ),
     $                   SCALE( LDA ), SCALIN( LDA )
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DLANGE
      EXTERNAL           DLANGE
*     ..
*     .. External Subroutines ..
      EXTERNAL           DGEBAL
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX
*     ..
*     .. Executable Statements ..
*
      LMAX( 1 ) = 0
      LMAX( 2 ) = 0
      LMAX( 3 ) = 0
      NINFO = 0
      KNT = 0
      RMAX = ZERO
*
   10 CONTINUE
*
      READ( NIN, FMT = * )N
      IF( N.EQ.0 )
     $   GO TO 70
      DO 20 I = 1, N
         READ( NIN, FMT = * )( A( I, J ), J = 1, N )
   20 CONTINUE
*
      READ( NIN, FMT = * )ILOIN, IHIIN
      DO 30 I = 1, N
         READ( NIN, FMT = * )( AIN( I, J ), J = 1, N )
   30 CONTINUE
      READ( NIN, FMT = * )( SCALIN( I ), I = 1, N )
*
      ANORM = DLANGE( 'M', N, N, A, LDA, DUMMY )
      KNT = KNT + 1
*
      CALL DGEBAL( 'B', N, A, LDA, ILO, IHI, SCALE, INFO )
*
      IF( INFO.NE.0 ) THEN
         NINFO = NINFO + 1
         LMAX( 1 ) = KNT
      END IF
*
      IF( ILO.NE.ILOIN .OR. IHI.NE.IHIIN ) THEN
         NINFO = NINFO + 1
         LMAX( 2 ) = KNT
      END IF
*
      VMAX = ZERO
      DO 50 I = 1, N
         DO 40 J = 1, N
            VMAX = MAX( VMAX, ABS( A( I, J )-AIN( I, J ) ) )
   40    CONTINUE
   50 CONTINUE
*
      DO 60 I = 1, N
         VMAX = MAX( VMAX, ABS( SCALE( I )-SCALIN( I ) ) )
   60 CONTINUE
*
      VMAX = VMAX / ANORM
*
      IF( VMAX.GT.RMAX ) THEN
         LMAX( 3 ) = KNT
         RMAX = VMAX
      END IF
*
      GO TO 10
*
   70 CONTINUE
*
      WRITE( NOUT, FMT = 9999 )
 9999 FORMAT( 1X, '.. test output of DGEBAL .. ' )
*
      WRITE( NOUT, FMT = 9998 )RMAX
 9998 FORMAT( 1X, 'value of largest test error             = ', D12.3 )
      WRITE( NOUT, FMT = 9997 )LMAX( 1 )
 9997 FORMAT( 1X, 'example number where info is not zero   = ', I4 )
      WRITE( NOUT, FMT = 9996 )LMAX( 2 )
 9996 FORMAT( 1X, 'example number where ILO or IHI wrong   = ', I4 )
      WRITE( NOUT, FMT = 9995 )LMAX( 3 )
 9995 FORMAT( 1X, 'example number having largest error     = ', I4 )
      WRITE( NOUT, FMT = 9994 )NINFO
 9994 FORMAT( 1X, 'number of examples where info is not 0  = ', I4 )
      WRITE( NOUT, FMT = 9993 )KNT
 9993 FORMAT( 1X, 'total number of examples tested         = ', I4 )
*
      RETURN
*
*     End of DCHKBL
*
      END
