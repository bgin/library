      SUBROUTINE DLASUM( TYPE, IOUNIT, IE, NRUN )
*
*  -- LAPACK auxiliary test routine (version 1.1) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
      CHARACTER*3        TYPE
      INTEGER            IE, IOUNIT, NRUN
*     ..
*
*  Purpose
*  =======
*
*  DLASUM prints a summary of the results from one of the test routines.
*
*  =====================================================================
*
*     .. Executable Statements ..
*
      IF( IE.GT.0 ) THEN
         WRITE( IOUNIT, FMT = 9999 )TYPE, ': ', IE, ' out of ', NRUN,
     $      ' tests failed to pass the threshold'
      ELSE
         WRITE( IOUNIT, FMT = 9998 )'All tests for ', TYPE,
     $      ' passed the threshold (', NRUN, ' tests run)'
      END IF
 9999 FORMAT( 1X, A3, A2, I4, A8, I4, A35 )
 9998 FORMAT( / 1X, A14, A3, A23, I4, A11 )
      RETURN
*
*     End of DLASUM
*
      END