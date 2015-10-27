      PROGRAM ZCHKEE
*
*  -- LAPACK test routine (version 1.1) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     March 31, 1993
*
*  Purpose
*  =======
*
*  ZCHKEE tests the COMPLEX*16 LAPACK subroutines for the matrix
*  eigenvalue problem.  The test paths in this version are
*
*  NEP (Nonsymmetric Eigenvalue Problem):
*      Test ZGEHRD, ZUNGHR, ZHSEQR, ZTREVC, ZHSEIN, and ZUNMHR
*
*  SEP (Hermitian Eigenvalue Problem):
*      Test ZHETRD, ZUNGTR, ZSTEQR, ZSTERF, and ZSTEIN
*
*  SVD (Singular Value Decomposition):
*      Test ZGEBRD, ZUNGBR, and ZBDSQR
*      and the driver routine ZGESVD
*
*  ZEV (Nonsymmetric Eigenvalue/eigenvector Driver):
*      Test ZGEEV
*
*  ZES (Nonsymmetric Schur form Driver):
*      Test ZGEES
*
*  ZVX (Nonsymmetric Eigenvalue/eigenvector Expert Driver):
*      Test ZGEEVX
*
*  ZSX (Nonsymmetric Schur form Expert Driver):
*      Test ZGEESX
*
*  ZGG (Generalized Nonsymmetric Eigenvalue Problem):
*      Test ZGGHRD, ZGGBAL, ZGGBAK, ZHGEQZ, and ZTGEVC
*      and the driver routines ZGEGS and ZGEGV
*
*  ZSG (Hermitian Generalized Eigenvalue Problem):
*      Test ZHEGST, ZHEGV, ZHPGST, and ZHPGV
*
*  ZHB (Hermitian Band Eigenvalue Problem):
*      Test ZHBTRD
*
*  ZEC (Eigencondition estimation):
*      Test ZTRSYL, ZTREXC, ZTRSNA, and ZTRSEN
*
*  ZBL (Balancing a general matrix)
*      Test ZGEBAL
*
*  ZBK (Back transformation on a balanced matrix)
*      Test ZGEBAK
*
*  GLM (Generalized Linear Regression Model):
*      Tests ZGGGLM
*
*  GQR (Generalized QR and RQ factorizations):
*      Tests ZGGQRF and ZGGRQF
*
*  GSV (Generalized Singular Value Decomposition):
*      Tests ZGGSVD, ZGGSVP, ZTGSJA, ZLAGS2, ZLAPLL, and ZLAPMT
*
*  LSE (Constrained Linear Least Squares):
*      Tests ZGGLSE
*
*  Each test path has a different set of inputs, but the data sets for
*  the driver routines xEV, xES, xVX, and xSX can be concatenated in a
*  single input file.  The first line of input should contain one of the
*  3-character path names in columns 1-3 (except for ZBL and ZBK; see
*  below).  The number of remaining lines depends on what is found on
*  the first line.
*
*  The number of matrix types used in testing is often controllable from
*  the input file.  The number of matrix types for each path, and the
*  test routine that describes them, is as follows:
*
*  Path name(s)  Types    Test routine
*
*  ZHS or NEP      21     ZCHKHS
*  ZST or SEP      21     ZCHKST (routines)
*                  18     ZDRVST (drivers)
*  ZBD or SVD      16     ZCHKBD (routines)
*                   5     ZDRVBD (drivers)
*  ZEV             21     ZDRVEV
*  ZES             21     ZDRVES
*  ZVX             21     ZDRVVX
*  ZSX             21     ZDRVSX
*  ZGG             26     ZCHKGG (routines)
*                  26     ZDRVGG (drivers)
*  ZSG             21     ZDRVSG
*  ZHB             15     ZCHKHB
*  ZEC              -     ZCHKEC
*  ZBL              -     ZCHKBL
*  ZBK              -     ZCHKBK
*  GLM              8     ZCKGLM
*  GQR              8     ZCKGQR
*  GSV              8     ZCKGSV
*  LSE              8     ZCKLSE
*
*-----------------------------------------------------------------------
*
*  NEP input file:
*
*  line 2:  NN, INTEGER
*           Number of values of N.
*
*  line 3:  NVAL, INTEGER array, dimension (NN)
*           The values for the matrix dimension N.
*
*  line 4:  NPARMS, INTEGER
*           Number of values of the parameters NB, NBMIN, NX, NS, and
*           MAXB.
*
*  line 5:  NBVAL, INTEGER array, dimension (NPARMS)
*           The values for the blocksize NB.
*
*  line 6:  NBMIN, INTEGER array, dimension (NPARMS)
*           The values for the minimum blocksize NBMIN.
*
*  line 7:  NXVAL, INTEGER array, dimension (NPARMS)
*           The values for the crossover point NX.
*
*  line 8:  NSVAL, INTEGER array, dimension (NPARMS)
*           The values for the number of shifts.
*
*  line 9:  MXBVAL, INTEGER array, dimension (NPARMS)
*           The values for MAXB, used in determining minimum blocksize.
*
*  line 10: THRESH
*           Threshold value for the test ratios.  Information will be
*           printed about each test for which the test ratio is greater
*           than or equal to the threshold.  To have all of the test
*           ratios printed, use THRESH = 0.0 .
*
*  line 11: NEWSD, INTEGER
*           A code indicating how to set the random number seed.
*           = 0:  Set the seed to a default value before each run
*           = 1:  Initialize the seed to a default value only before the
*                 first run
*           = 2:  Like 1, but use the seed values on the next line
*
*  If line 11 was 2:
*
*  line 12: INTEGER array, dimension (4)
*           Four integer values for the random number seed.
*
*  lines 12-EOF:  The remaining lines occur in sets of 1 or 2 and allow
*           the user to specify the matrix types.  Each line contains
*           a 3-character path name in columns 1-3, and the number
*           of matrix types must be the first nonblank item in columns
*           4-80.  If the number of matrix types is at least 1 but is
*           less than the maximum number of possible types, a second
*           line will be read to get the numbers of the matrix types to
*           be used.  For example,
*  NEP 21
*           requests all of the matrix types for the nonsymmetric
*           eigenvalue problem, while
*  NEP  4
*  9 10 11 12
*           requests only matrices of type 9, 10, 11, and 12.
*
*           The valid 3-character path names are 'NEP' or 'ZHS' for the
*           nonsymmetric eigenvalue routines.
*
*-----------------------------------------------------------------------
*
*  SEP or ZSG input file:
*
*  line 2:  NN, INTEGER
*           Number of values of N.
*
*  line 3:  NVAL, INTEGER array, dimension (NN)
*           The values for the matrix dimension N.
*
*  line 4:  NPARMS, INTEGER
*           Number of values of the parameters NB, NBMIN, and NX.
*
*  line 5:  NBVAL, INTEGER array, dimension (NPARMS)
*           The values for the blocksize NB.
*
*  line 6:  NBMIN, INTEGER array, dimension (NPARMS)
*           The values for the minimum blocksize NBMIN.
*
*  line 7:  NXVAL, INTEGER array, dimension (NPARMS)
*           The values for the crossover point NX.
*
*  line 8:  THRESH
*           Threshold value for the test ratios.  Information will be
*           printed about each test for which the test ratio is greater
*           than or equal to the threshold.
*
*  line 9:  TSTCHK, LOGICAL
*           Flag indicating whether or not to test the LAPACK routines.
*
*  line 10: TSTDRV, LOGICAL
*           Flag indicating whether or not to test the driver routines.
*
*  line 11: TSTERR, LOGICAL
*           Flag indicating whether or not to test the error exits for
*           the LAPACK routines and driver routines.
*
*  line 12: NEWSD, INTEGER
*           A code indicating how to set the random number seed.
*           = 0:  Set the seed to a default value before each run
*           = 1:  Initialize the seed to a default value only before the
*                 first run
*           = 2:  Like 1, but use the seed values on the next line
*
*  If line 12 was 2:
*
*  line 13: INTEGER array, dimension (4)
*           Four integer values for the random number seed.
*
*  lines 13-EOF:  Lines specifying matrix types, as for NEP.
*           The valid 3-character path names are 'SEP' or 'ZST' for the
*           Hermitian eigenvalue routines and driver routines, and
*           'ZSG' for the routines for the Hermitian generalized
*           eigenvalue problem.
*
*-----------------------------------------------------------------------
*
*  SVD input file:
*
*  line 2:  NN, INTEGER
*           Number of values of M and N.
*
*  line 3:  MVAL, INTEGER array, dimension (NN)
*           The values for the matrix row dimension M.
*
*  line 4:  NVAL, INTEGER array, dimension (NN)
*           The values for the matrix column dimension N.
*
*  line 5:  NPARMS, INTEGER
*           Number of values of the parameter NB, NBMIN, NX, and NRHS.
*
*  line 6:  NBVAL, INTEGER array, dimension (NPARMS)
*           The values for the blocksize NB.
*
*  line 7:  NBMIN, INTEGER array, dimension (NPARMS)
*           The values for the minimum blocksize NBMIN.
*
*  line 8:  NXVAL, INTEGER array, dimension (NPARMS)
*           The values for the crossover point NX.
*
*  line 9:  NSVAL, INTEGER array, dimension (NPARMS)
*           The values for the number of right hand sides NRHS.
*
*  line 10: THRESH
*           Threshold value for the test ratios.  Information will be
*           printed about each test for which the test ratio is greater
*           than or equal to the threshold.
*
*  line 11: TSTCHK, LOGICAL
*           Flag indicating whether or not to test the LAPACK routines.
*
*  line 12: TSTDRV, LOGICAL
*           Flag indicating whether or not to test the driver routines.
*
*  line 13: TSTERR, LOGICAL
*           Flag indicating whether or not to test the error exits for
*           the LAPACK routines and driver routines.
*
*  line 14: NEWSD, INTEGER
*           A code indicating how to set the random number seed.
*           = 0:  Set the seed to a default value before each run
*           = 1:  Initialize the seed to a default value only before the
*                 first run
*           = 2:  Like 1, but use the seed values on the next line
*
*  If line 14 was 2:
*
*  line 15: INTEGER array, dimension (4)
*           Four integer values for the random number seed.
*
*  lines 15-EOF:  Lines specifying matrix types, as for NEP.
*           The 3-character path names are 'SVD' or 'ZBD' for both the
*           SVD routines and the SVD driver routines.
*
*-----------------------------------------------------------------------
*
*  ZEV and ZES data files:
*
*  line 1:  'ZEV' or 'ZES' in columns 1 to 3.
*
*  line 2:  NSIZES, INTEGER
*           Number of sizes of matrices to use. Should be at least 0
*           and at most 20. If NSIZES = 0, no testing is done
*           (although the remaining  3 lines are still read).
*
*  line 3:  NN, INTEGER array, dimension(NSIZES)
*           Dimensions of matrices to be tested.
*
*  line 4:  NB, NBMIN, NX, NS, NBCOL, INTEGERs
*           These integer parameters determine how blocking is done
*           (see ILAENV for details)
*           NB     : block size
*           NBMIN  : minimum block size
*           NX     : minimum dimension for blocking
*           NS     : number of shifts in xHSEQR
*           NBCOL  : minimum column dimension for blocking
*
*  line 5:  THRESH, DOUBLE PRECISION
*           The test threshold against which computed residuals are
*           compared. Should generally be in the range from 10. to 20.
*           If it is 0., all test case data will be printed.
*
*  line 6:  NEWSD, INTEGER
*           A code indicating how to set the random number seed.
*           = 0:  Set the seed to a default value before each run
*           = 1:  Initialize the seed to a default value only before the
*                 first run
*           = 2:  Like 1, but use the seed values on the next line
*
*  If line 6 was 2:
*
*  line 7:  INTEGER array, dimension (4)
*           Four integer values for the random number seed.
*
*  lines 8 and following:  Lines specifying matrix types, as for NEP.
*           The 3-character path name is 'ZEV' to test ZGEEV, or
*           'ZES' to test ZGEES.
*
*-----------------------------------------------------------------------
*
*  The ZVX data has two parts. The first part is identical to ZEV,
*  and the second part consists of test matrices with precomputed
*  solutions.
*
*  line 1:  'ZVX' in columns 1-3.
*
*  line 2:  NSIZES, INTEGER
*           If NSIZES = 0, no testing of randomly generated examples
*           is done, but any precomputed examples are tested.
*
*  line 3:  NN, INTEGER array, dimension(NSIZES)
*
*  line 4:  NB, NBMIN, NX, NS, NBCOL, INTEGERs
*
*  line 5:  THRESH, DOUBLE PRECISION
*
*  line 6:  NEWSD, INTEGER
*
*  If line 6 was 2:
*
*  line 7:  INTEGER array, dimension (4)
*
*  lines 8 and following: The first line contains 'ZVX' in columns 1-3
*           followed by the number of matrix types, possibly with
*           a second line to specify certain matrix types.
*           If the number of matrix types = 0, no testing of randomly
*           generated examples is done, but any precomputed examples
*           are tested.
*
*  remaining lines : Each matrix is stored on 1+N+N**2 lines, where N is
*           its dimension. The first line contains the dimension N and
*           ISRT (two integers). ISRT indicates whether the last N lines
*           are sorted by increasing real part of the eigenvalue
*           (ISRT=0) or by increasing imaginary part (ISRT=1). The next
*           N**2 lines contain the matrix rowwise, one entry per line.
*           The last N lines correspond to each eigenvalue. Each of
*           these last N lines contains 4 real values: the real part of
*           the eigenvalues, the imaginary part of the eigenvalue, the
*           reciprocal condition number of the eigenvalues, and the
*           reciprocal condition number of the vector eigenvector. The
*           end of data is indicated by dimension N=0. Even if no data
*           is to be tested, there must be at least one line containing
*           N=0.
*
*-----------------------------------------------------------------------
*
*  The ZSX data is like ZVX. The first part is identical to ZEV, and the
*  second part consists of test matrices with precomputed solutions.
*
*  line 1:  'ZSX' in columns 1-3.
*
*  line 2:  NSIZES, INTEGER
*           If NSIZES = 0, no testing of randomly generated examples
*           is done, but any precomputed examples are tested.
*
*  line 3:  NN, INTEGER array, dimension(NSIZES)
*
*  line 4:  NB, NBMIN, NX, NS, NBCOL, INTEGERs
*
*  line 5:  THRESH, DOUBLE PRECISION
*
*  line 6:  NEWSD, INTEGER
*
*  If line 6 was 2:
*
*  line 7:  INTEGER array, dimension (4)
*
*  lines 8 and following: The first line contains 'ZSX' in columns 1-3
*           followed by the number of matrix types, possibly with
*           a second line to specify certain matrix types.
*           If the number of matrix types = 0, no testing of randomly
*           generated examples is done, but any precomputed examples
*           are tested.
*
*  remaining lines : Each matrix is stored on 3+N**2 lines, where N is
*           its dimension. The first line contains the dimension N, the
*           dimension M of an invariant subspace, and ISRT. The second
*           line contains M integers, identifying the eigenvalues in the
*           invariant subspace (by their position in a list of
*           eigenvalues ordered by increasing real part (if ISRT=0) or
*           by increasing imaginary part (if ISRT=1)). The next N**2
*           lines contain the matrix rowwise. The last line contains the
*           reciprocal condition number for the average of the selected
*           eigenvalues, and the reciprocal condition number for the
*           corresponding right invariant subspace. The end of data in
*           indicated by a line containing N=0, M=0, and ISRT = 0.  Even
*           if no data is to be tested, there must be at least one line
*           containing N=0, M=0 and ISRT=0.
*
*-----------------------------------------------------------------------
*
*  ZGG input file:
*
*  line 2:  NN, INTEGER
*           Number of values of N.
*
*  line 3:  NVAL, INTEGER array, dimension (NN)
*           The values for the matrix dimension N.
*
*  line 4:  NPARMS, INTEGER
*           Number of values of the parameters NB, NBMIN, NBCOL, NS, and
*           MAXB.
*
*  line 5:  NBVAL, INTEGER array, dimension (NPARMS)
*           The values for the blocksize NB.
*
*  line 6:  NBMIN, INTEGER array, dimension (NPARMS)
*           The values for NBMIN, the minimum row dimension for blocks.
*
*  line 7:  NSVAL, INTEGER array, dimension (NPARMS)
*           The values for the number of shifts.
*
*  line 8:  MXBVAL, INTEGER array, dimension (NPARMS)
*           The values for MAXB, used in determining minimum blocksize.
*
*  line 9:  NBCOL, INTEGER array, dimension (NPARMS)
*           The values for NBCOL, the minimum column dimension for
*           blocks.
*
*  line 10: THRESH
*           Threshold value for the test ratios.  Information will be
*           printed about each test for which the test ratio is greater
*           than or equal to the threshold.
*
*  line 11: TSTCHK, LOGICAL
*           Flag indicating whether or not to test the LAPACK routines.
*
*  line 12: TSTDRV, LOGICAL
*           Flag indicating whether or not to test the driver routines.
*
*  line 13: TSTERR, LOGICAL
*           Flag indicating whether or not to test the error exits for
*           the LAPACK routines and driver routines.
*
*  line 14: NEWSD, INTEGER
*           A code indicating how to set the random number seed.
*           = 0:  Set the seed to a default value before each run
*           = 1:  Initialize the seed to a default value only before the
*                 first run
*           = 2:  Like 1, but use the seed values on the next line
*
*  If line 14 was 2:
*
*  line 15: INTEGER array, dimension (4)
*           Four integer values for the random number seed.
*
*  lines 16-EOF:  Lines specifying matrix types, as for NEP.
*           The 3-character path name is 'ZGG' for the generalized
*           eigenvalue problem routines and driver routines.
*
*-----------------------------------------------------------------------
*
*  ZHB input file:
*
*  line 2:  NN, INTEGER
*           Number of values of N.
*
*  line 3:  NVAL, INTEGER array, dimension (NN)
*           The values for the matrix dimension N.
*
*  line 4:  NK, INTEGER
*           Number of values of K.
*
*  line 5:  KVAL, INTEGER array, dimension (NK)
*           The values for the matrix dimension K.
*
*  line 6:  THRESH
*           Threshold value for the test ratios.  Information will be
*           printed about each test for which the test ratio is greater
*           than or equal to the threshold.
*
*  line 7:  NEWSD, INTEGER
*           A code indicating how to set the random number seed.
*           = 0:  Set the seed to a default value before each run
*           = 1:  Initialize the seed to a default value only before the
*                 first run
*           = 2:  Like 1, but use the seed values on the next line
*
*  If line 7 was 2:
*
*  line 8:  INTEGER array, dimension (4)
*           Four integer values for the random number seed.
*
*  lines 8-EOF:  Lines specifying matrix types, as for NEP.
*           The 3-character path name is 'ZHB'.
*
*-----------------------------------------------------------------------
*
*  ZEC input file:
*
*  line  2: THRESH, DOUBLE PRECISION
*           Threshold value for the test ratios.  Information will be
*           printed about each test for which the test ratio is greater
*           than or equal to the threshold.
*
*  lines  3-EOF:
*
*  Input for testing the eigencondition routines consists of a set of
*  specially constructed test cases and their solutions.  The data
*  format is not intended to be modified by the user.
*
*-----------------------------------------------------------------------
*
*  ZBL and ZBK input files:
*
*  line 1:  'ZGEBAL' in columns 1-6 to test ZGEBAL, or 'ZGEBAK in
*           columns 1-6 to test ZGEBAK.
*
*  The remaining lines consist of specially constructed test cases.
*
*-----------------------------------------------------------------------
*
*  GLM data file:
*
*  line 1:  'GLM' in columns 1 to 3.
*
*  line 2:  NN, INTEGER
*           Number of values of M, P, and N.
*
*  line 3:  MVAL, INTEGER array, dimension(NN)
*           Values of M (row dimension).
*
*  line 4:  PVAL, INTEGER array, dimension(NN)
*           Values of P (row dimension).
*
*  line 5:  NVAL, INTEGER array, dimension(NN)
*           Values of N (column dimension), note M <= N <= M+P.
*
*  line 6:  THRESH, DOUBLE PRECISION
*           Threshold value for the test ratios.  Information will be
*           printed about each test for which the test ratio is greater
*           than or equal to the threshold.
*
*  line 7:  TSTERR, LOGICAL
*           Flag indicating whether or not to test the error exits for
*           the LAPACK routines and driver routines.
*
*  line 8:  NEWSD, INTEGER
*           A code indicating how to set the random number seed.
*           = 0:  Set the seed to a default value before each run
*           = 1:  Initialize the seed to a default value only before the
*                 first run
*           = 2:  Like 1, but use the seed values on the next line
*
*  If line 8 was 2:
*
*  line 9:  INTEGER array, dimension (4)
*           Four integer values for the random number seed.
*
*  lines 9-EOF:  Lines specifying matrix types, as for NEP.
*           The 3-character path name is 'GLM' for the generalized
*           linear regression model routines.
*
*-----------------------------------------------------------------------
*
*  GQR data file:
*
*  line 1:  'GQR' in columns 1 to 3.
*
*  line 2:  NN, INTEGER
*           Number of values of M, P, and N.
*
*  line 3:  MVAL, INTEGER array, dimension(NN)
*           Values of M.
*
*  line 4:  PVAL, INTEGER array, dimension(NN)
*           Values of P.
*
*  line 5:  NVAL, INTEGER array, dimension(NN)
*           Values of N.
*
*  line 6:  THRESH, DOUBLE PRECISION
*           Threshold value for the test ratios.  Information will be
*           printed about each test for which the test ratio is greater
*           than or equal to the threshold.
*
*  line 7:  TSTERR, LOGICAL
*           Flag indicating whether or not to test the error exits for
*           the LAPACK routines and driver routines.
*
*  line 8:  NEWSD, INTEGER
*           A code indicating how to set the random number seed.
*           = 0:  Set the seed to a default value before each run
*           = 1:  Initialize the seed to a default value only before the
*                 first run
*           = 2:  Like 1, but use the seed values on the next line
*
*  If line 8 was 2:
*
*  line 9:  INTEGER array, dimension (4)
*           Four integer values for the random number seed.
*
*  lines 9-EOF:  Lines specifying matrix types, as for NEP.
*           The 3-character path name is 'GQR' for the generalized
*           QR and RQ routines.
*
*-----------------------------------------------------------------------
*
*  GSV data file:
*
*  line 1:  'GSV' in columns 1 to 3.
*
*  line 2:  NN, INTEGER
*           Number of values of M, P, and N.
*
*  line 3:  MVAL, INTEGER array, dimension(NN)
*           Values of M (row dimension).
*
*  line 4:  PVAL, INTEGER array, dimension(NN)
*           Values of P (row dimension).
*
*  line 5:  NVAL, INTEGER array, dimension(NN)
*           Values of N (column dimension).
*
*  line 6:  THRESH, DOUBLE PRECISION
*           Threshold value for the test ratios.  Information will be
*           printed about each test for which the test ratio is greater
*           than or equal to the threshold.
*
*  line 7:  TSTERR, LOGICAL
*           Flag indicating whether or not to test the error exits for
*           the LAPACK routines and driver routines.
*
*  line 8:  NEWSD, INTEGER
*           A code indicating how to set the random number seed.
*           = 0:  Set the seed to a default value before each run
*           = 1:  Initialize the seed to a default value only before the
*                 first run
*           = 2:  Like 1, but use the seed values on the next line
*
*  If line 8 was 2:
*
*  line 9:  INTEGER array, dimension (4)
*           Four integer values for the random number seed.
*
*  lines 9-EOF:  Lines specifying matrix types, as for NEP.
*           The 3-character path name is 'GSV' for the generalized
*           SVD routines.
*
*-----------------------------------------------------------------------
*
*  LSE data file:
*
*  line 1:  'LSE' in columns 1 to 3.
*
*  line 2:  NN, INTEGER
*           Number of values of M, P, and N.
*
*  line 3:  MVAL, INTEGER array, dimension(NN)
*           Values of M.
*
*  line 4:  PVAL, INTEGER array, dimension(NN)
*           Values of P.
*
*  line 5:  NVAL, INTEGER array, dimension(NN)
*           Values of N, note P <= N <= P+M.
*
*  line 6:  THRESH, DOUBLE PRECISION
*           Threshold value for the test ratios.  Information will be
*           printed about each test for which the test ratio is greater
*           than or equal to the threshold.
*
*  line 7:  TSTERR, LOGICAL
*           Flag indicating whether or not to test the error exits for
*           the LAPACK routines and driver routines.
*
*  line 8:  NEWSD, INTEGER
*           A code indicating how to set the random number seed.
*           = 0:  Set the seed to a default value before each run
*           = 1:  Initialize the seed to a default value only before the
*                 first run
*           = 2:  Like 1, but use the seed values on the next line
*
*  If line 8 was 2:
*
*  line 9:  INTEGER array, dimension (4)
*           Four integer values for the random number seed.
*
*  lines 9-EOF:  Lines specifying matrix types, as for NEP.
*           The 3-character path name is 'GSV' for the generalized
*           SVD routines.
*
*-----------------------------------------------------------------------
*
*  NMAX is currently set to 132 and must be at least 12 for some of the
*  precomputed examples, and LWORK = NMAX*(4*NMAX+3) in the parameter
*  statements below.  For SVD, we assume NRHS may be as big as N.  The
*  parameter NEED is set to 14 to allow for 14 N-by-N matrices for ZGG.
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            NMAX
      PARAMETER          ( NMAX = 132 )
      INTEGER            NEED
      PARAMETER          ( NEED = 14 )
      INTEGER            LWORK
      PARAMETER          ( LWORK = NMAX*( 4*NMAX+3 ) )
      INTEGER            LIWORK
      PARAMETER          ( LIWORK = NMAX*NMAX )
      INTEGER            MAXIN
      PARAMETER          ( MAXIN = 20 )
      INTEGER            MAXT
      PARAMETER          ( MAXT = 25 )
      INTEGER            NIN, NOUT
      PARAMETER          ( NIN = 5, NOUT = 6 )
*     ..
*     .. Local Scalars ..
      LOGICAL            FATAL, GLM, GQR, GSV, LSE, NEP, SEP, SVD,
     $                   TSTCHK, TSTDIF, TSTDRV, TSTERR, ZES, ZEV, ZGG,
     $                   ZHB, ZSX, ZVX
      CHARACTER          C1
      CHARACTER*3        C3, PATH
      CHARACTER*6        SUBNAM, VNAME
      CHARACTER*10       INTSTR
      CHARACTER*80       LINE
      INTEGER            I, I1, IC, INFO, ITMP, K, LENP, MAXTYP, NEWSD,
     $                   NK, NN, NPARMS, NRHS, NTYPES
      DOUBLE PRECISION   EPS, S1, S2, THRESH, THRSHN
*     ..
*     .. Local Arrays ..
      LOGICAL            DOTYPE( MAXT ), LOGWRK( NMAX )
      INTEGER            IOLDSD( 4 ), ISEED( 4 ), IWORK( LIWORK ),
     $                   KVAL( MAXIN ), MVAL( MAXIN ), MXBVAL( MAXIN ),
     $                   NBCOL( MAXIN ), NBMIN( MAXIN ), NBVAL( MAXIN ),
     $                   NSVAL( MAXIN ), NVAL( MAXIN ), NXVAL( MAXIN ),
     $                   PVAL( MAXIN )
      DOUBLE PRECISION   ALPHA( NMAX ), BETA( NMAX ), DR( NMAX, 11 ),
     $                   RESULT( 84 ), RWORK( 8*NMAX )
      COMPLEX*16         A( NMAX*NMAX, NEED ), B( NMAX*NMAX, 5 ),
     $                   DC( NMAX, 6 ), TAUA( NMAX ), TAUB( NMAX ),
     $                   WORK( LWORK ), X( 5*NMAX )
*     ..
*     .. External Functions ..
      LOGICAL            LSAMEN
      DOUBLE PRECISION   DLAMCH, DSECND
      EXTERNAL           LSAMEN, DLAMCH, DSECND
*     ..
*     .. External Subroutines ..
      EXTERNAL           ALAREQ, ZCHKBD, ZCHKBK, ZCHKBL, ZCHKEC, ZCHKGG,
     $                   ZCHKHB, ZCHKHS, ZCHKST, ZCKGLM, ZCKGQR, ZCKGSV,
     $                   ZCKLSE, ZDRVBD, ZDRVES, ZDRVEV, ZDRVGG, ZDRVSG,
     $                   ZDRVST, ZDRVSX, ZDRVVX, ZERRBD, ZERRED, ZERRGG,
     $                   ZERRHS, ZERRST, XLAENV
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          LEN, MIN
*     ..
*     .. Scalars in Common ..
      LOGICAL            LERR, OK
      CHARACTER*6        SRNAMT
      INTEGER            INFOT, MAXB, NPROC, NSHIFT, NUNIT, SELDIM,
     $                   SELOPT
*     ..
*     .. Arrays in Common ..
      LOGICAL            SELVAL( 20 )
      INTEGER            IPARMS( 100 )
      DOUBLE PRECISION   SELWI( 20 ), SELWR( 20 )
*     ..
*     .. Common blocks ..
      COMMON             / CENVIR / NPROC, NSHIFT, MAXB
      COMMON             / CLAENV / IPARMS
      COMMON             / INFOC / INFOT, NUNIT, OK, LERR
      COMMON             / SRNAMC / SRNAMT
      COMMON             / SSLCT / SELOPT, SELDIM, SELVAL, SELWR, SELWI
*     ..
*     .. Data statements ..
      DATA               INTSTR / '0123456789' /
      DATA               IOLDSD / 0, 0, 0, 1 /
*     ..
*     .. Executable Statements ..
*
      S1 = DSECND( )
      FATAL = .FALSE.
      NUNIT = NOUT
*
*     Return to here to read multiple sets of data
*
   10 CONTINUE
*
*     Read the first line and set the 3-character test path
*
      READ( NIN, FMT = '(A80)', END = 350 )LINE
      PATH = LINE( 1: 3 )
      NEP = LSAMEN( 3, PATH, 'NEP' ) .OR. LSAMEN( 3, PATH, 'ZHS' )
      SEP = LSAMEN( 3, PATH, 'SEP' ) .OR. LSAMEN( 3, PATH, 'ZST' ) .OR.
     $      LSAMEN( 3, PATH, 'ZSG' )
      SVD = LSAMEN( 3, PATH, 'SVD' ) .OR. LSAMEN( 3, PATH, 'ZBD' )
      ZEV = LSAMEN( 3, PATH, 'ZEV' )
      ZES = LSAMEN( 3, PATH, 'ZES' )
      ZVX = LSAMEN( 3, PATH, 'ZVX' )
      ZSX = LSAMEN( 3, PATH, 'ZSX' )
      ZGG = LSAMEN( 3, PATH, 'ZGG' )
      ZHB = LSAMEN( 3, PATH, 'ZHB' )
      GLM = LSAMEN( 3, PATH, 'GLM' )
      GQR = LSAMEN( 3, PATH, 'GQR' ) .OR. LSAMEN( 3, PATH, 'GRQ' )
      GSV = LSAMEN( 3, PATH, 'GSV' )
      LSE = LSAMEN( 3, PATH, 'LSE' )
*
*     Report values of parameters.
*
      IF( PATH.EQ.'   ' ) THEN
         GO TO 10
      ELSE IF( NEP ) THEN
         WRITE( NOUT, FMT = 9987 )
      ELSE IF( SEP ) THEN
         WRITE( NOUT, FMT = 9986 )
      ELSE IF( SVD ) THEN
         WRITE( NOUT, FMT = 9985 )
      ELSE IF( ZEV ) THEN
         WRITE( NOUT, FMT = 9979 )
      ELSE IF( ZES ) THEN
         WRITE( NOUT, FMT = 9978 )
      ELSE IF( ZVX ) THEN
         WRITE( NOUT, FMT = 9977 )
      ELSE IF( ZSX ) THEN
         WRITE( NOUT, FMT = 9976 )
      ELSE IF( ZGG ) THEN
         WRITE( NOUT, FMT = 9975 )
      ELSE IF( ZHB ) THEN
         WRITE( NOUT, FMT = 9974 )
      ELSE IF( GLM ) THEN
         WRITE( NOUT, FMT = 9971 )
      ELSE IF( GQR ) THEN
         WRITE( NOUT, FMT = 9970 )
      ELSE IF( GSV ) THEN
         WRITE( NOUT, FMT = 9969 )
      ELSE IF( LSE ) THEN
         WRITE( NOUT, FMT = 9968 )
      ELSE
*
*        Not NEP, SEP, SVD, ZEV, ZES, ZVX, or ZSX:
*        Test for one of the special paths.
*
         SUBNAM = LINE( 1: 6 )
         IF( LSAMEN( 6, SUBNAM, 'ZGEBAL' ) ) THEN
*
*           ZGEBAL:  Balancing
*
            CALL ZCHKBL( NIN, NOUT )
         ELSE IF( LSAMEN( 6, SUBNAM, 'ZGEBAK' ) ) THEN
*
*           ZGEBAK:  Back transformation
*
            CALL ZCHKBK( NIN, NOUT )
         ELSE IF( LSAMEN( 3, PATH, 'ZEC' ) ) THEN
*
*           ZEC:  Eigencondition estimation
*
            READ( NIN, FMT = * )THRESH
            TSTERR = .TRUE.
            CALL ZCHKEC( THRESH, TSTERR, NIN, NOUT )
         ELSE
            WRITE( NOUT, FMT = 9992 )PATH
         END IF
         GO TO 350
      END IF
      WRITE( NOUT, FMT = 9972 )
      WRITE( NOUT, FMT = 9984 )
*
*     Read the number of values of M, P, and N.
*
      READ( NIN, FMT = * )NN
      IF( NN.LT.1 ) THEN
         WRITE( NOUT, FMT = 9989 )'   NN ', NN, 1
         NN = 0
         FATAL = .TRUE.
      ELSE IF( NN.GT.MAXIN ) THEN
         WRITE( NOUT, FMT = 9988 )'   NN ', NN, MAXIN
         NN = 0
         FATAL = .TRUE.
      END IF
*
*     Read the values of M
*
      READ( NIN, FMT = * )( MVAL( I ), I = 1, NN )
      IF( SVD ) THEN
         VNAME = '    M '
      ELSE
         VNAME = '    N '
      END IF
      DO 20 I = 1, NN
         IF( MVAL( I ).LT.0 ) THEN
            WRITE( NOUT, FMT = 9989 )VNAME, MVAL( I ), 0
            FATAL = .TRUE.
         ELSE IF( MVAL( I ).GT.NMAX ) THEN
            WRITE( NOUT, FMT = 9988 )VNAME, MVAL( I ), NMAX
            FATAL = .TRUE.
         END IF
   20 CONTINUE
      WRITE( NOUT, FMT = 9983 )'M:    ', ( MVAL( I ), I = 1, NN )
*
*     Read the values of P
*
      IF( GLM .OR. GQR .OR. GSV .OR. LSE ) THEN
         READ( NIN, FMT = * )( PVAL( I ), I = 1, NN )
         DO 30 I = 1, NN
            IF( PVAL( I ).LT.0 ) THEN
               WRITE( NOUT, FMT = 9989 )' P  ', PVAL( I ), 0
               FATAL = .TRUE.
            ELSE IF( PVAL( I ).GT.NMAX ) THEN
               WRITE( NOUT, FMT = 9988 )' P  ', PVAL( I ), NMAX
               FATAL = .TRUE.
            END IF
   30    CONTINUE
         WRITE( NOUT, FMT = 9983 )'P:    ', ( PVAL( I ), I = 1, NN )
      END IF
*
*     Read the values of N
*
      IF( SVD .OR. GLM .OR. GQR .OR. GSV .OR. LSE ) THEN
         READ( NIN, FMT = * )( NVAL( I ), I = 1, NN )
         DO 40 I = 1, NN
            IF( NVAL( I ).LT.0 ) THEN
               WRITE( NOUT, FMT = 9989 )'    N ', NVAL( I ), 0
               FATAL = .TRUE.
            ELSE IF( NVAL( I ).GT.NMAX ) THEN
               WRITE( NOUT, FMT = 9988 )'    N ', NVAL( I ), NMAX
               FATAL = .TRUE.
            END IF
   40    CONTINUE
      ELSE
         DO 50 I = 1, NN
            NVAL( I ) = MVAL( I )
   50    CONTINUE
      END IF
      WRITE( NOUT, FMT = 9983 )'N:    ', ( NVAL( I ), I = 1, NN )
*
*     Read the number of values of K, followed by the values of K
*
      IF( ZHB ) THEN
         READ( NIN, FMT = * )NK
         READ( NIN, FMT = * )( KVAL( I ), I = 1, NK )
         DO 60 I = 1, NK
            IF( KVAL( I ).LT.0 ) THEN
               WRITE( NOUT, FMT = 9989 )'    K ', KVAL( I ), 0
               FATAL = .TRUE.
            ELSE IF( KVAL( I ).GT.NMAX ) THEN
               WRITE( NOUT, FMT = 9988 )'    K ', KVAL( I ), NMAX
               FATAL = .TRUE.
            END IF
   60    CONTINUE
         WRITE( NOUT, FMT = 9983 )'K:    ', ( KVAL( I ), I = 1, NK )
      END IF
*
      IF( ZEV .OR. ZES .OR. ZVX .OR. ZSX ) THEN
*
*        For the nonsymmetric driver routines, only one set of
*        parameters is allowed.
*
         READ( NIN, FMT = * )NBVAL( 1 ), NBMIN( 1 ), NXVAL( 1 ),
     $      NSVAL( 1 ), MXBVAL( 1 )
         IF( NBVAL( 1 ).LT.1 ) THEN
            WRITE( NOUT, FMT = 9989 )'   NB ', NBVAL( 1 ), 1
            FATAL = .TRUE.
         ELSE IF( NBMIN( 1 ).LT.1 ) THEN
            WRITE( NOUT, FMT = 9989 )'NBMIN ', NBMIN( 1 ), 1
            FATAL = .TRUE.
         ELSE IF( NXVAL( 1 ).LT.1 ) THEN
            WRITE( NOUT, FMT = 9989 )'   NX ', NXVAL( 1 ), 1
            FATAL = .TRUE.
         ELSE IF( NSVAL( 1 ).LT.2 ) THEN
            WRITE( NOUT, FMT = 9989 )'   NS ', NSVAL( 1 ), 2
            FATAL = .TRUE.
         ELSE IF( MXBVAL( 1 ).LT.1 ) THEN
            WRITE( NOUT, FMT = 9989 )' MAXB ', MXBVAL( 1 ), 1
            FATAL = .TRUE.
         END IF
         CALL XLAENV( 1, NBVAL( 1 ) )
         CALL XLAENV( 2, NBMIN( 1 ) )
         CALL XLAENV( 3, NXVAL( 1 ) )
         CALL XLAENV( 4, NSVAL( 1 ) )
         CALL XLAENV( 8, MXBVAL( 1 ) )
         WRITE( NOUT, FMT = 9983 )'NB:   ', NBVAL( 1 )
         WRITE( NOUT, FMT = 9983 )'NBMIN:', NBMIN( 1 )
         WRITE( NOUT, FMT = 9983 )'NX:   ', NXVAL( 1 )
         WRITE( NOUT, FMT = 9983 )'NS:   ', NSVAL( 1 )
         WRITE( NOUT, FMT = 9983 )'MAXB: ', MXBVAL( 1 )
      ELSE IF( .NOT.ZHB .AND. .NOT.GLM .AND. .NOT.GQR .AND. .NOT.
     $         GSV .AND. .NOT.LSE ) THEN
*
*        For the other paths, the number of parameters can be varied
*        from the input file.  Read the number of parameter values.
*
         READ( NIN, FMT = * )NPARMS
         IF( NPARMS.LT.1 ) THEN
            WRITE( NOUT, FMT = 9989 )'NPARMS', NPARMS, 1
            NPARMS = 0
            FATAL = .TRUE.
         ELSE IF( NPARMS.GT.MAXIN ) THEN
            WRITE( NOUT, FMT = 9988 )'NPARMS', NPARMS, MAXIN
            NPARMS = 0
            FATAL = .TRUE.
         END IF
*
*        Read the values of NB
*
         READ( NIN, FMT = * )( NBVAL( I ), I = 1, NPARMS )
         DO 70 I = 1, NPARMS
            IF( NBVAL( I ).LT.0 ) THEN
               WRITE( NOUT, FMT = 9989 )'   NB ', NBVAL( I ), 0
               FATAL = .TRUE.
            ELSE IF( NBVAL( I ).GT.NMAX ) THEN
               WRITE( NOUT, FMT = 9988 )'   NB ', NBVAL( I ), NMAX
               FATAL = .TRUE.
            END IF
   70    CONTINUE
         WRITE( NOUT, FMT = 9983 )'NB:   ',
     $      ( NBVAL( I ), I = 1, NPARMS )
*
*        Read the values of NBMIN
*
         IF( NEP .OR. SEP .OR. SVD .OR. ZGG ) THEN
            READ( NIN, FMT = * )( NBMIN( I ), I = 1, NPARMS )
            DO 80 I = 1, NPARMS
               IF( NBMIN( I ).LT.0 ) THEN
                  WRITE( NOUT, FMT = 9989 )'NBMIN ', NBMIN( I ), 0
                  FATAL = .TRUE.
               ELSE IF( NBMIN( I ).GT.NMAX ) THEN
                  WRITE( NOUT, FMT = 9988 )'NBMIN ', NBMIN( I ), NMAX
                  FATAL = .TRUE.
               END IF
   80       CONTINUE
            WRITE( NOUT, FMT = 9983 )'NBMIN:',
     $         ( NBMIN( I ), I = 1, NPARMS )
         ELSE
            DO 90 I = 1, NPARMS
               NBMIN( I ) = 1
   90       CONTINUE
         END IF
*
*        Read the values of NX
*
         IF( NEP .OR. SEP .OR. SVD ) THEN
            READ( NIN, FMT = * )( NXVAL( I ), I = 1, NPARMS )
            DO 100 I = 1, NPARMS
               IF( NXVAL( I ).LT.0 ) THEN
                  WRITE( NOUT, FMT = 9989 )'   NX ', NXVAL( I ), 0
                  FATAL = .TRUE.
               ELSE IF( NXVAL( I ).GT.NMAX ) THEN
                  WRITE( NOUT, FMT = 9988 )'   NX ', NXVAL( I ), NMAX
                  FATAL = .TRUE.
               END IF
  100       CONTINUE
            WRITE( NOUT, FMT = 9983 )'NX:   ',
     $         ( NXVAL( I ), I = 1, NPARMS )
         ELSE
            DO 110 I = 1, NPARMS
               NXVAL( I ) = 1
  110       CONTINUE
         END IF
*
*        Read the values of NSHIFT (if NEP or ZGG) or NRHS (if SVD).
*
         IF( NEP .OR. SVD .OR. ZGG ) THEN
            READ( NIN, FMT = * )( NSVAL( I ), I = 1, NPARMS )
            DO 120 I = 1, NPARMS
               IF( NSVAL( I ).LT.0 ) THEN
                  WRITE( NOUT, FMT = 9989 )'   NS ', NSVAL( I ), 0
                  FATAL = .TRUE.
               ELSE IF( NSVAL( I ).GT.NMAX ) THEN
                  WRITE( NOUT, FMT = 9988 )'   NS ', NSVAL( I ), NMAX
                  FATAL = .TRUE.
               END IF
  120       CONTINUE
            WRITE( NOUT, FMT = 9983 )'NS:   ',
     $         ( NSVAL( I ), I = 1, NPARMS )
         ELSE
            DO 130 I = 1, NPARMS
               NSVAL( I ) = 1
  130       CONTINUE
         END IF
*
*        Read the values for MAXB.
*
         IF( NEP .OR. ZGG ) THEN
            READ( NIN, FMT = * )( MXBVAL( I ), I = 1, NPARMS )
            DO 140 I = 1, NPARMS
               IF( MXBVAL( I ).LT.0 ) THEN
                  WRITE( NOUT, FMT = 9989 )' MAXB ', MXBVAL( I ), 0
                  FATAL = .TRUE.
               ELSE IF( MXBVAL( I ).GT.NMAX ) THEN
                  WRITE( NOUT, FMT = 9988 )' MAXB ', MXBVAL( I ), NMAX
                  FATAL = .TRUE.
               END IF
  140       CONTINUE
            WRITE( NOUT, FMT = 9983 )'MAXB: ',
     $         ( MXBVAL( I ), I = 1, NPARMS )
         ELSE
            DO 150 I = 1, NPARMS
               MXBVAL( I ) = 1
  150       CONTINUE
         END IF
*
*        Read the values for NBCOL.
*
         IF( ZGG ) THEN
            READ( NIN, FMT = * )( NBCOL( I ), I = 1, NPARMS )
            DO 160 I = 1, NPARMS
               IF( NBCOL( I ).LT.0 ) THEN
                  WRITE( NOUT, FMT = 9989 )'NBCOL ', NBCOL( I ), 0
                  FATAL = .TRUE.
               ELSE IF( NBCOL( I ).GT.NMAX ) THEN
                  WRITE( NOUT, FMT = 9988 )'NBCOL ', NBCOL( I ), NMAX
                  FATAL = .TRUE.
               END IF
  160       CONTINUE
            WRITE( NOUT, FMT = 9983 )'NBCOL:',
     $         ( NBCOL( I ), I = 1, NPARMS )
         ELSE
            DO 170 I = 1, NPARMS
               NBCOL( I ) = 1
  170       CONTINUE
         END IF
      END IF
*
*     Calculate and print the machine dependent constants.
*
      WRITE( NOUT, FMT = * )
      EPS = DLAMCH( 'Underflow threshold' )
      WRITE( NOUT, FMT = 9981 )'underflow', EPS
      EPS = DLAMCH( 'Overflow threshold' )
      WRITE( NOUT, FMT = 9981 )'overflow ', EPS
      EPS = DLAMCH( 'Epsilon' )
      WRITE( NOUT, FMT = 9981 )'precision', EPS
*
*     Read the threshold value for the test ratios.
*
      READ( NIN, FMT = * )THRESH
      WRITE( NOUT, FMT = 9982 )THRESH
      IF( SEP .OR. SVD .OR. ZGG ) THEN
*
*        Read the flag that indicates whether to test LAPACK routines.
*
         READ( NIN, FMT = * )TSTCHK
*
*        Read the flag that indicates whether to test driver routines.
*
         READ( NIN, FMT = * )TSTDRV
      END IF
*
*     Read the flag that indicates whether to test the error exits.
*
      READ( NIN, FMT = * )TSTERR
*
*     Read the code describing how to set the random number seed.
*
      READ( NIN, FMT = * )NEWSD
*
*     If NEWSD = 2, read another line with 4 integers for the seed.
*
      IF( NEWSD.EQ.2 )
     $   READ( NIN, FMT = * )( IOLDSD( I ), I = 1, 4 )
*
      DO 180 I = 1, 4
         ISEED( I ) = IOLDSD( I )
  180 CONTINUE
*
      IF( FATAL ) THEN
         WRITE( NOUT, FMT = 9999 )
         STOP
      END IF
*
*     Read the input lines indicating the test path and its parameters.
*     The first three characters indicate the test path, and the number
*     of test matrix types must be the first nonblank item in columns
*     4-80.
*
  190 CONTINUE
      READ( NIN, FMT = '(A80)', END = 350 )LINE
      C3 = LINE( 1: 3 )
      LENP = LEN( LINE )
      I = 3
      ITMP = 0
      I1 = 0
  200 CONTINUE
      I = I + 1
      IF( I.GT.LENP ) THEN
         IF( I1.GT.0 ) THEN
            GO TO 230
         ELSE
            NTYPES = MAXT
            GO TO 230
         END IF
      END IF
      IF( LINE( I: I ).NE.' ' .AND. LINE( I: I ).NE.',' ) THEN
         I1 = I
         C1 = LINE( I1: I1 )
*
*        Check that a valid integer was read
*
         DO 210 K = 1, 10
            IF( C1.EQ.INTSTR( K: K ) ) THEN
               IC = K - 1
               GO TO 220
            END IF
  210    CONTINUE
         WRITE( NOUT, FMT = 9991 )I, LINE
         GO TO 190
  220    CONTINUE
         ITMP = 10*ITMP + IC
         GO TO 200
      ELSE IF( I1.GT.0 ) THEN
         GO TO 230
      ELSE
         GO TO 200
      END IF
  230 CONTINUE
      NTYPES = ITMP
*
*     Skip the tests if NTYPES is <= 0.
*
      IF( .NOT.( ZEV .OR. ZES .OR. ZVX .OR. ZSX ) .AND. NTYPES.LE.0 )
     $     THEN
         WRITE( NOUT, FMT = 9990 )C3
         GO TO 190
      END IF
*
*     Reset the random number seed.
*
      IF( NEWSD.EQ.0 ) THEN
         DO 240 K = 1, 4
            ISEED( K ) = IOLDSD( K )
  240    CONTINUE
      END IF
*
      IF( LSAMEN( 3, C3, 'ZHS' ) .OR. LSAMEN( 3, C3, 'NEP' ) ) THEN
*
*        -------------------------------------
*        NEP:  Nonsymmetric Eigenvalue Problem
*        -------------------------------------
*        Vary the parameters
*           NB    = block size
*           NBMIN = minimum block size
*           NX    = crossover point
*           NS    = number of shifts
*           MAXB  = minimum submatrix size
*
         MAXTYP = 21
         NTYPES = MIN( MAXTYP, NTYPES )
         CALL ALAREQ( C3, NTYPES, DOTYPE, MAXTYP, NIN, NOUT )
         IF( TSTERR )
     $      CALL ZERRHS( 'ZHSEQR', NOUT )
         DO 260 I = 1, NPARMS
            CALL XLAENV( 1, NBVAL( I ) )
            CALL XLAENV( 2, NBMIN( I ) )
            CALL XLAENV( 3, NXVAL( I ) )
            CALL XLAENV( 4, NSVAL( I ) )
            CALL XLAENV( 8, MXBVAL( I ) )
*
            IF( NEWSD.EQ.0 ) THEN
               DO 250 K = 1, 4
                  ISEED( K ) = IOLDSD( K )
  250          CONTINUE
            END IF
            WRITE( NOUT, FMT = 9998 )C3, NBVAL( I ), NBMIN( I ),
     $         NXVAL( I ), NSVAL( I ), MXBVAL( I )
            CALL ZCHKHS( NN, NVAL, MAXTYP, DOTYPE, ISEED, THRESH, NOUT,
     $                   A( 1, 1 ), NMAX, A( 1, 2 ), A( 1, 3 ),
     $                   A( 1, 4 ), A( 1, 5 ), NMAX, A( 1, 6 ),
     $                   A( 1, 7 ), DC( 1, 1 ), DC( 1, 2 ), A( 1, 8 ),
     $                   A( 1, 9 ), A( 1, 10 ), A( 1, 11 ), A( 1, 12 ),
     $                   DC( 1, 3 ), WORK, LWORK, RWORK, IWORK, LOGWRK,
     $                   RESULT, INFO )
            IF( INFO.NE.0 )
     $         WRITE( NOUT, FMT = 9980 )'ZCHKHS', INFO
  260    CONTINUE
*
      ELSE IF( LSAMEN( 3, C3, 'ZST' ) .OR. LSAMEN( 3, C3, 'SEP' ) ) THEN
*
*        ----------------------------------
*        SEP:  Symmetric Eigenvalue Problem
*        ----------------------------------
*        Vary the parameters
*           NB    = block size
*           NBMIN = minimum block size
*           NX    = crossover point
*
         MAXTYP = 15
         NTYPES = MIN( MAXTYP, NTYPES )
         CALL ALAREQ( C3, NTYPES, DOTYPE, MAXTYP, NIN, NOUT )
         IF( TSTERR )
     $      CALL ZERRST( 'ZSTEQR', NOUT )
         DO 280 I = 1, NPARMS
            CALL XLAENV( 1, NBVAL( I ) )
            CALL XLAENV( 2, NBMIN( I ) )
            CALL XLAENV( 3, NXVAL( I ) )
*
            IF( NEWSD.EQ.0 ) THEN
               DO 270 K = 1, 4
                  ISEED( K ) = IOLDSD( K )
  270          CONTINUE
            END IF
            WRITE( NOUT, FMT = 9997 )C3, NBVAL( I ), NBMIN( I ),
     $         NXVAL( I )
            IF( TSTCHK ) THEN
               CALL ZCHKST( NN, NVAL, MAXTYP, DOTYPE, ISEED, THRESH,
     $                      NOUT, A( 1, 1 ), NMAX, A( 1, 2 ),
     $                      DR( 1, 1 ), DR( 1, 2 ), DR( 1, 3 ),
     $                      DR( 1, 4 ), DR( 1, 5 ), DR( 1, 6 ),
     $                      DR( 1, 7 ), DR( 1, 8 ), DR( 1, 9 ),
     $                      DR( 1, 10 ), DR( 1, 11 ), A( 1, 3 ), NMAX,
     $                      A( 1, 4 ), A( 1, 5 ), DC( 1, 1 ), A( 1, 6 ),
     $                      WORK, LWORK, RWORK, IWORK, RESULT, INFO )
               IF( INFO.NE.0 )
     $            WRITE( NOUT, FMT = 9980 )'ZCHKST', INFO
            END IF
            IF( TSTDRV ) THEN
               CALL ZDRVST( NN, NVAL, MAXTYP, DOTYPE, ISEED, THRESH,
     $                      NOUT, A( 1, 1 ), NMAX, DR( 1, 1 ),
     $                      DR( 1, 2 ), DR( 1, 3 ), DR( 1, 4 ),
     $                      DR( 1, 5 ), DR( 1, 6 ), DR( 1, 7 ),
     $                      DR( 1, 8 ), DR( 1, 9 ), DR( 1, 10 ),
     $                      DR( 1, 11 ), A( 1, 2 ), NMAX, A( 1, 3 ),
     $                      DC( 1, 1 ), A( 1, 4 ), A( 1, 5 ), WORK,
     $                      LWORK, RWORK, IWORK, RESULT, INFO )
               IF( INFO.NE.0 )
     $            WRITE( NOUT, FMT = 9980 )'ZDRVST', INFO
            END IF
  280    CONTINUE
*
      ELSE IF( LSAMEN( 3, C3, 'ZSG' ) ) THEN
*
*        ----------------------------------------------
*        ZSG:  Hermitian Generalized Eigenvalue Problem
*        ----------------------------------------------
*        Vary the parameters
*           NB    = block size
*           NBMIN = minimum block size
*           NX    = crossover point
*
         MAXTYP = 15
         NTYPES = MIN( MAXTYP, NTYPES )
         CALL ALAREQ( C3, NTYPES, DOTYPE, MAXTYP, NIN, NOUT )
         DO 300 I = 1, NPARMS
            CALL XLAENV( 1, NBVAL( I ) )
            CALL XLAENV( 2, NBMIN( I ) )
            CALL XLAENV( 3, NXVAL( I ) )
*
            IF( NEWSD.EQ.0 ) THEN
               DO 290 K = 1, 4
                  ISEED( K ) = IOLDSD( K )
  290          CONTINUE
            END IF
            WRITE( NOUT, FMT = 9997 )C3, NBVAL( I ), NBMIN( I ),
     $         NXVAL( I )
            IF( TSTCHK ) THEN
               CALL ZDRVSG( NN, NVAL, MAXTYP, DOTYPE, ISEED, THRESH,
     $                      NOUT, A( 1, 1 ), NMAX, A( 1, 2 ), NMAX,
     $                      DR( 1, 1 ), DR( 1, 2 ), DR( 1, 3 ),
     $                      DR( 1, 4 ), DR( 1, 5 ), DR( 1, 6 ),
     $                      DR( 1, 7 ), A( 1, 3 ), NMAX, A( 1, 4 ),
     $                      A( 1, 5 ), DC( 1, 1 ), A( 1, 6 ), A( 1, 7 ),
     $                      WORK, LWORK, RWORK, IWORK, RESULT, INFO )
               IF( INFO.NE.0 )
     $            WRITE( NOUT, FMT = 9980 )'ZDRVSG', INFO
            END IF
  300    CONTINUE
*
      ELSE IF( LSAMEN( 3, C3, 'ZBD' ) .OR. LSAMEN( 3, C3, 'SVD' ) ) THEN
*
*        ----------------------------------
*        SVD:  Singular Value Decomposition
*        ----------------------------------
*        Vary the parameters
*           NB    = block size
*           NBMIN = minimum block size
*           NX    = crossover point
*           NRHS  = number of right hand sides
*
         MAXTYP = 16
         NTYPES = MIN( MAXTYP, NTYPES )
         CALL ALAREQ( C3, NTYPES, DOTYPE, MAXTYP, NIN, NOUT )
*
*        Test the error exits
*
         IF( TSTERR .AND. TSTCHK )
     $      CALL ZERRBD( 'ZBDSQR', NOUT )
         IF( TSTERR .AND. TSTDRV )
     $      CALL ZERRED( 'ZBDSQR', NOUT )
*
         DO 320 I = 1, NPARMS
            NRHS = NSVAL( I )
            CALL XLAENV( 1, NBVAL( I ) )
            CALL XLAENV( 2, NBMIN( I ) )
            CALL XLAENV( 3, NXVAL( I ) )
            IF( NEWSD.EQ.0 ) THEN
               DO 310 K = 1, 4
                  ISEED( K ) = IOLDSD( K )
  310          CONTINUE
            END IF
            WRITE( NOUT, FMT = 9995 )C3, NBVAL( I ), NBMIN( I ),
     $         NXVAL( I ), NRHS
            IF( TSTCHK ) THEN
               CALL ZCHKBD( NN, MVAL, NVAL, MAXTYP, DOTYPE, NRHS, ISEED,
     $                      THRESH, A( 1, 1 ), NMAX, DR( 1, 1 ),
     $                      DR( 1, 2 ), DR( 1, 3 ), DR( 1, 4 ),
     $                      A( 1, 2 ), NMAX, A( 1, 3 ), A( 1, 4 ),
     $                      A( 1, 5 ), NMAX, A( 1, 6 ), NMAX, A( 1, 7 ),
     $                      A( 1, 8 ), WORK, LWORK, RWORK, NOUT, INFO )
               IF( INFO.NE.0 )
     $            WRITE( NOUT, FMT = 9980 )'ZCHKBD', INFO
            END IF
            IF( TSTDRV )
     $         CALL ZDRVBD( NN, MVAL, NVAL, MAXTYP, DOTYPE, ISEED,
     $                      THRESH, A( 1, 1 ), NMAX, A( 1, 2 ), NMAX,
     $                      A( 1, 3 ), NMAX, A( 1, 4 ), A( 1, 5 ),
     $                      A( 1, 6 ), DR( 1, 1 ), DR( 1, 2 ),
     $                      DR( 1, 3 ), WORK, LWORK, RWORK, NOUT, INFO )
  320    CONTINUE
*
      ELSE IF( LSAMEN( 3, C3, 'ZEV' ) ) THEN
*
*        --------------------------------------------
*        ZEV:  Nonsymmetric Eigenvalue Problem Driver
*              ZGEEV (eigenvalues and eigenvectors)
*        --------------------------------------------
*
         MAXTYP = 21
         NTYPES = MIN( MAXTYP, NTYPES )
         IF( NTYPES.LE.0 ) THEN
            WRITE( NOUT, FMT = 9990 )C3
         ELSE
            IF( TSTERR )
     $         CALL ZERRED( C3, NOUT )
            CALL ALAREQ( C3, NTYPES, DOTYPE, MAXTYP, NIN, NOUT )
            CALL ZDRVEV( NN, NVAL, NTYPES, DOTYPE, ISEED, THRESH, NOUT,
     $                   A( 1, 1 ), NMAX, A( 1, 2 ), DC( 1, 1 ),
     $                   DC( 1, 2 ), A( 1, 3 ), NMAX, A( 1, 4 ), NMAX,
     $                   A( 1, 5 ), NMAX, RESULT, WORK, LWORK, RWORK,
     $                   IWORK, INFO )
            IF( INFO.NE.0 )
     $         WRITE( NOUT, FMT = 9980 )'ZGEEV', INFO
         END IF
         WRITE( NOUT, FMT = 9973 )
         GO TO 10
*
      ELSE IF( LSAMEN( 3, C3, 'ZES' ) ) THEN
*
*        --------------------------------------------
*        ZES:  Nonsymmetric Eigenvalue Problem Driver
*              ZGEES (Schur form)
*        --------------------------------------------
*
         MAXTYP = 21
         NTYPES = MIN( MAXTYP, NTYPES )
         IF( NTYPES.LE.0 ) THEN
            WRITE( NOUT, FMT = 9990 )C3
         ELSE
            IF( TSTERR )
     $         CALL ZERRED( C3, NOUT )
            CALL ALAREQ( C3, NTYPES, DOTYPE, MAXTYP, NIN, NOUT )
            CALL ZDRVES( NN, NVAL, NTYPES, DOTYPE, ISEED, THRESH, NOUT,
     $                   A( 1, 1 ), NMAX, A( 1, 2 ), A( 1, 3 ),
     $                   DC( 1, 1 ), DC( 1, 2 ), A( 1, 4 ), NMAX,
     $                   RESULT, WORK, LWORK, RWORK, IWORK, LOGWRK,
     $                   INFO )
            IF( INFO.NE.0 )
     $         WRITE( NOUT, FMT = 9980 )'ZGEES', INFO
         END IF
         WRITE( NOUT, FMT = 9973 )
         GO TO 10
*
      ELSE IF( LSAMEN( 3, C3, 'ZVX' ) ) THEN
*
*        --------------------------------------------------------------
*        ZVX:  Nonsymmetric Eigenvalue Problem Expert Driver
*              ZGEEVX (eigenvalues, eigenvectors and condition numbers)
*        --------------------------------------------------------------
*
         MAXTYP = 21
         NTYPES = MIN( MAXTYP, NTYPES )
         IF( NTYPES.LE.0 ) THEN
            WRITE( NOUT, FMT = 9990 )C3
         ELSE
            IF( TSTERR )
     $         CALL ZERRED( C3, NOUT )
            CALL ALAREQ( C3, NTYPES, DOTYPE, MAXTYP, NIN, NOUT )
            CALL ZDRVVX( NN, NVAL, NTYPES, DOTYPE, ISEED, THRESH, NIN,
     $                   NOUT, A( 1, 1 ), NMAX, A( 1, 2 ), DC( 1, 1 ),
     $                   DC( 1, 2 ), A( 1, 3 ), NMAX, A( 1, 4 ), NMAX,
     $                   A( 1, 5 ), NMAX, DR( 1, 1 ), DR( 1, 2 ),
     $                   DR( 1, 3 ), DR( 1, 4 ), DR( 1, 5 ), DR( 1, 6 ),
     $                   DR( 1, 7 ), DR( 1, 8 ), RESULT, WORK, LWORK,
     $                   RWORK, INFO )
            IF( INFO.NE.0 )
     $         WRITE( NOUT, FMT = 9980 )'ZGEEVX', INFO
         END IF
         WRITE( NOUT, FMT = 9973 )
         GO TO 10
*
      ELSE IF( LSAMEN( 3, C3, 'ZSX' ) ) THEN
*
*        ---------------------------------------------------
*        ZSX:  Nonsymmetric Eigenvalue Problem Expert Driver
*              ZGEESX (Schur form and condition numbers)
*        ---------------------------------------------------
*
         MAXTYP = 21
         NTYPES = MIN( MAXTYP, NTYPES )
         IF( NTYPES.LE.0 ) THEN
            WRITE( NOUT, FMT = 9990 )C3
         ELSE
            IF( TSTERR )
     $         CALL ZERRED( C3, NOUT )
            CALL ALAREQ( C3, NTYPES, DOTYPE, MAXTYP, NIN, NOUT )
            CALL ZDRVSX( NN, NVAL, NTYPES, DOTYPE, ISEED, THRESH, NIN,
     $                   NOUT, A( 1, 1 ), NMAX, A( 1, 2 ), A( 1, 3 ),
     $                   DC( 1, 1 ), DC( 1, 2 ), DC( 1, 3 ), A( 1, 4 ),
     $                   NMAX, A( 1, 5 ), RESULT, WORK, LWORK, RWORK,
     $                   LOGWRK, INFO )
            IF( INFO.NE.0 )
     $         WRITE( NOUT, FMT = 9980 )'ZGEESX', INFO
         END IF
         WRITE( NOUT, FMT = 9973 )
         GO TO 10
*
      ELSE IF( LSAMEN( 3, C3, 'ZGG' ) ) THEN
*
*        -------------------------------------------------
*        ZGG:  Generalized Nonsymmetric Eigenvalue Problem
*        -------------------------------------------------
*        Vary the parameters
*           NB    = block size
*           NBMIN = minimum block size
*           NS    = number of shifts
*           MAXB  = minimum submatrix size
*           NBCOL = minimum column dimension for blocks
*
         MAXTYP = 26
         NTYPES = MIN( MAXTYP, NTYPES )
         CALL ALAREQ( C3, NTYPES, DOTYPE, MAXTYP, NIN, NOUT )
         IF( TSTCHK .AND. TSTERR )
     $      CALL ZERRGG( C3, NOUT )
         DO 340 I = 1, NPARMS
            CALL XLAENV( 1, NBVAL( I ) )
            CALL XLAENV( 2, NBMIN( I ) )
            CALL XLAENV( 4, NSVAL( I ) )
            CALL XLAENV( 8, MXBVAL( I ) )
            CALL XLAENV( 5, NBCOL( I ) )
*
            IF( NEWSD.EQ.0 ) THEN
               DO 330 K = 1, 4
                  ISEED( K ) = IOLDSD( K )
  330          CONTINUE
            END IF
            WRITE( NOUT, FMT = 9996 )C3, NBVAL( I ), NBMIN( I ),
     $         NSVAL( I ), MXBVAL( I ), NBCOL( I )
            TSTDIF = .FALSE.
            THRSHN = 10.D0
            IF( TSTCHK ) THEN
               CALL ZCHKGG( NN, NVAL, MAXTYP, DOTYPE, ISEED, THRESH,
     $                      TSTDIF, THRSHN, NOUT, A( 1, 1 ), NMAX,
     $                      A( 1, 2 ), A( 1, 3 ), A( 1, 4 ), A( 1, 5 ),
     $                      A( 1, 6 ), A( 1, 7 ), A( 1, 8 ), A( 1, 9 ),
     $                      NMAX, A( 1, 10 ), A( 1, 11 ), A( 1, 12 ),
     $                      DC( 1, 1 ), DC( 1, 2 ), DC( 1, 3 ),
     $                      DC( 1, 4 ), A( 1, 13 ), A( 1, 14 ), WORK,
     $                      LWORK, RWORK, LOGWRK, RESULT, INFO )
               IF( INFO.NE.0 )
     $            WRITE( NOUT, FMT = 9980 )'ZCHKGG', INFO
            END IF
            IF( TSTDRV ) THEN
               CALL ZDRVGG( NN, NVAL, MAXTYP, DOTYPE, ISEED, THRESH,
     $                      THRSHN, NOUT, A( 1, 1 ), NMAX, A( 1, 2 ),
     $                      A( 1, 3 ), A( 1, 4 ), A( 1, 5 ), A( 1, 6 ),
     $                      A( 1, 7 ), NMAX, A( 1, 8 ), DC( 1, 1 ),
     $                      DC( 1, 2 ), DC( 1, 3 ), DC( 1, 4 ),
     $                      A( 1, 8 ), A( 1, 9 ), WORK, LWORK, RWORK,
     $                      RESULT, INFO )
               IF( INFO.NE.0 )
     $            WRITE( NOUT, FMT = 9980 )'ZDRVGG', INFO
            END IF
  340    CONTINUE
*
      ELSE IF( LSAMEN( 3, C3, 'ZHB' ) ) THEN
*
*        ------------------------------
*        ZHB:  Hermitian Band Reduction
*        ------------------------------
*
         MAXTYP = 15
         NTYPES = MIN( MAXTYP, NTYPES )
         CALL ALAREQ( C3, NTYPES, DOTYPE, MAXTYP, NIN, NOUT )
         IF( TSTERR )
     $      CALL ZERRST( 'ZHBTRD', NOUT )
         CALL ZCHKHB( NN, NVAL, NK, KVAL, MAXTYP, DOTYPE, ISEED, THRESH,
     $                NOUT, A( 1, 1 ), NMAX, DR( 1, 1 ), DR( 1, 2 ),
     $                A( 1, 2 ), NMAX, WORK, LWORK, RWORK, RESULT,
     $                INFO )
         IF( INFO.NE.0 )
     $      WRITE( NOUT, FMT = 9980 )'ZCHKHB', INFO
*
      ELSE IF( LSAMEN( 3, C3, 'GLM' ) ) THEN
*
*        -----------------------------------------
*        GLM:  Generalized Linear Regression Model
*        -----------------------------------------
*
         CALL ZCKGLM( NN, NVAL, MVAL, PVAL, NTYPES, ISEED, THRESH, NMAX,
     $                A( 1, 1 ), A( 1, 2 ), B( 1, 1 ), B( 1, 2 ), X,
     $                WORK, DR( 1, 1 ), NIN, NOUT, INFO )
         IF( INFO.NE.0 )
     $      WRITE( NOUT, FMT = 9980 )'ZCKGLM', INFO
*
      ELSE IF( LSAMEN( 3, C3, 'GQR' ) ) THEN
*
*        ------------------------------------------
*        GQR:  Generalized QR and RQ factorizations
*        ------------------------------------------
*
         CALL ZCKGQR( NN, MVAL, NN, PVAL, NN, NVAL, NTYPES, ISEED,
     $                THRESH, NMAX, A( 1, 1 ), A( 1, 2 ), A( 1, 3 ),
     $                A( 1, 4 ), TAUA, B( 1, 1 ), B( 1, 2 ), B( 1, 3 ),
     $                B( 1, 4 ), B( 1, 5 ), TAUB, WORK, DR( 1, 1 ), NIN,
     $                NOUT, INFO )
         IF( INFO.NE.0 )
     $      WRITE( NOUT, FMT = 9980 )'ZCKGQR', INFO
*
      ELSE IF( LSAMEN( 3, C3, 'GSV' ) ) THEN
*
*        ----------------------------------------------
*        GSV:  Generalized Singular Value Decomposition
*        ----------------------------------------------
*
         CALL ZCKGSV( NN, MVAL, PVAL, NVAL, NTYPES, ISEED, THRESH, NMAX,
     $                A( 1, 1 ), A( 1, 2 ), B( 1, 1 ), B( 1, 2 ),
     $                A( 1, 3 ), B( 1, 3 ), A( 1, 4 ), ALPHA, BETA,
     $                B( 1, 4 ), IWORK, WORK, DR( 1, 1 ), NIN, NOUT,
     $                INFO )
         IF( INFO.NE.0 )
     $      WRITE( NOUT, FMT = 9980 )'ZCKGSV', INFO
*
      ELSE IF( LSAMEN( 3, C3, 'LSE' ) ) THEN
*
*        --------------------------------------
*        LSE:  Constrained Linear Least Squares
*        --------------------------------------
*
         CALL ZCKLSE( NN, MVAL, PVAL, NVAL, NTYPES, ISEED, THRESH, NMAX,
     $                A( 1, 1 ), A( 1, 2 ), B( 1, 1 ), B( 1, 2 ), X,
     $                WORK, DR( 1, 1 ), NIN, NOUT, INFO )
         IF( INFO.NE.0 )
     $      WRITE( NOUT, FMT = 9980 )'ZCKLSE', INFO
      ELSE
         WRITE( NOUT, FMT = * )
         WRITE( NOUT, FMT = * )
         WRITE( NOUT, FMT = 9992 )C3
      END IF
      GO TO 190
  350 CONTINUE
      WRITE( NOUT, FMT = 9994 )
      S2 = DSECND( )
      WRITE( NOUT, FMT = 9993 )S2 - S1
*
 9999 FORMAT( / ' Execution not attempted due to input errors' )
 9998 FORMAT( / / 1X, A3, ':  NB =', I4, ', NBMIN =', I4, ', NX =', I4,
     $      ', NS =', I4, ', MAXB =', I4 )
 9997 FORMAT( / / 1X, A3, ':  NB =', I4, ', NBMIN =', I4, ', NX =', I4 )
 9996 FORMAT( / / 1X, A3, ':  NB =', I4, ', NBMIN =', I4, ', NS =', I4,
     $      ', MAXB =', I4, ', NBCOL =', I4 )
 9995 FORMAT( / / 1X, A3, ':  NB =', I4, ', NBMIN =', I4, ', NX =', I4,
     $      ', NRHS =', I4 )
 9994 FORMAT( / / ' End of tests' )
 9993 FORMAT( ' Total time used = ', F12.2, ' seconds', / )
 9992 FORMAT( 1X, A3, ':  Unrecognized path name' )
 9991 FORMAT( / / ' *** Invalid integer value in column ', I2,
     $      ' of input', ' line:', / A79 )
 9990 FORMAT( / / 1X, A3, ' routines were not tested' )
 9989 FORMAT( ' Invalid input value: ', A6, '=', I6, '; must be >=',
     $      I6 )
 9988 FORMAT( ' Invalid input value: ', A6, '=', I6, '; must be <=',
     $      I6 )
 9987 FORMAT( ' Tests of the Nonsymmetric Eigenvalue Problem routines' )
 9986 FORMAT( ' Tests of the Hermitian Eigenvalue Problem routines' )
 9985 FORMAT( ' Tests of the Singular Value Decomposition routines' )
 9984 FORMAT( / ' The following parameter values will be used:' )
 9983 FORMAT( 4X, A6, 10I6, / 10X, 10I6 )
 9982 FORMAT( / ' Routines pass computational tests if test ratio is ',
     $      'less than', F8.2, / )
 9981 FORMAT( ' Relative machine ', A, ' is taken to be', D16.6 )
 9980 FORMAT( ' *** Error code from ', A6, ' = ', I4 )
 9979 FORMAT( / ' Tests of the Nonsymmetric Eigenvalue Problem Driver',
     $      / '    ZGEEV (eigenvalues and eigevectors)' )
 9978 FORMAT( / ' Tests of the Nonsymmetric Eigenvalue Problem Driver',
     $      / '    ZGEES (Schur form)' )
 9977 FORMAT( / ' Tests of the Nonsymmetric Eigenvalue Problem Expert',
     $      ' Driver', / '    ZGEEVX (eigenvalues, eigenvectors and',
     $      ' condition numbers)' )
 9976 FORMAT( / ' Tests of the Nonsymmetric Eigenvalue Problem Expert',
     $      ' Driver', / '    ZGEESX (Schur form and condition',
     $      ' numbers)' )
 9975 FORMAT( / ' Tests of the Generalized Nonsymmetric Eigenvalue ',
     $      'Problem routines' )
 9974 FORMAT( ' Tests of ZHBTRD', / ' (reduction of a Hermitian band ',
     $      'matrix to real tridiagonal form)' )
 9973 FORMAT( / 1X, 71( '-' ) )
 9972 FORMAT( / ' LAPACK VERSION 1.1, released March 31, 1993 ' )
 9971 FORMAT( / ' Tests of the Generalized Linear Regression Model ',
     $      'routines' )
 9970 FORMAT( / ' Tests of the Generalized QR and RQ routines' )
 9969 FORMAT( / ' Tests of the Generalized Singular Value',
     $      ' Decomposition routines' )
 9968 FORMAT( / ' Tests of the Linear Least Squares routines' )
*
*     End of ZCHKEE
*
      END