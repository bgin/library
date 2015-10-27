*deck dtrsm
      subroutine dtrsm (side, uplo, transa, diag, m, n, alpha, a, lda,
     $   b, ldb)
c***begin prologue  dtrsm
c***purpose  solve one of the matrix equations.
c***library   slatec (blas)
c***category  d1b6
c***type      double precision (strsm-s, dtrsm-d, ctrsm-c)
c***keywords  level 3 blas, linear algebra
c***author  dongarra, j., (anl)
c           duff, i., (aere)
c           du croz, j., (nag)
c           hammarling, s. (nag)
c***description
c
c  dtrsm  solves one of the matrix equations
c
c     op( a )*x = alpha*b,   or   x*op( a ) = alpha*b,
c
c  where alpha is a scalar, x and b are m by n matrices, a is a unit, or
c  non-unit,  upper or lower triangular matrix  and  op( a )  is one  of
c
c     op( a ) = a   or   op( a ) = a'.
c
c  the matrix x is overwritten on b.
c
c  parameters
c  ==========
c
c  side   - character*1.
c           on entry, side specifies whether op( a ) appears on the left
c           or right of x as follows:
c
c              side = 'l' or 'l'   op( a )*x = alpha*b.
c
c              side = 'r' or 'r'   x*op( a ) = alpha*b.
c
c           unchanged on exit.
c
c  uplo   - character*1.
c           on entry, uplo specifies whether the matrix a is an upper or
c           lower triangular matrix as follows:
c
c              uplo = 'u' or 'u'   a is an upper triangular matrix.
c
c              uplo = 'l' or 'l'   a is a lower triangular matrix.
c
c           unchanged on exit.
c
c  transa - character*1.
c           on entry, transa specifies the form of op( a ) to be used in
c           the matrix multiplication as follows:
c
c              transa = 'n' or 'n'   op( a ) = a.
c
c              transa = 't' or 't'   op( a ) = a'.
c
c              transa = 'c' or 'c'   op( a ) = a'.
c
c           unchanged on exit.
c
c  diag   - character*1.
c           on entry, diag specifies whether or not a is unit triangular
c           as follows:
c
c              diag = 'u' or 'u'   a is assumed to be unit triangular.
c
c              diag = 'n' or 'n'   a is not assumed to be unit
c                                  triangular.
c
c           unchanged on exit.
c
c  m      - integer.
c           on entry, m specifies the number of rows of b. m must be at
c           least zero.
c           unchanged on exit.
c
c  n      - integer.
c           on entry, n specifies the number of columns of b.  n must be
c           at least zero.
c           unchanged on exit.
c
c  alpha  - double precision.
c           on entry,  alpha specifies the scalar  alpha. when  alpha is
c           zero then  a is not referenced and  b need not be set before
c           entry.
c           unchanged on exit.
c
c  a      - double precision array of dimension ( lda, k ), where k is m
c           when  side = 'l' or 'l'  and is  n  when  side = 'r' or 'r'.
c           before entry  with  uplo = 'u' or 'u',  the  leading  k by k
c           upper triangular part of the array  a must contain the upper
c           triangular matrix  and the strictly lower triangular part of
c           a is not referenced.
c           before entry  with  uplo = 'l' or 'l',  the  leading  k by k
c           lower triangular part of the array  a must contain the lower
c           triangular matrix  and the strictly upper triangular part of
c           a is not referenced.
c           note that when  diag = 'u' or 'u',  the diagonal elements of
c           a  are not referenced either,  but are assumed to be  unity.
c           unchanged on exit.
c
c  lda    - integer.
c           on entry, lda specifies the first dimension of a as declared
c           in the calling (sub) program.  when  side = 'l' or 'l'  then
c           lda  must be at least  max( 1, m ),  when  side = 'r' or 'r'
c           then lda must be at least max( 1, n ).
c           unchanged on exit.
c
c  b      - double precision array of dimension ( ldb, n ).
c           before entry,  the leading  m by n part of the array  b must
c           contain  the  right-hand  side  matrix  b,  and  on exit  is
c           overwritten by the solution matrix  x.
c
c  ldb    - integer.
c           on entry, ldb specifies the first dimension of b as declared
c           in  the  calling  (sub)  program.   ldb  must  be  at  least
c           max( 1, m ).
c           unchanged on exit.
c
c***references  dongarra, j., du croz, j., duff, i., and hammarling, s.
c                 a set of level 3 basic linear algebra subprograms.
c                 acm toms, vol. 16, no. 1, pp. 1-17, march 1990.
c***routines called  lsame, xerbla
c***revision history  (yymmdd)
c   890208  date written
c   910605  modified to meet slatec prologue standards.  only comment
c           lines were modified.  (bks)
c***end prologue  dtrsm
c     .. scalar arguments ..
      character*1        side, uplo, transa, diag
      integer            m, n, lda, ldb
      double precision   alpha
c     .. array arguments ..
      double precision   a( lda, * ), b( ldb, * )
c
c     .. external functions ..
      logical            lsame
      external           lsame
c     .. external subroutines ..
      external           xerbla
c     .. intrinsic functions ..
      intrinsic          max
c     .. local scalars ..
      logical            lside, nounit, upper
      integer            i, info, j, k, nrowa
      double precision   temp
c     .. parameters ..
      double precision   one         , zero
      parameter        ( one = 1.0d+0, zero = 0.0d+0 )
c***first executable statement  dtrsm
c
c     test the input parameters.
c
      lside  = lsame( side  , 'l' )
      if( lside )then
         nrowa = m
      else
         nrowa = n
      end if
      nounit = lsame( diag  , 'n' )
      upper  = lsame( uplo  , 'u' )
c
      info   = 0
      if(      ( .not.lside                ).and.
     $         ( .not.lsame( side  , 'r' ) )      )then
         info = 1
      else if( ( .not.upper                ).and.
     $         ( .not.lsame( uplo  , 'l' ) )      )then
         info = 2
      else if( ( .not.lsame( transa, 'n' ) ).and.
     $         ( .not.lsame( transa, 't' ) ).and.
     $         ( .not.lsame( transa, 'c' ) )      )then
         info = 3
      else if( ( .not.lsame( diag  , 'u' ) ).and.
     $         ( .not.lsame( diag  , 'n' ) )      )then
         info = 4
      else if( m  .lt.0               )then
         info = 5
      else if( n  .lt.0               )then
         info = 6
      else if( lda.lt.max( 1, nrowa ) )then
         info = 9
      else if( ldb.lt.max( 1, m     ) )then
         info = 11
      end if
      if( info.ne.0 )then
         call xerbla( 'dtrsm ', info )
         return
      end if
c
c     quick return if possible.
c
      if( n.eq.0 )
     $   return
c
c     and when  alpha.eq.zero.
c
      if( alpha.eq.zero )then
         do 20, j = 1, n
            do 10, i = 1, m
               b( i, j ) = zero
   10       continue
   20    continue
         return
      end if
c
c     start the operations.
c
      if( lside )then
         if( lsame( transa, 'n' ) )then
c
c           form  b := alpha*inv( a )*b.
c
            if( upper )then
               do 60, j = 1, n
                  if( alpha.ne.one )then
                     do 30, i = 1, m
                        b( i, j ) = alpha*b( i, j )
   30                continue
                  end if
                  do 50, k = m, 1, -1
                     if( b( k, j ).ne.zero )then
                        if( nounit )
     $                     b( k, j ) = b( k, j )/a( k, k )
                        do 40, i = 1, k - 1
                           b( i, j ) = b( i, j ) - b( k, j )*a( i, k )
   40                   continue
                     end if
   50             contiis ',i7)    
      call exit
      end
      subroutine voronoi(miter,ncent,a,ngrid,grid,npmx,d,r,p,wtt)
      dimension a(3,ncent),ngrid(ncent),grid(4,npmx,ncent),
     $ d(ncent),r(ncent,ncent),p(npmx),wtt(ncent)
      data pi/3.14159265358/,eta/1./
c
c compute distances between atoms
c
      do 1 i=1,ncent
      do 1 j=1,ncent
       dist=sqrt((a(1,i)-a(1,j))**2
     $           +(a(2,i)-a(2,j))**2
     $           +(a(3,i)-a(3,j))**2)
 1     r(i,j)=dist         
c
c loop over all atoms
c
      do 2 jato( i, j ) = b( i, j ) - b( k, j )*a( i, k )
   80                   continue
                     end if
   90             continue
  100          continue
            end if
         else
c
c           form  b := alpha*inv( a' )*b.
c
            if( upper )then
               do 130, j = 1, n
                  do 120, i = 1, m
                     temp = alpha*b( i, j )
                     do 110, k = 1, i - 1
                        temp = temp - a( k, i )*b( k, j )
  110                continue
                     if( nounit )
     $                  temp = temp/a( i, i )
                     b( i, j ) = temp
  120             continue
  130          continue
            else
               do 160, j = 1, n
                  do 150, i = m, 1, -1
                     temp = alpha*b( i, j )
                     do 140, k = i + 1, m
                        temp = temp - a( k, i )*b( k, j )
  140                continue
                     if( nounit )
     $                  temp = temp/a( i, i )
                     b( i, j ) = temp
  150             continue
  160          continue
            end if
         end if
      else
         if( lsame( transa, 'n' ) )then
c
c           form  b := alpha*b*inv( a ).
c
            if( upper )then
               do 210, j = 1, n
                  if( alpha.ne.one )then
                     do 170, i = 1, m
                        b( i, j ) = alpha*b( i, j )
  170                continue
                  end if
                  do 190, k = 1, j - 1
                     if( a( k, j ).ne.zero )then
                        do 180, i = 1, m
                           b( i, j ) = b( i, j ) - a( k, j )*b( i, k )
  180                   continue
                     end if
  190             continue
                  if( nounit )then
                     temp = one/a( j, j )
                     do 200, i = 1, m
                        b( i, j ) = temp*b( i, j )
  200                continue
                  end if
  210          continue
            else
               do 260, j = n, 1, -1
                  if( alpha.ne.one )then
                     do 220, i = 1, m
                        b( i, j ) = alpha*b( i, j )
  220                continue
                  end if
                  do 240, k = j + 1, n
                     if( a( k, j ).ne.zero )then
                        do 230, i = 1, m
                           b( i, j ) = b( i, j ) - a( k, j )*b( i, k )
  230                   continue
                     end if
  240             continue
                  if( nounit )then
                     temp = one/a( j, j )
                     do 250, i = 1, m
                       b( i, j ) = temp*b( i, j )
  250                continue
                  end if
  260          continue
            end if
         else
c
c           form  b := alpha*b*inv( a' ).
c
            if( upper )then
               do 310, k = n, 1, -1
                  if( nounit )then
                     temp = one/a( k, k )
                     do 270, i = 1, m
                        b( i, k ) = temp*b( i, k )
  270                continue
                  end if
                  do 290, j = 1, k - 1
                     if( a( j, k ).ne.zero )then
                        temp = a( j, k )
                        do 280, i = 1, m
                           b( i, j ) = b( i, j ) - temp*b( i, k )
  280                   continue
                     end if
  290             continue
                  if( alpha.ne.one )then
                     do 300, i = 1, m
                        b( i, k ) = alpha*b( i, k )
  300                continue
                  end if
  310          continue
            else
               do 360, k = 1, n
                  if( nounit )then
                     temp = one/a( k, k )
                     do 320, i = 1, m
                        b( i, k ) = temp*b( i, k )
  320                continue
                  end if
                  do 340, j = k + 1, n
                     if( a( j, k ).ne.zero )then
                        temp = a( j, k )
                        do 330, i = 1, m
                           b( i, j ) = b( i, j ) - temp*b( i, k )
  330                   continue
                     end if
  340             continue
                  if( alpha.ne.one )then
                     do 350, i = 1, m
                        b( i, k ) = alpha*b( i, k )
  350                continue
                  end if
  360          continue
            end if
         end if
      end if
c
      return
c
c     end of dtrsm .
c
      end
