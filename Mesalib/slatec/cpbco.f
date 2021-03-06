*deck cpbco
      subroutine cpbco (abd, lda, n, m, rcond, z, info)
c***begin prologue  cpbco
c***purpose  factor a complex hermitian positive definite matrix stored
c            in band form and estimate the condition number of the
c            matrix.
c***library   slatec (linpack)
c***category  d2d2
c***type      complex (spbco-s, dpbco-d, cpbco-c)
c***keywords  banded, condition number, linear algebra, linpack,
c             matrix factorization, positive definite
c***author  moler, c. b., (u. of new mexico)
c***description
c
c     cpbco factors a complex hermitian positive definite matrix
c     stored in band form and estimates the condition of the matrix.
c
c     if  rcond  is not needed, cpbfa is slightly faster.
c     to solve  a*x = b , follow cpbco by cpbsl.
c     to compute  inverse(a)*c , follow cpbco by cpbsl.
c     to compute  determinant(a) , follow cpbco by cpbdi.
c
c     on entry
c
c        abd     complex(lda, n)
c                the matrix to be factored.  the columns of the upper
c                triangle are stored in the columns of abd and the
c                diagonals of the upper triangle are stored in the
c                rows of abd .  see the comments below for details.
c
c        lda     integer
c                the leading dimension of the array  abd .
c                lda must be .ge. m + 1 .
c
c        n       integer
c                the order of the matrix  a .
c
c        m       integer
c                the number of diagonals above the main diagonal.
c                0 .le. m .lt. n .
c
c     on return
c
c        abd     an upper triangular matrix  r , stored in band
c                form, so that  a = ctrans(r)*r .
c                if  info .ne. 0 , the factorization is not complete.
c
c        rcond   real
c                an estimate of the reciprocal condition of  a .
c                for the system  a*x = b , relative perturbations
c                in  a  and  b  of size  epsilon  may cause
c                relative perturbations in  x  of size  epsilon/rcond .
c                if  rcond  is so small that the logical expression
c                           1.0 + rcond .eq. 1.0
c                is true, then  a  may be singular to working
c                precision.  in particular,  rcond  is zero  if
c                exact singularity is detected or the estimate
c                underflows.  if info .ne. 0 , rcond is unchanged.
c
c        z       complex(n)
c                a work vector whose contents are usually unimportant.
c                if  a  is singular to working precision, then  z  is
c                an approximate null vector in the sense that
c                norm(a*z) = rcond*norm(a)*norm(z) .
c                if  info .ne. 0 , z  is unchanged.
c
c        info    integer
c                = 0  for normal return.
c                = k  signals an error condition.  the leading minor
c                     of order  k  is not positive definite.
c
c     band storage
c
c           if  a  is a hermitian positive definite band matrix,
c           the following program segment will set up the input.
c
c                   m = (band width above diagonal)
c                   do 20 j = 1, n
c                      i1 = max(1, j-m)
c                      do 10 i = i1, j
c                         k = i-j+m+1
c                         abd(k,j) = a(i,j)
c                10    continue
c                20 continue
c
c           this uses  m + 1  rows of  a , except for the  m by m
c           upper left triangle, which is ignored.
c
c     example:  if the original matrix is
c
c           11 12 13  0  0  0
c           12 22 23 24  0  0
c           13 23 33 34 35  0
c            0 24 34 44 45 46
c            0  0 35 45 55 56
c            0  0  0 46 56 66
c
c     then  n = 6 , m = 2  and  abd  should contain
c
c            *  * 13 24 35 46
c            * 12 23 34 45 56
c           11 22 33 44 55 66
c
c***references  j. j. dongarra, j. r. bunch, c. b. moler, and g. w.
c                 stewart, linpack users' guide, siam, 1979.
c***routines called  caxpy, cdotc, cpbfa, csscal, scasum
c***revision history  (yymmdd)
c   780814  date written
c   890531  changed all specific intrinsics to generic.  (wrb)
c   890831  modified array declarations.  (wrb)
c   890831  revision date from version 3.2
c   891214  prologue converted to version 4.0 format.  (bab)
c   900326  removed duplicate information from description section.
c           (wrb)
c   920501  reformatted the references section.  (wrb)
c***end prologue  cpbco
      integer lda,n,m,info
      complex abd(lda,*),z(*)
      real rcond
c
      complex cdotc,ek,t,wk,wkm
      real anorm,s,scasum,sm,ynorm
      integer i,j,j2,k,kb,kp1,l,la,lb,lm,mu
      complex zdum,zdum2,csign1
      real cabs1
      cabs1(zdum) = abs(real(zdum)) + abs(aimag(zdum))
      csign1(zdum,zdum2) = cabs1(zdum)*(zdum2/cabs1(zdum2))
c
c     find norm of a
c
c***first executable statement  cpbco
      do 30 j = 1, n
         l = min(j,m+1)
         mu = max(m+2-j,1)
         z(j) = cmplx(scasum(l,abd(mu,j),1),0.0e0)
         k = j - l
         if (m .lt. mu) go to 20
         do 10 i = mu, m
            k = k + 1
            z(k) = cmplx(real(z(k))+cabs1(abd(i,j)),0.0e0)
   10    continue
   20    continue
   30 continue
      anorm = 0.0e0
      do 40 j = 1, n
         anorm = max(anorm,real(z(j)))
   40 continue
c
c     factor
c
      call cpbfa(abd,lda,n,m,info)
      if (info .ne. 0) go to 180
c
c        rcond = 1/(norm(a)*(estimate of norm(inverse(a)))) .
c        estimate = norm(z)/norm(y) where  a*z = y  and  a*y = e .
c        the components of  e  are chosen to cause maximum local
c        growth in the elements of w  where  ctrans(r)*w = e .
c        the vectors are frequently rescaled to avoid overflow.
c
c        solve ctrans(r)*w = e
c
         ek = (1.0e0,0.0e0)
         do 50 j = 1, n
            z(j) = (0.0e0,0.0e0)
   50    continue
         do 110 k = 1, n
            if (cabs1(z(k)) .ne. 0.0e0) ek = csign1(ek,-z(k))
            if (cabs1(ek-z(k)) .le. real(abd(m+1,k))) go to 60
               s = real(abd(m+1,k))/cabs1(ek-z(k))
               call csscal(n,s,z,1)
               ek = cmplx(s,0.0e0)*ek
   60       continue
            wk = ek - z(k)
            wkm = -ek - z(k)
            s = cabs1(wk)
            sm = cabs1(wkm)
            wk = wk/abd(m+1,k)
            wkm = wkm/abd(m+1,k)
            kp1 = k + 1
            j2 = min(k+m,n)
            i = m + 1
            if (kp1 .gt. j2) go to 100
               do 70 j = kp1, j2
                  i = i - 1
                  sm = sm + cabs1(z(j)+wkm*conjg(abd(i,j)))
                  z(j) = z(j) + wk*conjg(abd(i,j))
                  s = s + cabs1(z(j))
   70          continue
               if (s .ge. sm) go to 90
                  t = wkm - wk
                  wk = wkm
                  i = m + 1
                  do 80 j = kp1, j2
                     i = i - 1
                     z(j) = z(j) + t*conjg(abd(i,j))
   80             continue
   90          continue
  100       continue
            z(k) = wk
  110    continue
         s = 1.0e0/scasum(n,z,1)
         call csscal(n,s,z,1)
c
c        solve  r*y = w
c
         do 130 kb = 1, n
            k = n + 1 - kb
            if (cabs1(z(k)) .le. real(abd(m+1,k))) go to 120
               s = real(abd(m+1,k))/cabs1(z(k))
               call csscal(n,s,z,1)
  120       continue
            z(k) = z(k)/abd(m+1,k)
            lm = min(k-1,m)
            la = m + 1 - lm
            lb = k - lm
            t = -z(k)
            call caxpy(lm,t,abd(la,k),1,z(lb),1)
  130    continue
         s = 1.0e0/scasum(n,z,1)
         call csscal(n,s,z,1)
c
         ynorm = 1.0e0
c
c        solve ctrans(r)*v = y
c
         do 150 k = 1, n
            lm = min(k-1,m)
            la = m + 1 - lm
            lb = k - lm
            z(k) = z(k) - cdotc(lm,abd(la,k),1,z(lb),1)
            if (cabs1(z(k)) .le. real(abd(m+1,k))) go to 140
               s = real(abd(m+1,k))/cabs1(z(k))
               call csscal(n,s,z,1)
               ynorm = s*ynorm
  140       continue
            z(k) = z(k)/abd(m+1,k)
  150    continue
         s = 1.0e0/scasum(n,z,1)
         call csscal(n,s,z,1)
         ynorm = s*ynorm
c
c        solve  r*z = w
c
         do 170 kb = 1, n
            k = n + 1 - kb
            if (cabs1(z(k)) .le. real(abd(m+1,k))) go to 160
               s = real(abd(m+1,k))/cabs1(z(k))
               call csscal(n,s,z,1)
               ynorm = s*ynorm
  160       continue
            z(k) = z(k)/abd(m+1,k)
            lm = min(k-1,m)
            la = m + 1 - lm
            lb = k - lm
            t = -z(k)
            call caxpy(lm,t,abd(la,k),1,z(lb),1)
  170    continue
c        make znorm = 1.0
         s = 1.0e0/scasum(n,z,1)
         call csscal(n,s,z,1)
         ynorm = s*ynorm
c
         if (anorm .ne. 0.0e0) rcond = ynorm/anorm
         if (anorm .eq. 0.0e0) rcond = 0.0e0
  180 continue
      return
      end
