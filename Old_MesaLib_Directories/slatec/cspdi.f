*deck cspdi
      subroutine cspdi (ap, n, kpvt, det, work, job)
c***begin prologue  cspdi
c***purpose  compute the determinant and inverse of a complex symmetric
c            matrix stored in packed form using the factors from cspfa.
c***library   slatec (linpack)
c***category  d2c1, d3c1
c***type      complex (sspdi-s, dspdi-d, chpdi-c, cspdi-c)
c***keywords  determinant, inverse, linear algebra, linpack, matrix,
c             packed, symmetric
c***author  bunch, j., (ucsd)
c***description
c
c     cspdi computes the determinant and inverse
c     of a complex symmetric matrix using the factors from cspfa,
c     where the matrix is stored in packed form.
c
c     on entry
c
c        ap      complex (n*(n+1)/2)
c                the output from cspfa.
c
c        n       integer
c                the order of the matrix a .
c
c        kvpt    integer(n)
c                the pivot vector from cspfa.
c
c        work    complex(n)
c                work vector.  contents ignored.
c
c        job     integer
c                job has the decimal expansion  ab  where
c                   if  b .ne. 0, the inverse is computed,
c                   if  a .ne. 0, the determinant is computed.
c
c                for example, job = 11  gives both.
c
c     on return
c
c        variables not requested by job are not used.
c
c        ap     contains the upper triangle of the inverse of
c               the original matrix, stored in packed form.
c               the columns of the upper triangle are stored
c               sequentially in a one-dimensional array.
c
c        det    complex(2)
c               determinant of original matrix.
c               determinant = det(1) * 10.0**det(2)
c               with 1.0 .le. abs(det(1)) .lt. 10.0
c               or det(1) = 0.0.
c
c     error condition
c
c        a division by zero will occur if the inverse is requested
c        and  cspco  has set rcond .eq. 0.0
c        or  cspfa  has set  info .ne. 0 .
c
c***references  j. j. dongarra, j. r. bunch, c. b. moler, and g. w.
c                 stewart, linpack users' guide, siam, 1979.
c***routines called  caxpy, ccopy, cdotu, cswap
c***revision history  (yymmdd)
c   780814  date written
c   890531  changed all specific intrinsics to generic.  (wrb)
c   890831  modified array declarations.  (wrb)
c   891107  corrected category and modified routine equivalence
c           list.  (wrb)
c   891107  revision date from version 3.2
c   891214  prologue converted to version 4.0 format.  (bab)
c   900326  removed duplicate information from description section.
c           (wrb)
c   920501  reformatted the references section.  (wrb)
c***end prologue  cspdi
      integer n,job
      complex ap(*),work(*),det(2)
      integer kpvt(*)
c
      complex ak,akkp1,akp1,cdotu,d,t,temp
      real ten
      integer ij,ik,ikp1,iks,j,jb,jk,jkp1
      integer k,kk,kkp1,km1,ks,ksj,kskp1,kstep
      logical noinv,nodet
      complex zdum
      real cabs1
      cabs1(zdum) = abs(real(zdum)) + abs(aimag(zdum))
c
c***first executable statement  cspdi
      noinv = mod(job,10) .eq. 0
      nodet = mod(job,100)/10 .eq. 0
c
      if (nodet) go to 110
         det(1) = (1.0e0,0.0e0)
         det(2) = (0.0e0,0.0e0)
         ten = 10.0e0
         t = (0.0e0,0.0e0)
         ik = 0
         do 100 k = 1, n
            kk = ik + k
            d = ap(kk)
c
c           check if 1 by 1
c
            if (kpvt(k) .gt. 0) go to 30
c
c              2 by 2 block
c              use det (d  t)  =  (d/t * c - t) * t
c                      (t  c)
c              to avoid underflow/overflow troubles.
c              take two passes through scaling.  use  t  for flag.
c
               if (cabs1(t) .ne. 0.0e0) go to 10
                  ikp1 = ik + k
                  kkp1 = ikp1 + k
                  t = ap(kkp1)
                  d = (d/t)*ap(kkp1+1) - t
               go to 20
   10          continue
                  d = t
                  t = (0.0e0,0.0e0)
   20          continue
   30       continue
c
            if (nodet) go to 90
               det(1) = d*det(1)
               if (cabs1(det(1)) .eq. 0.0e0) go to 80
   40             if (cabs1(det(1)) .ge. 1.0e0) go to 50
                     det(1) = cmplx(ten,0.0e0)*det(1)
                     det(2) = det(2) - (1.0e0,0.0e0)
                  go to 40
   50             continue
   60             if (cabs1(det(1)) .lt. ten) go to 70
                     det(1) = det(1)/cmplx(ten,0.0e0)
                     det(2) = det(2) + (1.0e0,0.0e0)
                  go to 60
   70             continue
   80          continue
   90       continue
            ik = ik + k
  100    continue
  110 continue
c
c     compute inverse(a)
c
      if (noinv) go to 240
         k = 1
         ik = 0
  120    if (k .gt. n) go to 230
            km1 = k - 1
            kk = ik + k
            ikp1 = ik + k
            if (kpvt(k) .lt. 0) go to 150
c
c              1 by 1
c
               ap(kk) = (1.0e0,0.0e0)/ap(kk)
               if (km1 .lt. 1) go to 140
                  call ccopy(km1,ap(ik+1),1,work,1)
                  ij = 0
                  do 130 j = 1, km1
                     jk = ik + j
                     ap(jk) = cdotu(j,ap(ij+1),1,work,1)
                     call caxpy(j-1,work(j),ap(ij+1),1,ap(ik+1),1)
                     ij = ij + j
  130             continue
                  ap(kk) = ap(kk) + cdotu(km1,work,1,ap(ik+1),1)
  140          continue
               kstep = 1
            go to 190
  150       continue
c
c              2 by 2
c
               kkp1 = ikp1 + k
               t = ap(kkp1)
               ak = ap(kk)/t
               akp1 = ap(kkp1+1)/t
               akkp1 = ap(kkp1)/t
               d = t*(ak*akp1 - (1.0e0,0.0e0))
               ap(kk) = akp1/d
               ap(kkp1+1) = ak/d
               ap(kkp1) = -akkp1/d
               if (km1 .lt. 1) go to 180
                  call ccopy(km1,ap(ikp1+1),1,work,1)
                  ij = 0
                  do 160 j = 1, km1
                     jkp1 = ikp1 + j
                     ap(jkp1) = cdotu(j,ap(ij+1),1,work,1)
                     call caxpy(j-1,work(j),ap(ij+1),1,ap(ikp1+1),1)
                     ij = ij + j
  160             continue
                  ap(kkp1+1) = ap(kkp1+1)
     1                         + cdotu(km1,work,1,ap(ikp1+1),1)
                  ap(kkp1) = ap(kkp1)
     1                       + cdotu(km1,ap(ik+1),1,ap(ikp1+1),1)
                  call ccopy(km1,ap(ik+1),1,work,1)
                  ij = 0
                  do 170 j = 1, km1
                     jk = ik + j
                     ap(jk) = cdotu(j,ap(ij+1),1,work,1)
                     call caxpy(j-1,work(j),ap(ij+1),1,ap(ik+1),1)
                     ij = ij + j
  170             continue
                  ap(kk) = ap(kk) + cdotu(km1,work,1,ap(ik+1),1)
  180          continue
               kstep = 2
  190       continue
c
c           swap
c
            ks = abs(kpvt(k))
            if (ks .eq. k) go to 220
               iks = (ks*(ks - 1))/2
               call cswap(ks,ap(iks+1),1,ap(ik+1),1)
               ksj = ik + ks
               do 200 jb = ks, k
                  j = k + ks - jb
                  jk = ik + j
                  temp = ap(jk)
                  ap(jk) = ap(ksj)
                  ap(ksj) = temp
                  ksj = ksj - (j - 1)
  200          continue
               if (kstep .eq. 1) go to 210
                  kskp1 = ikp1 + ks
                  temp = ap(kskp1)
                  ap(kskp1) = ap(kkp1)
                  ap(kkp1) = temp
  210          continue
  220       continue
            ik = ik + k
            if (kstep .eq. 2) ik = ik + k + 1
            k = k + kstep
         go to 120
  230    continue
  240 continue
      return
      end
