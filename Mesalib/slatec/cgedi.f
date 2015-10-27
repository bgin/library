*deck cgedi
      subroutine cgedi (a, lda, n, ipvt, det, work, job)
c***begin prologue  cgedi
c***purpose  compute the determinant and inverse of a matrix using the
c            factors computed by cgeco or cgefa.
c***library   slatec (linpack)
c***category  d2c1, d3c1
c***type      complex (sgedi-s, dgedi-d, cgedi-c)
c***keywords  determinant, inverse, linear algebra, linpack, matrix
c***author  moler, c. b., (u. of new mexico)
c***description
c
c     cgedi computes the determinant and inverse of a matrix
c     using the factors computed by cgeco or cgefa.
c
c     on entry
c
c        a       complex(lda, n)
c                the output from cgeco or cgefa.
c
c        lda     integer
c                the leading dimension of the array  a .
c
c        n       integer
c                the order of the matrix  a .
c
c        ipvt    integer(n)
c                the pivot vector from cgeco or cgefa.
c
c        work    complex(n)
c                work vector.  contents destroyed.
c
c        job     integer
c                = 11   both determinant and inverse.
c                = 01   inverse only.
c                = 10   determinant only.
c
c     on return
c
c        a       inverse of original matrix if requested.
c                otherwise unchanged.
c
c        det     complex(2)
c                determinant of original matrix if requested.
c                otherwise not referenced.
c                determinant = det(1) * 10.0**det(2)
c                with  1.0 .le. cabs1(det(1)) .lt. 10.0
c                or  det(1) .eq. 0.0 .
c
c     error condition
c
c        a division by zero will occur if the input factor contains
c        a zero on the diagonal and the inverse is requested.
c        it will not occur if the subroutines are called correctly
c        and if cgeco has set rcond .gt. 0.0 or cgefa has set
c        info .eq. 0 .
c
c***references  j. j. dongarra, j. r. bunch, c. b. moler, and g. w.
c                 stewart, linpack users' guide, siam, 1979.
c***routines called  caxpy, cscal, cswap
c***revision history  (yymmdd)
c   780814  date written
c   890831  modified array declarations.  (wrb)
c   890831  revision date from version 3.2
c   891214  prologue converted to version 4.0 format.  (bab)
c   900326  removed duplicate information from description section.
c           (wrb)
c   920501  reformatted the references section.  (wrb)
c***end prologue  cgedi
      integer lda,n,ipvt(*),job
      complex a(lda,*),det(2),work(*)
c
      complex t
      real ten
      integer i,j,k,kb,kp1,l,nm1
      complex zdum
      real cabs1
      cabs1(zdum) = abs(real(zdum)) + abs(aimag(zdum))
c***first executable statement  cgedi
c
c     compute determinant
c
      if (job/10 .eq. 0) go to 70
         det(1) = (1.0e0,0.0e0)
         det(2) = (0.0e0,0.0e0)
         ten = 10.0e0
         do 50 i = 1, n
            if (ipvt(i) .ne. i) det(1) = -det(1)
            det(1) = a(i,i)*det(1)
            if (cabs1(det(1)) .eq. 0.0e0) go to 60
   10       if (cabs1(det(1)) .ge. 1.0e0) go to 20
               det(1) = cmplx(ten,0.0e0)*det(1)
               det(2) = det(2) - (1.0e0,0.0e0)
            go to 10
   20       continue
   30       if (cabs1(det(1)) .lt. ten) go to 40
               det(1) = det(1)/cmplx(ten,0.0e0)
               det(2) = det(2) + (1.0e0,0.0e0)
            go to 30
   40       continue
   50    continue
   60    continue
   70 continue
c
c     compute inverse(u)
c
      if (mod(job,10) .eq. 0) go to 150
         do 100 k = 1, n
            a(k,k) = (1.0e0,0.0e0)/a(k,k)
            t = -a(k,k)
            call cscal(k-1,t,a(1,k),1)
            kp1 = k + 1
            if (n .lt. kp1) go to 90
            do 80 j = kp1, n
               t = a(k,j)
               a(k,j) = (0.0e0,0.0e0)
               call caxpy(k,t,a(1,k),1,a(1,j),1)
   80       continue
   90       continue
  100    continue
c
c        form inverse(u)*inverse(l)
c
         nm1 = n - 1
         if (nm1 .lt. 1) go to 140
         do 130 kb = 1, nm1
            k = n - kb
            kp1 = k + 1
            do 110 i = kp1, n
               work(i) = a(i,k)
               a(i,k) = (0.0e0,0.0e0)
  110       continue
            do 120 j = kp1, n
               t = work(j)
               call caxpy(n,t,a(1,j),1,a(1,k),1)
  120       continue
            l = ipvt(k)
            if (l .ne. k) call cswap(n,a(1,k),1,a(1,l),1)
  130    continue
  140    continue
  150 continue
      return
      end