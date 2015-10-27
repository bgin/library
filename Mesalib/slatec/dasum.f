*deck dasum
      double precision function dasum (n, dx, incx)
c***begin prologue  dasum
c***purpose  compute the sum of the magnitudes of the elements of a
c            vector.
c***library   slatec (blas)
c***category  d1a3a
c***type      double precision (sasum-s, dasum-d, scasum-c)
c***keywords  blas, linear algebra, sum of magnitudes of a vector
c***author  lawson, c. l., (jpl)
c           hanson, r. j., (snla)
c           kincaid, d. r., (u. of texas)
c           krogh, f. t., (jpl)
c***description
c
c                b l a s  subprogram
c    description of parameters
c
c     --input--
c        n  number of elements in input vector(s)
c       dx  double precision vector with n elements
c     incx  storage spacing between elements of dx
c
c     --output--
c    dasum  double precision result (zero if n .le. 0)
c
c     returns sum of magnitudes of double precision dx.
c     dasum = sum from 0 to n-1 of abs(dx(ix+i*incx)),
c     where ix = 1 if incx .ge. 0, else ix = 1+(1-n)*incx.
c
c***references  c. l. lawson, r. j. hanson, d. r. kincaid and f. t.
c                 krogh, basic linear algebra subprograms for fortran
c                 usage, algorithm no. 539, transactions on mathematical
c                 software 5, 3 (september 1979), pp. 308-323.
c***routines called  (none)
c***revision history  (yymmdd)
c   791001  date written
c   890531  changed all specific intrinsics to generic.  (wrb)
c   890831  modified array declarations.  (wrb)
c   890831  revision date from version 3.2
c   891214  prologue converted to version 4.0 format.  (bab)
c   900821  modified to correct problem with a negative increment.
c           (wrb)
c   920501  reformatted the references section.  (wrb)
c***end prologue  dasum
      double precision dx(*)
      integer i, incx, ix, m, mp1, n
c***first executable statement  dasum
      dasum = 0.0d0
      if (n .le. 0) return
c
      if (incx .eq. 1) goto 20
c
c     code for increment not equal to 1.
c
      ix = 1
      if (incx .lt. 0) ix = (-n+1)*incx + 1
      do 10 i = 1,n
        dasum = dasum + abs(dx(ix))
        ix = ix + incx
   10 continue
      return
c
c     code for increment equal to 1.
c
c     clean-up loop so remaining vector length is a multiple of 6.
c
   20 m = mod(n,6)
      if (m .eq. 0) goto 40
      do 30 i = 1,m
        dasum = dasum + abs(dx(i))
   30 continue
      if (n .lt. 6) return
   40 mp1 = m + 1
      do 50 i = mp1,n,6
        dasum = dasum + abs(dx(i)) + abs(dx(i+1)) + abs(dx(i+2)) +
     1          abs(dx(i+3)) + abs(dx(i+4)) + abs(dx(i+5))
   50 continue
      return
      end
