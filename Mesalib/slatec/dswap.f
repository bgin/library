*deck dswap
      subroutine dswap (n, dx, incx, dy, incy)
c***begin prologue  dswap
c***purpose  interchange two vectors.
c***library   slatec (blas)
c***category  d1a5
c***type      double precision (sswap-s, dswap-d, cswap-c, iswap-i)
c***keywords  blas, interchange, linear algebra, vector
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
c       dy  double precision vector with n elements
c     incy  storage spacing between elements of dy
c
c     --output--
c       dx  input vector dy (unchanged if n .le. 0)
c       dy  input vector dx (unchanged if n .le. 0)
c
c     interchange double precision dx and double precision dy.
c     for i = 0 to n-1, interchange  dx(lx+i*incx) and dy(ly+i*incy),
c     where lx = 1 if incx .ge. 0, else lx = 1+(1-n)*incx, and ly is
c     defined in a similar way using incy.
c
c***references  c. l. lawson, r. j. hanson, d. r. kincaid and f. t.
c                 krogh, basic linear algebra subprograms for fortran
c                 usage, algorithm no. 539, transactions on mathematical
c                 software 5, 3 (september 1979), pp. 308-323.
c***routines called  (none)
c***revision history  (yymmdd)
c   791001  date written
c   890831  modified array declarations.  (wrb)
c   890831  revision date from version 3.2
c   891214  prologue converted to version 4.0 format.  (bab)
c   920310  corrected definition of lx in description.  (wrb)
c   920501  reformatted the references section.  (wrb)
c***end prologue  dswap
      double precision dx(*), dy(*), dtemp1, dtemp2, dtemp3
c***first executable statement  dswap
      if (n .le. 0) return
      if (incx .eq. incy) if (incx-1) 5,20,60
c
c     code for unequal or nonpositive increments.
c
    5 ix = 1
      iy = 1
      if (incx .lt. 0) ix = (-n+1)*incx + 1
      if (incy .lt. 0) iy = (-n+1)*incy + 1
      do 10 i = 1,n
        dtemp1 = dx(ix)
        dx(ix) = dy(iy)
        dy(iy) = dtemp1
        ix = ix + incx
        iy = iy + incy
   10 continue
      return
c
c     code for both increments equal to 1.
c
c     clean-up loop so remaining vector length is a multiple of 3.
c
   20 m = mod(n,3)
      if (m .eq. 0) go to 40
      do 30 i = 1,m
        dtemp1 = dx(i)
        dx(i) = dy(i)
        dy(i) = dtemp1
   30 continue
      if (n .lt. 3) return
   40 mp1 = m + 1
      do 50 i = mp1,n,3
        dtemp1 = dx(i)
        dtemp2 = dx(i+1)
        dtemp3 = dx(i+2)
        dx(i) = dy(i)
        dx(i+1) = dy(i+1)
        dx(i+2) = dy(i+2)
        dy(i) = dtemp1
        dy(i+1) = dtemp2
        dy(i+2) = dtemp3
   50 continue
      return
c
c     code for equal, positive, non-unit increments.
c
   60 ns = n*incx
      do 70 i = 1,ns,incx
        dtemp1 = dx(i)
        dx(i) = dy(i)
        dy(i) = dtemp1
   70 continue
      return
      end