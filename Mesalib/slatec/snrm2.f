*deck snrm2
      real function snrm2 (n, sx, incx)
c***begin prologue  snrm2
c***purpose  compute the euclidean length (l2 norm) of a vector.
c***library   slatec (blas)
c***category  d1a3b
c***type      single precision (snrm2-s, dnrm2-d, scnrm2-c)
c***keywords  blas, euclidean length, euclidean norm, l2,
c             linear algebra, unitary, vector
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
c       sx  single precision vector with n elements
c     incx  storage spacing between elements of sx
c
c     --output--
c    snrm2  single precision result (zero if n .le. 0)
c
c     euclidean norm of the n-vector stored in sx with storage
c     increment incx .
c     if n .le. 0, return with result = 0.
c     if n .ge. 1, then incx must be .ge. 1
c
c     four phase method using two built-in constants that are
c     hopefully applicable to all machines.
c         cutlo = maximum of  sqrt(u/eps)  over all known machines.
c         cuthi = minimum of  sqrt(v)      over all known machines.
c     where
c         eps = smallest no. such that eps + 1. .gt. 1.
c         u   = smallest positive no.   (underflow limit)
c         v   = largest  no.            (overflow  limit)
c
c     brief outline of algorithm.
c
c     phase 1 scans zero components.
c     move to phase 2 when a component is nonzero and .le. cutlo
c     move to phase 3 when a component is .gt. cutlo
c     move to phase 4 when a component is .ge. cuthi/m
c     where m = n for x() real and m = 2*n for complex.
c
c     values for cutlo and cuthi.
c     from the environmental parameters listed in the imsl converter
c     document the limiting values are as follows:
c     cutlo, s.p.   u/eps = 2**(-102) for  honeywell.  close seconds are
c                   univac and dec at 2**(-103)
c                   thus cutlo = 2**(-51) = 4.44089e-16
c     cuthi, s.p.   v = 2**127 for univac, honeywell, and dec.
c                   thus cuthi = 2**(63.5) = 1.30438e19
c     cutlo, d.p.   u/eps = 2**(-67) for honeywell and dec.
c                   thus cutlo = 2**(-33.5) = 8.23181d-11
c     cuthi, d.p.   same as s.p.  cuthi = 1.30438d19
c     data cutlo, cuthi /8.232d-11,  1.304d19/
c     data cutlo, cuthi /4.441e-16,  1.304e19/
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
c   920501  reformatted the references section.  (wrb)
c***end prologue  snrm2
      integer next
      real sx(*), cutlo, cuthi, hitest, sum, xmax, zero, one
      save cutlo, cuthi, zero, one
      data zero, one /0.0e0, 1.0e0/
c
      data cutlo, cuthi /4.441e-16,  1.304e19/
c***first executable statement  snrm2
      if (n .gt. 0) go to 10
         snrm2  = zero
         go to 300
c
   10 assign 30 to next
      sum = zero
      nn = n * incx
c
c                                                 begin main loop
c
      i = 1
   20    go to next,(30, 50, 70, 110)
   30 if (abs(sx(i)) .gt. cutlo) go to 85
      assign 50 to next
      xmax = zero
c
c                        phase 1.  sum is zero
c
   50 if (sx(i) .eq. zero) go to 200
      if (abs(sx(i)) .gt. cutlo) go to 85
c
c                                prepare for phase 2.
c
      assign 70 to next
      go to 105
c
c                                prepare for phase 4.
c
  100 i = j
      assign 110 to next
      sum = (sum / sx(i)) / sx(i)
  105 xmax = abs(sx(i))
      go to 115
c
c                   phase 2.  sum is small.
c                             scale to avoid destructive underflow.
c
   70 if (abs(sx(i)) .gt. cutlo) go to 75
c
c                     common code for phases 2 and 4.
c                     in phase 4 sum is large.  scale to avoid overflow.
c
  110 if (abs(sx(i)) .le. xmax) go to 115
         sum = one + sum * (xmax / sx(i))**2
         xmax = abs(sx(i))
         go to 200
c
  115 sum = sum + (sx(i)/xmax)**2
      go to 200
c
c                  prepare for phase 3.
c
   75 sum = (sum * xmax) * xmax
c
c     for real or d.p. set hitest = cuthi/n
c     for complex      set hitest = cuthi/(2*n)
c
   85 hitest = cuthi / n
c
c                   phase 3.  sum is mid-range.  no scaling.
c
      do 95 j = i,nn,incx
      if (abs(sx(j)) .ge. hitest) go to 100
   95    sum = sum + sx(j)**2
      snrm2 = sqrt( sum )
      go to 300
c
  200 continue
      i = i + incx
      if (i .le. nn) go to 20
c
c              end of main loop.
c
c              compute square root and adjust for scaling.
c
      snrm2 = xmax * sqrt(sum)
  300 continue
      return
      end
