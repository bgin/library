*deck polcof
      subroutine polcof (xx, n, x, c, d, work)
c***begin prologue  polcof
c***purpose  compute the coefficients of the polynomial fit (including
c            hermite polynomial fits) produced by a previous call to
c            polint.
c***library   slatec
c***category  e1b
c***type      single precision (polcof-s, dpolcf-d)
c***keywords  coefficients, polynomial
c***author  huddleston, r. e., (snll)
c***description
c
c     written by robert e. huddleston, sandia laboratories, livermore
c
c     abstract
c        subroutine polcof computes the coefficients of the polynomial
c     fit (including hermite polynomial fits ) produced by a previous
c     call to polint. the coefficients of the polynomial, expanded about
c     xx, are stored in the array d. the expansion is of the form
c     p(z) = d(1) + d(2)*(z-xx) +d(3)*((z-xx)**2) + ... +
c                                                  d(n)*((z-xx)**(n-1)).
c     between the call to polint and the call to polcof the variable n
c     and the arrays x and c must not be altered.
c
c     *****  input parameters
c
c     xx   - the point about which the taylor expansion is to be made.
c
c     n    - ****
c            *     n, x, and c must remain unchanged between the
c     x    - *     call to polint or the call to polcof.
c     c    - ****
c
c     *****  output parameter
c
c     d    - the array of coefficients for the taylor expansion as
c            explained in the abstract
c
c     *****  storage parameter
c
c     work - this is an array to provide internal working storage. it
c            must be dimensioned by at least 2*n in the calling program.
c
c
c     **** note - there are two methods for evaluating the fit produced
c     by polint. you may call polyvl to perform the task, or you may
c     call polcof to obtain the coefficients of the taylor expansion and
c     then write your own evaluation scheme. due to the inherent errors
c     in the computations of the taylor expansion from the newton
c     coefficients produced by polint, much more accuracy may be
c     expected by calling polyvl as opposed to writing your own scheme.
c
c***references  (none)
c***routines called  (none)
c***revision history  (yymmdd)
c   890213  date written
c   891024  corrected keyword section.  (wrb)
c   891024  revision date from version 3.2
c   891214  prologue converted to version 4.0 format.  (bab)
c***end prologue  polcof
c
      dimension x(*), c(*), d(*), work(*)
c***first executable statement  polcof
      do 10010 k=1,n
      d(k)=c(k)
10010 continue
      if (n.eq.1) return
      work(1)=1.0
      pone=c(1)
      nm1=n-1
      do 10020 k=2,n
      km1=k-1
      npkm1=n+k-1
      work(npkm1)=xx-x(km1)
      work(k)=work(npkm1)*work(km1)
      ptwo=pone+work(k)*c(k)
      pone=ptwo
10020 continue
      d(1)=ptwo
      if (n.eq.2) return
      do 10030 k=2,nm1
      km1=k-1
      km2n=k-2+n
      nmkp1=n-k+1
      do 10030 i=2,nmkp1
      km2npi=km2n+i
      im1=i-1
      km1pi=km1+i
      work(i)=work(km2npi)*work(im1)+work(i)
      d(k)=d(k)+work(i)*d(km1pi)
10030 continue
      return
      end
