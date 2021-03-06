*deck drc3jm
      subroutine drc3jm (l1, l2, l3, m1, m2min, m2max, thrcof, ndim,
     +   ier)
c***begin prologue  drc3jm
c***purpose  evaluate the 3j symbol g(m2) = (l1 l2   l3  )
c                                           (m1 m2 -m1-m2)
c            for all allowed values of m2, the other parameters
c            being held fixed.
c***library   slatec
c***category  c19
c***type      double precision (rc3jm-s, drc3jm-d)
c***keywords  3j coefficients, 3j symbols, clebsch-gordan coefficients,
c             racah coefficients, vector addition coefficients,
c             wigner coefficients
c***author  gordon, r. g., harvard university
c           schulten, k., max planck institute
c***description
c
c *usage:
c
c        double precision l1, l2, l3, m1, m2min, m2max, thrcof(ndim)
c        integer ndim, ier
c
c        call drc3jm (l1, l2, l3, m1, m2min, m2max, thrcof, ndim, ier)
c
c *arguments:
c
c     l1 :in      parameter in 3j symbol.
c
c     l2 :in      parameter in 3j symbol.
c
c     l3 :in      parameter in 3j symbol.
c
c     m1 :in      parameter in 3j symbol.
c
c     m2min :out  smallest allowable m2 in 3j symbol.
c
c     m2max :out  largest allowable m2 in 3j symbol.
c
c     thrcof :out set of 3j coefficients generated by evaluating the
c                 3j symbol for all allowed values of m2.  thrcof(i)
c                 will contain g(m2min+i-1), i=1,2,...,m2max-m2min+1.
c
c     ndim :in    declared length of thrcof in calling program.
c
c     ier :out    error flag.
c                 ier=0 no errors.
c                 ier=1 either l1.lt.abs(m1) or l1+abs(m1) non-integer.
c                 ier=2 abs(l1-l2).le.l3.le.l1+l2 not satisfied.
c                 ier=3 l1+l2+l3 not an integer.
c                 ier=4 m2max-m2min not an integer.
c                 ier=5 m2max less than m2min.
c                 ier=6 ndim less than m2max-m2min+1.
c
c *description:
c
c     although conventionally the parameters of the vector addition
c  coefficients satisfy certain restrictions, such as being integers
c  or integers plus 1/2, the restrictions imposed on input to this
c  subroutine are somewhat weaker. see, for example, section 27.9 of
c  abramowitz and stegun or appendix c of volume ii of a. messiah.
c  the restrictions imposed by this subroutine are
c       1. l1.ge.abs(m1) and l1+abs(m1) must be an integer;
c       2. abs(l1-l2).le.l3.le.l1+l2;
c       3. l1+l2+l3 must be an integer;
c       4. m2max-m2min must be an integer, where
c          m2max=min(l2,l3-m1) and m2min=max(-l2,-l3-m1).
c  if the conventional restrictions are satisfied, then these
c  restrictions are met.
c
c     the user should be cautious in using input parameters that do
c  not satisfy the conventional restrictions. for example, the
c  the subroutine produces values of
c       g(m2) = (0.75 1.50   1.75  )
c               (0.25  m2  -0.25-m2)
c  for m2=-1.5,-0.5,0.5,1.5 but none of the symmetry properties of the
c  3j symbol, set forth on page 1056 of messiah, is satisfied.
c
c     the subroutine generates g(m2min), g(m2min+1), ..., g(m2max)
c  where m2min and m2max are defined above. the sequence g(m2) is
c  generated by a three-term recurrence algorithm with scaling to
c  control overflow. both backward and forward recurrence are used to
c  maintain numerical stability. the two recurrence sequences are
c  matched at an interior point and are normalized from the unitary
c  property of 3j coefficients and wigner's phase convention.
c
c    the algorithm is suited to applications in which large quantum
c  numbers arise, such as in molecular dynamics.
c
c***references  1. abramowitz, m., and stegun, i. a., eds., handbook
c                  of mathematical functions with formulas, graphs
c                  and mathematical tables, nbs applied mathematics
c                  series 55, june 1964 and subsequent printings.
c               2. messiah, albert., quantum mechanics, volume ii,
c                  north-holland publishing company, 1963.
c               3. schulten, klaus and gordon, roy g., exact recursive
c                  evaluation of 3j and 6j coefficients for quantum-
c                  mechanical coupling of angular momenta, j math
c                  phys, v 16, no. 10, october 1975, pp. 1961-1970.
c               4. schulten, klaus and gordon, roy g., semiclassical
c                  approximations to 3j and 6j coefficients for
c                  quantum-mechanical coupling of angular momenta,
c                  j math phys, v 16, no. 10, october 1975,
c                  pp. 1971-1988.
c               5. schulten, klaus and gordon, roy g., recursive
c                  evaluation of 3j and 6j coefficients, computer
c                  phys comm, v 11, 1976, pp. 269-278.
c***routines called  d1mach, xermsg
c***revision history  (yymmdd)
c   750101  date written
c   880515  slatec prologue added by g. c. nielson, nbs; parameters
c           huge and tiny revised to depend on d1mach.
c   891229  prologue description rewritten; other prologue sections
c           revised; mmatch (location of match point for recurrences)
c           removed from argument list; argument ier changed to serve
c           only as an error flag (previously, in cases without error,
c           it returned the number of scalings); number of error codes
c           increased to provide more precise error information;
c           program comments revised; slatec error handler calls
c           introduced to enable printing of error messages to meet
c           slatec standards. these changes were done by d. w. lozier,
c           m. a. mcclain and j. m. smith of the national institute
c           of standards and technology, formerly nbs.
c   910415  mixed type expressions eliminated; variable c1 initialized;
c           description of thrcof expanded. these changes were done by
c           d. w. lozier.
c***end prologue  drc3jm
c
      integer ndim, ier
      double precision l1, l2, l3, m1, m2min, m2max, thrcof(ndim)
c
      integer i, index, lstep, n, nfin, nfinp1, nfinp2, nfinp3, nlim,
     +        nstep2
      double precision a1, a1s, c1, c1old, c2, cnorm, d1mach, dv, eps,
     +                 huge, m2, m3, newfac, oldfac, one, ratio, sign1,
     +                 sign2, srhuge, srtiny, sum1, sum2, sumbac,
     +                 sumfor, sumuni, thresh, tiny, two, x, x1, x2, x3,
     +                 y, y1, y2, y3, zero
c
      data  zero,eps,one,two /0.0d0,0.01d0,1.0d0,2.0d0/
c
c***first executable statement  drc3jm
      ier=0
c  huge is the square root of one twentieth of the largest floating
c  point number, approximately.
      huge = sqrt(d1mach(2)/20.0d0)
      srhuge = sqrt(huge)
      tiny = 1.0d0/huge
      srtiny = 1.0d0/srhuge
c
c     mmatch = zero
c
c
c  check error conditions 1, 2, and 3.
      if((l1-abs(m1)+eps.lt.zero).or.
     +   (mod(l1+abs(m1)+eps,one).ge.eps+eps))then
         ier=1
         call xermsg('slatec','drc3jm','l1-abs(m1) less than zero or '//
     +      'l1+abs(m1) not integer.',ier,1)
         return
      elseif((l1+l2-l3.lt.-eps).or.(l1-l2+l3.lt.-eps).or.
     +   (-l1+l2+l3.lt.-eps))then
         ier=2
         call xermsg('slatec','drc3jm','l1, l2, l3 do not satisfy '//
     +      'triangular condition.',ier,1)
         return
      elseif(mod(l1+l2+l3+eps,one).ge.eps+eps)then
         ier=3
         call xermsg('slatec','drc3jm','l1+l2+l3 not integer.',ier,1)
         return
      endif
c
c
c  limits for m2
      m2min = max(-l2,-l3-m1)
      m2max = min(l2,l3-m1)
c
c  check error condition 4.
      if(mod(m2max-m2min+eps,one).ge.eps+eps)then
         ier=4
         call xermsg('slatec','drc3jm','m2max-m2min not integer.',ier,1)
         return
      endif
      if(m2min.lt.m2max-eps)   go to 20
      if(m2min.lt.m2max+eps)   go to 10
c
c  check error condition 5.
      ier=5
      call xermsg('slatec','drc3jm','m2min greater than m2max.',ier,1)
      return
c
c
c  this is reached in case that m2 and m3 can take only one value.
   10 continue
c     mscale = 0
      thrcof(1) = (-one) ** int(abs(l2-l3-m1)+eps) /
     1 sqrt(l1+l2+l3+one)
      return
c
c  this is reached in case that m1 and m2 take more than one value.
   20 continue
c     mscale = 0
      nfin = int(m2max-m2min+one+eps)
      if(ndim-nfin)   21, 23, 23
c
c  check error condition 6.
   21 ier = 6
      call xermsg('slatec','drc3jm','dimension of result array for '//
     +            '3j coefficients too small.',ier,1)
      return
c
c
c
c  start of forward recursion from m2 = m2min
c
   23 m2 = m2min
      thrcof(1) = srtiny
      newfac = 0.0d0
      c1 = 0.0d0
      sum1 = tiny
c
c
      lstep = 1
   30 lstep = lstep + 1
      m2 = m2 + one
      m3 = - m1 - m2
c
c
      oldfac = newfac
      a1 = (l2-m2+one) * (l2+m2) * (l3+m3+one) * (l3-m3)
      newfac = sqrt(a1)
c
c
      dv = (l1+l2+l3+one)*(l2+l3-l1) - (l2-m2+one)*(l3+m3+one)
     1                               - (l2+m2-one)*(l3-m3-one)
c
      if(lstep-2)  32, 32, 31
c
   31 c1old = abs(c1)
   32 c1 = - dv / newfac
c
      if(lstep.gt.2)   go to 60
c
c
c  if m2 = m2min + 1, the third term in the recursion equation vanishes,
c  hence
c
      x = srtiny * c1
      thrcof(2) = x
      sum1 = sum1 + tiny * c1*c1
      if(lstep.eq.nfin)   go to 220
      go to 30
c
c
   60 c2 = - oldfac / newfac
c
c  recursion to the next 3j coefficient
      x = c1 * thrcof(lstep-1) + c2 * thrcof(lstep-2)
      thrcof(lstep) = x
      sumfor = sum1
      sum1 = sum1 + x*x
      if(lstep.eq.nfin)   go to 100
c
c  see if last unnormalized 3j coefficient exceeds srhuge
c
      if(abs(x).lt.srhuge)   go to 80
c
c  this is reached if last 3j coefficient larger than srhuge,
c  so that the recursion series thrcof(1), ... , thrcof(lstep)
c  has to be rescaled to prevent overflow
c
c     mscale = mscale + 1
      do 70 i=1,lstep
      if(abs(thrcof(i)).lt.srtiny)   thrcof(i) = zero
   70 thrcof(i) = thrcof(i) / srhuge
      sum1 = sum1 / huge
      sumfor = sumfor / huge
      x = x / srhuge
c
c
c  as long as abs(c1) is decreasing, the recursion proceeds towards
c  increasing 3j values and, hence, is numerically stable.  once
c  an increase of abs(c1) is detected, the recursion direction is
c  reversed.
c
   80 if(c1old-abs(c1))   100, 100, 30
c
c
c  keep three 3j coefficients around mmatch for comparison later
c  with backward recursion values.
c
  100 continue
c     mmatch = m2 - 1
      nstep2 = nfin - lstep + 3
      x1 = x
      x2 = thrcof(lstep-1)
      x3 = thrcof(lstep-2)
c
c  starting backward recursion from m2max taking nstep2 steps, so
c  that forwards and backwards recursion overlap at the three points
c  m2 = mmatch+1, mmatch, mmatch-1.
c
      nfinp1 = nfin + 1
      nfinp2 = nfin + 2
      nfinp3 = nfin + 3
      thrcof(nfin) = srtiny
      sum2 = tiny
c
c
c
      m2 = m2max + two
      lstep = 1
  110 lstep = lstep + 1
      m2 = m2 - one
      m3 = - m1 - m2
      oldfac = newfac
      a1s = (l2-m2+two) * (l2+m2-one) * (l3+m3+two) * (l3-m3-one)
      newfac = sqrt(a1s)
      dv = (l1+l2+l3+one)*(l2+l3-l1) - (l2-m2+one)*(l3+m3+one)
     1                               - (l2+m2-one)*(l3-m3-one)
      c1 = - dv / newfac
      if(lstep.gt.2)   go to 120
c
c  if m2 = m2max + 1 the third term in the recursion equation vanishes
c
      y = srtiny * c1
      thrcof(nfin-1) = y
      if(lstep.eq.nstep2)   go to 200
      sumbac = sum2
      sum2 = sum2 + y*y
      go to 110
c
  120 c2 = - oldfac / newfac
c
c  recursion to the next 3j coefficient
c
      y = c1 * thrcof(nfinp2-lstep) + c2 * thrcof(nfinp3-lstep)
c
      if(lstep.eq.nstep2)   go to 200
c
      thrcof(nfinp1-lstep) = y
      sumbac = sum2
      sum2 = sum2 + y*y
c
c
c  see if last 3j coefficient exceeds srhuge
c
      if(abs(y).lt.srhuge)   go to 110
c
c  this is reached if last 3j coefficient larger than srhuge,
c  so that the recursion series thrcof(nfin), ... , thrcof(nfin-lstep+1)
c  has to be rescaled to prevent overflow.
c
c     mscale = mscale + 1
      do 111 i=1,lstep
      index = nfin - i + 1
      if(abs(thrcof(index)).lt.srtiny)
     1  thrcof(index) = zero
  111 thrcof(index) = thrcof(index) / srhuge
      sum2 = sum2 / huge
      sumbac = sumbac / huge
c
      go to 110
c
c
c
c  the forward recursion 3j coefficients x1, x2, x3 are to be matched
c  with the corresponding backward recursion values y1, y2, y3.
c
  200 y3 = y
      y2 = thrcof(nfinp2-lstep)
      y1 = thrcof(nfinp3-lstep)
c
c
c  determine now ratio such that yi = ratio * xi  (i=1,2,3) holds
c  with minimal error.
c
      ratio = ( x1*y1 + x2*y2 + x3*y3 ) / ( x1*x1 + x2*x2 + x3*x3 )
      nlim = nfin - nstep2 + 1
c
      if(abs(ratio).lt.one)   go to 211
c
      do 210 n=1,nlim
  210 thrcof(n) = ratio * thrcof(n)
      sumuni = ratio * ratio * sumfor + sumbac
      go to 230
c
  211 nlim = nlim + 1
      ratio = one / ratio
      do 212 n=nlim,nfin
  212 thrcof(n) = ratio * thrcof(n)
      sumuni = sumfor + ratio*ratio*sumbac
      go to 230
c
  220 sumuni = sum1
c
c
c  normalize 3j coefficients
c
  230 cnorm = one / sqrt((l1+l1+one) * sumuni)
c
c  sign convention for last 3j coefficient determines overall phase
c
      sign1 = sign(one,thrcof(nfin))
      sign2 = (-one) ** int(abs(l2-l3-m1)+eps)
      if(sign1*sign2)  235,235,236
  235 cnorm = - cnorm
c
  236 if(abs(cnorm).lt.one)   go to 250
c
      do 240 n=1,nfin
  240 thrcof(n) = cnorm * thrcof(n)
      return
c
  250 thresh = tiny / abs(cnorm)
      do 251 n=1,nfin
      if(abs(thrcof(n)).lt.thresh)   thrcof(n) = zero
  251 thrcof(n) = cnorm * thrcof(n)
c
c
c
      return
      end
