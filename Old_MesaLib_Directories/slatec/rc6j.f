*deck rc6j
      subroutine rc6j (l2, l3, l4, l5, l6, l1min, l1max, sixcof, ndim,
     +   ier)
c***begin prologue  rc6j
c***purpose  evaluate the 6j symbol h(l1) = {l1 l2 l3}
c                                           {l4 l5 l6}
c            for all allowed values of l1, the other parameters
c            being held fixed.
c***library   slatec
c***category  c19
c***type      single precision (rc6j-s, drc6j-d)
c***keywords  6j coefficients, 6j symbols, clebsch-gordan coefficients,
c             racah coefficients, vector addition coefficients,
c             wigner coefficients
c***author  gordon, r. g., harvard university
c           schulten, k., max planck institute
c***description
c
c *usage:
c
c        real l2, l3, l4, l5, l6, l1min, l1max, sixcof(ndim)
c        integer ndim, ier
c
c        call rc6j(l2, l3, l4, l5, l6, l1min, l1max, sixcof, ndim, ier)
c
c *arguments:
c
c     l2 :in      parameter in 6j symbol.
c
c     l3 :in      parameter in 6j symbol.
c
c     l4 :in      parameter in 6j symbol.
c
c     l5 :in      parameter in 6j symbol.
c
c     l6 :in      parameter in 6j symbol.
c
c     l1min :out  smallest allowable l1 in 6j symbol.
c
c     l1max :out  largest allowable l1 in 6j symbol.
c
c     sixcof :out set of 6j coefficients generated by evaluating the
c                 6j symbol for all allowed values of l1.  sixcof(i)
c                 will contain h(l1min+i-1), i=1,2,...,l1max-l1min+1.
c
c     ndim :in    declared length of sixcof in calling program.
c
c     ier :out    error flag.
c                 ier=0 no errors.
c                 ier=1 l2+l3+l5+l6 or l4+l2+l6 not an integer.
c                 ier=2 l4, l2, l6 triangular condition not satisfied.
c                 ier=3 l4, l5, l3 triangular condition not satisfied.
c                 ier=4 l1max-l1min not an integer.
c                 ier=5 l1max less than l1min.
c                 ier=6 ndim less than l1max-l1min+1.
c
c *description:
c
c     the definition and properties of 6j symbols can be found, for
c  example, in appendix c of volume ii of a. messiah. although the
c  parameters of the vector addition coefficients satisfy certain
c  conventional restrictions, the restriction that they be non-negative
c  integers or non-negative integers plus 1/2 is not imposed on input
c  to this subroutine. the restrictions imposed are
c       1. l2+l3+l5+l6 and l2+l4+l6 must be integers;
c       2. abs(l2-l4).le.l6.le.l2+l4 must be satisfied;
c       3. abs(l4-l5).le.l3.le.l4+l5 must be satisfied;
c       4. l1max-l1min must be a non-negative integer, where
c          l1max=min(l2+l3,l5+l6) and l1min=max(abs(l2-l3),abs(l5-l6)).
c  if all the conventional restrictions are satisfied, then these
c  restrictions are met. conversely, if input to this subroutine meets
c  all of these restrictions and the conventional restriction stated
c  above, then all the conventional restrictions are satisfied.
c
c     the user should be cautious in using input parameters that do
c  not satisfy the conventional restrictions. for example, the
c  the subroutine produces values of
c       h(l1) = { l1 2/3  1 }
c               {2/3 2/3 2/3}
c  for l1=1/3 and 4/3 but none of the symmetry properties of the 6j
c  symbol, set forth on pages 1063 and 1064 of messiah, is satisfied.
c
c     the subroutine generates h(l1min), h(l1min+1), ..., h(l1max)
c  where l1min and l1max are defined above. the sequence h(l1) is
c  generated by a three-term recurrence algorithm with scaling to
c  control overflow. both backward and forward recurrence are used to
c  maintain numerical stability. the two recurrence sequences are
c  matched at an interior point and are normalized from the unitary
c  property of 6j coefficients and wigner's phase convention.
c
c    the algorithm is suited to applications in which large quantum
c  numbers arise, such as in molecular dynamics.
c
c***references  1. messiah, albert., quantum mechanics, volume ii,
c                  north-holland publishing company, 1963.
c               2. schulten, klaus and gordon, roy g., exact recursive
c                  evaluation of 3j and 6j coefficients for quantum-
c                  mechanical coupling of angular momenta, j math
c                  phys, v 16, no. 10, october 1975, pp. 1961-1970.
c               3. schulten, klaus and gordon, roy g., semiclassical
c                  approximations to 3j and 6j coefficients for
c                  quantum-mechanical coupling of angular momenta,
c                  j math phys, v 16, no. 10, october 1975,
c                  pp. 1971-1988.
c               4. schulten, klaus and gordon, roy g., recursive
c                  evaluation of 3j and 6j coefficients, computer
c                  phys comm, v 11, 1976, pp. 269-278.
c***routines called  r1mach, xermsg
c***revision history  (yymmdd)
c   750101  date written
c   880515  slatec prologue added by g. c. nielson, nbs; parameters
c           huge and tiny revised to depend on r1mach.
c   891229  prologue description rewritten; other prologue sections
c           revised; lmatch (location of match point for recurrences)
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
c           description of sixcof expanded. these changes were done by
c           d. w. lozier.
c***end prologue  rc6j
c
      integer ndim, ier
      real l2, l3, l4, l5, l6, l1min, l1max, sixcof(ndim)
c
      integer i, index, lstep, n, nfin, nfinp1, nfinp2, nfinp3, nlim,
     +        nstep2
      real a1, a1s, a2, a2s, c1, c1old, c2, cnorm, r1mach,
     +     denom, dv, eps, huge, l1, newfac, oldfac, one,
     +     ratio, sign1, sign2, srhuge, srtiny, sum1, sum2,
     +     sumbac, sumfor, sumuni, three, thresh, tiny, two,
     +     x, x1, x2, x3, y, y1, y2, y3, zero
c
      data  zero,eps,one,two,three /0.0,0.01,1.0,2.0,3.0/
c
c***first executable statement  rc6j
      ier=0
c  huge is the square root of one twentieth of the largest floating
c  point number, approximately.
      huge = sqrt(r1mach(2)/20.0)
      srhuge = sqrt(huge)
      tiny = 1.0/huge
      srtiny = 1.0/srhuge
c
c     lmatch = zero
c
c  check error conditions 1, 2, and 3.
      if((mod(l2+l3+l5+l6+eps,one).ge.eps+eps).or.
     +   (mod(l4+l2+l6+eps,one).ge.eps+eps))then
         ier=1
         call xermsg('slatec','rc6j','l2+l3+l5+l6 or l4+l2+l6 not '//
     +      'integer.',ier,1)
         return
      elseif((l4+l2-l6.lt.zero).or.(l4-l2+l6.lt.zero).or.
     +   (-l4+l2+l6.lt.zero))then
         ier=2
         call xermsg('slatec','rc6j','l4, l2, l6 triangular '//
     +      'condition not satisfied.',ier,1)
         return
      elseif((l4-l5+l3.lt.zero).or.(l4+l5-l3.lt.zero).or.
     +   (-l4+l5+l3.lt.zero))then
         ier=3
         call xermsg('slatec','rc6j','l4, l5, l3 triangular '//
     +      'condition not satisfied.',ier,1)
         return
      endif
c
c  limits for l1
c
      l1min = max(abs(l2-l3),abs(l5-l6))
      l1max = min(l2+l3,l5+l6)
c
c  check error condition 4.
      if(mod(l1max-l1min+eps,one).ge.eps+eps)then
         ier=4
         call xermsg('slatec','rc6j','l1max-l1min not integer.',ier,1)
         return
      endif
      if(l1min.lt.l1max-eps)   go to 20
      if(l1min.lt.l1max+eps)   go to 10
c
c  check error condition 5.
      ier=5
      call xermsg('slatec','rc6j','l1min greater than l1max.',ier,1)
      return
c
c
c  this is reached in case that l1 can take only one value
c
   10 continue
c     lscale = 0
      sixcof(1) = (-one) ** int(l2+l3+l5+l6+eps) /
     1            sqrt((l1min+l1min+one)*(l4+l4+one))
      return
c
c
c  this is reached in case that l1 can take more than one value.
c
   20 continue
c     lscale = 0
      nfin = int(l1max-l1min+one+eps)
      if(ndim-nfin)   21, 23, 23
c
c  check error condition 6.
   21 ier = 6
      call xermsg('slatec','rc6j','dimension of result array for 6j '//
     +            'coefficients too small.',ier,1)
      return
c
c
c  start of forward recursion
c
   23 l1 = l1min
      newfac = 0.0
      c1 = 0.0
      sixcof(1) = srtiny
      sum1 = (l1+l1+one) * tiny
c
      lstep = 1
   30 lstep = lstep + 1
      l1 = l1 + one
c
      oldfac = newfac
      a1 = (l1+l2+l3+one) * (l1-l2+l3) * (l1+l2-l3) * (-l1+l2+l3+one)
      a2 = (l1+l5+l6+one) * (l1-l5+l6) * (l1+l5-l6) * (-l1+l5+l6+one)
      newfac = sqrt(a1*a2)
c
      if(l1.lt.one+eps)   go to 40
c
      dv = two * ( l2*(l2+one)*l5*(l5+one) + l3*(l3+one)*l6*(l6+one)
     1           - l1*(l1-one)*l4*(l4+one) )
     2                   - (l2*(l2+one) + l3*(l3+one) - l1*(l1-one))
     3                   * (l5*(l5+one) + l6*(l6+one) - l1*(l1-one))
c
      denom = (l1-one) * newfac
c
      if(lstep-2)  32, 32, 31
c
   31 c1old = abs(c1)
   32 c1 = - (l1+l1-one) * dv / denom
      go to 50
c
c  if l1 = 1, (l1 - 1) has to be factored out of dv, hence
c
   40 c1 = - two * ( l2*(l2+one) + l5*(l5+one) - l4*(l4+one) )
     1 / newfac
c
   50 if(lstep.gt.2)   go to 60
c
c  if l1 = l1min + 1, the third term in recursion equation vanishes
c
      x = srtiny * c1
      sixcof(2) = x
      sum1 = sum1 + tiny * (l1+l1+one) * c1 * c1
c
      if(lstep.eq.nfin)   go to 220
      go to 30
c
c
   60 c2 = - l1 * oldfac / denom
c
c  recursion to the next 6j coefficient x
c
      x = c1 * sixcof(lstep-1) + c2 * sixcof(lstep-2)
      sixcof(lstep) = x
c
      sumfor = sum1
      sum1 = sum1 + (l1+l1+one) * x * x
      if(lstep.eq.nfin)   go to 100
c
c  see if last unnormalized 6j coefficient exceeds srhuge
c
      if(abs(x).lt.srhuge)   go to 80
c
c  this is reached if last 6j coefficient larger than srhuge,
c  so that the recursion series sixcof(1), ... ,sixcof(lstep)
c  has to be rescaled to prevent overflow
c
c     lscale = lscale + 1
      do 70 i=1,lstep
      if(abs(sixcof(i)).lt.srtiny)   sixcof(i) = zero
   70 sixcof(i) = sixcof(i) / srhuge
      sum1 = sum1 / huge
      sumfor = sumfor / huge
      x = x / srhuge
c
c
c  as long as the coefficient abs(c1) is decreasing, the recursion
c  proceeds towards increasing 6j values and, hence, is numerically
c  stable.  once an increase of abs(c1) is detected, the recursion
c  direction is reversed.
c
   80 if(c1old-abs(c1))   100, 100, 30
c
c
c  keep three 6j coefficients around lmatch for comparison later
c  with backward recursion.
c
  100 continue
c     lmatch = l1 - 1
      x1 = x
      x2 = sixcof(lstep-1)
      x3 = sixcof(lstep-2)
c
c
c
c  starting backward recursion from l1max taking nstep2 steps, so
c  that forward and backward recursion overlap at the three points
c  l1 = lmatch+1, lmatch, lmatch-1.
c
      nfinp1 = nfin + 1
      nfinp2 = nfin + 2
      nfinp3 = nfin + 3
      nstep2 = nfin - lstep + 3
      l1 = l1max
c
      sixcof(nfin) = srtiny
      sum2 = (l1+l1+one) * tiny
c
c
      l1 = l1 + two
      lstep = 1
  110 lstep = lstep + 1
      l1 = l1 - one
c
      oldfac = newfac
      a1s = (l1+l2+l3)*(l1-l2+l3-one)*(l1+l2-l3-one)*(-l1+l2+l3+two)
      a2s = (l1+l5+l6)*(l1-l5+l6-one)*(l1+l5-l6-one)*(-l1+l5+l6+two)
      newfac = sqrt(a1s*a2s)
c
      dv = two * ( l2*(l2+one)*l5*(l5+one) + l3*(l3+one)*l6*(l6+one)
     1           - l1*(l1-one)*l4*(l4+one) )
     2                   - (l2*(l2+one) + l3*(l3+one) - l1*(l1-one))
     3                   * (l5*(l5+one) + l6*(l6+one) - l1*(l1-one))
c
      denom = l1 * newfac
      c1 = - (l1+l1-one) * dv / denom
      if(lstep.gt.2)   go to 120
c
c  if l1 = l1max + 1 the third term in the recursion equation vanishes
c
      y = srtiny * c1
      sixcof(nfin-1) = y
      if(lstep.eq.nstep2)   go to 200
      sumbac = sum2
      sum2 = sum2 + (l1+l1-three) * c1 * c1 * tiny
      go to 110
c
c
  120 c2 = - (l1-one) * oldfac / denom
c
c  recursion to the next 6j coefficient y
c
      y = c1 * sixcof(nfinp2-lstep) + c2 * sixcof(nfinp3-lstep)
      if(lstep.eq.nstep2)   go to 200
      sixcof(nfinp1-lstep) = y
      sumbac = sum2
      sum2 = sum2 + (l1+l1-three) * y * y
c
c  see if last unnormalized 6j coefficient exceeds srhuge
c
      if(abs(y).lt.srhuge)   go to 110
c
c  this is reached if last 6j coefficient larger than srhuge,
c  so that the recursion series sixcof(nfin), ... ,sixcof(nfin-lstep+1)
c  has to be rescaled to prevent overflow
c
c     lscale = lscale + 1
      do 130 i=1,lstep
      index = nfin-i+1
      if(abs(sixcof(index)).lt.srtiny)   sixcof(index) = zero
  130 sixcof(index) = sixcof(index) / srhuge
      sumbac = sumbac / huge
      sum2 = sum2 / huge
c
      go to 110
c
c
c  the forward recursion 6j coefficients x1, x2, x3 are to be matched
c  with the corresponding backward recursion values y1, y2, y3.
c
  200 y3 = y
      y2 = sixcof(nfinp2-lstep)
      y1 = sixcof(nfinp3-lstep)
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
  210 sixcof(n) = ratio * sixcof(n)
      sumuni = ratio * ratio * sumfor + sumbac
      go to 230
c
  211 nlim = nlim + 1
      ratio = one / ratio
      do 212 n=nlim,nfin
  212 sixcof(n) = ratio * sixcof(n)
      sumuni = sumfor + ratio*ratio*sumbac
      go to 230
c
  220 sumuni = sum1
c
c
c  normalize 6j coefficients
c
  230 cnorm = one / sqrt((l4+l4+one)*sumuni)
c
c  sign convention for last 6j coefficient determines overall phase
c
      sign1 = sign(one,sixcof(nfin))
      sign2 = (-one) ** int(l2+l3+l5+l6+eps)
      if(sign1*sign2) 235,235,236
  235 cnorm = - cnorm
c
  236 if(abs(cnorm).lt.one)   go to 250
c
      do 240 n=1,nfin
  240 sixcof(n) = cnorm * sixcof(n)
      return
c
  250 thresh = tiny / abs(cnorm)
      do 251 n=1,nfin
      if(abs(sixcof(n)).lt.thresh)   sixcof(n) = zero
  251 sixcof(n) = cnorm * sixcof(n)
c
      return
      end
