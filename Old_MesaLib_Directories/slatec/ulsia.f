*deck ulsia
      subroutine ulsia (a, mda, m, n, b, mdb, nb, re, ae, key, mode, np,
     +   krank, ksure, rnorm, w, lw, iwork, liw, info)
c***begin prologue  ulsia
c***purpose  solve an underdetermined linear system of equations by
c            performing an lq factorization of the matrix using
c            householder transformations.  emphasis is put on detecting
c            possible rank deficiency.
c***library   slatec
c***category  d9
c***type      single precision (ulsia-s, dulsia-d)
c***keywords  linear least squares, lq factorization,
c             underdetermined linear system
c***author  manteuffel, t. a., (lanl)
c***description
c
c     ulsia computes the minimal length solution(s) to the problem ax=b
c     where a is an m by n matrix with m.le.n and b is the m by nb
c     matrix of right hand sides.  user input bounds on the uncertainty
c     in the elements of a are used to detect numerical rank deficiency.
c     the algorithm employs a row and column pivot strategy to
c     minimize the growth of uncertainty and round-off errors.
c
c     ulsia requires (mda+1)*n + (mdb+1)*nb + 6*m dimensioned space
c
c   ******************************************************************
c   *                                                                *
c   *         warning - all input arrays are changed on exit.        *
c   *                                                                *
c   ******************************************************************
c
c     input..
c
c     a(,)          linear coefficient matrix of ax=b, with mda the
c      mda,m,n      actual first dimension of a in the calling program.
c                   m is the row dimension (no. of equations of the
c                   problem) and n the col dimension (no. of unknowns).
c                   must have mda.ge.m and m.le.n.
c
c     b(,)          right hand side(s), with mdb the actual first
c      mdb,nb       dimension of b in the calling program. nb is the
c                   number of m by 1 right hand sides.  since the
c                   solution is returned in b, must have mdb.ge.n. if
c                   nb = 0, b is never accessed.
c
c   ******************************************************************
c   *                                                                *
c   *         note - use of re and ae are what make this             *
c   *                code significantly different from               *
c   *                other linear least squares solvers.             *
c   *                however, the inexperienced user is              *
c   *                advised to set re=0.,ae=0.,key=0.               *
c   *                                                                *
c   ******************************************************************
c
c     re(),ae(),key
c     re()          re() is a vector of length n such that re(i) is
c                   the maximum relative uncertainty in row i of
c                   the matrix a. the values of re() must be between
c                   0 and 1. a minimum of 10*machine precision will
c                   be enforced.
c
c     ae()          ae() is a vector of length n such that ae(i) is
c                   the maximum absolute uncertainty in row i of
c                   the matrix a. the values of ae() must be greater
c                   than or equal to 0.
c
c     key           for ease of use, re and ae may be input as either
c                   vectors or scalars. if a scalar is input, the algo-
c                   rithm will use that value for each column of a.
c                   the parameter key indicates whether scalars or
c                   vectors are being input.
c                        key=0     re scalar  ae scalar
c                        key=1     re vector  ae scalar
c                        key=2     re scalar  ae vector
c                        key=3     re vector  ae vector
c
c
c     mode          the integer mode indicates how the routine
c                   is to react if rank deficiency is detected.
c                   if mode = 0 return immediately, no solution
c                             1 compute truncated solution
c                             2 compute minimal length least squares sol
c                   the inexperienced user is advised to set mode=0
c
c     np            the first np rows of a will not be interchanged
c                   with other rows even though the pivot strategy
c                   would suggest otherwise.
c                   the inexperienced user is advised to set np=0.
c
c     work()        a real work array dimensioned 5*m.  however, if
c                   re or ae have been specified as vectors, dimension
c                   work 4*m. if both re and ae have been specified
c                   as vectors, dimension work 3*m.
c
c     lw            actual dimension of work
c
c     iwork()       integer work array dimensioned at least n+m.
c
c     liw           actual dimension of iwork.
c
c
c     info          is a flag which provides for the efficient
c                   solution of subsequent problems involving the
c                   same a but different b.
c                   if info = 0 original call
c                      info = 1 subsequent calls
c                   on subsequent calls, the user must supply a, krank,
c                   lw, iwork, liw, and the first 2*m locations of work
c                   as output by the original call to ulsia. mode must
c                   be equal to the value of mode in the original call.
c                   if mode.lt.2, only the first n locations of work
c                   are accessed. ae, re, key, and np are not accessed.
c
c
c
c
c     output..
c
c     a(,)          contains the lower triangular part of the reduced
c                   matrix and the transformation information. it togeth
c                   with the first m elements of work (see below)
c                   completely specify the lq factorization of a.
c
c     b(,)          contains the n by nb solution matrix for x.
c
c     krank,ksure   the numerical rank of a,  based upon the relative
c                   and absolute bounds on uncertainty, is bounded
c                   above by krank and below by ksure. the algorithm
c                   returns a solution based on krank. ksure provides
c                   an indication of the precision of the rank.
c
c     rnorm()       contains the euclidean length of the nb residual
c                   vectors  b(i)-ax(i), i=1,nb. if the matrix a is of
c                   full rank, then rnorm=0.0.
c
c     work()        the first m locations of work contain values
c                   necessary to reproduce the householder
c                   transformation.
c
c     iwork()       the first n locations contain the order in
c                   which the columns of a were used. the next
c                   m locations contain the order in which the
c                   rows of a were used.
c
c     info          flag to indicate status of computation on completion
c                  -1   parameter error(s)
c                   0 - rank deficient, no solution
c                   1 - rank deficient, truncated solution
c                   2 - rank deficient, minimal length least squares sol
c                   3 - numerical rank 0, zero solution
c                   4 - rank .lt. np
c                   5 - full rank
c
c***references  t. manteuffel, an interval analysis approach to rank
c                 determination in linear least squares problems,
c                 report sand80-0655, sandia laboratories, june 1980.
c***routines called  r1mach, u11us, u12us, xermsg
c***revision history  (yymmdd)
c   810801  date written
c   890831  modified array declarations.  (wrb)
c   891009  removed unreferenced variable.  (wrb)
c   891009  revision date from version 3.2
c   891214  prologue converted to version 4.0 format.  (bab)
c   900315  calls to xerror changed to calls to xermsg.  (thj)
c   900510  fixed an error message.  (rwc)
c   920501  reformatted the references section.  (wrb)
c***end prologue  ulsia
      dimension a(mda,*),b(mdb,*),re(*),ae(*),rnorm(*),w(*)
      integer iwork(*)
c
c***first executable statement  ulsia
      if(info.lt.0 .or. info.gt.1) go to 514
      it=info
      info=-1
      if(nb.eq.0 .and. it.eq.1) go to 501
      if(m.lt.1) go to 502
      if(n.lt.1) go to 503
      if(n.lt.m) go to 504
      if(mda.lt.m) go to 505
      if(liw.lt.m+n) go to 506
      if(mode.lt.0 .or. mode.gt.3) go to 515
      if(nb.eq.0) go to 4
      if(nb.lt.0) go to 507
      if(mdb.lt.n) go to 508
      if(it.eq.0) go to 4
      go to 400
    4 if(key.lt.0.or.key.gt.3) go to 509
      if(key.eq.0 .and. lw.lt.5*m) go to 510
      if(key.eq.1 .and. lw.lt.4*m) go to 510
      if(key.eq.2 .and. lw.lt.4*m) go to 510
      if(key.eq.3 .and. lw.lt.3*m) go to 510
      if(np.lt.0 .or. np.gt.m) go to 516
c
      eps=10.*r1mach(4)
      m1=1
      m2=m1+m
      m3=m2+m
      m4=m3+m
      m5=m4+m
c
      if(key.eq.1) go to 100
      if(key.eq.2) go to 200
      if(key.eq.3) go to 300
c
      if(re(1).lt.0.0) go to 511
      if(re(1).gt.1.0) go to 512
      if(re(1).lt.eps) re(1)=eps
      if(ae(1).lt.0.0) go to 513
      do 20 i=1,m
      w(m4-1+i)=re(1)
      w(m5-1+i)=ae(1)
   20 continue
      call u11us(a,mda,m,n,w(m4),w(m5),mode,np,krank,ksure,
     1            w(m1),w(m2),w(m3),iwork(m1),iwork(m2))
      go to 400
c
  100 continue
      if(ae(1).lt.0.0) go to 513
      do 120 i=1,m
      if(re(i).lt.0.0) go to 511
      if(re(i).gt.1.0) go to 512
      if(re(i).lt.eps) re(i)=eps
      w(m4-1+i)=ae(1)
  120 continue
      call u11us(a,mda,m,n,re,w(m4),mode,np,krank,ksure,
     1            w(m1),w(m2),w(m3),iwork(m1),iwork(m2))
      go to 400
c
  200 continue
      if(re(1).lt.0.0) go to 511
      if(re(1).gt.1.0) go to 512
      if(re(1).lt.eps) re(1)=eps
      do 220 i=1,m
      w(m4-1+i)=re(1)
      if(ae(i).lt.0.0) go to 513
  220 continue
      call u11us(a,mda,m,n,w(m4),ae,mode,np,krank,ksure,
     1            w(m1),w(m2),w(m3),iwork(m1),iwork(m2))
      go to 400
c
  300 continue
      do 320 i=1,m
      if(re(i).lt.0.0) go to 511
      if(re(i).gt.1.0) go to 512
      if(re(i).lt.eps) re(i)=eps
      if(ae(i).lt.0.0) go to 513
  320 continue
      call u11us(a,mda,m,n,re,ae,mode,np,krank,ksure,
     1            w(m1),w(m2),w(m3),iwork(m1),iwork(m2))
c
c     determine info
c
  400 if(krank.ne.m) go to 402
          info=5
          go to 410
  402 if(krank.ne.0) go to 404
          info=3
          go to 410
  404 if(krank.ge.np) go to 406
          info=4
          return
  406 info=mode
      if(mode.eq.0) return
  410 if(nb.eq.0) return
c
c
c     solution phase
c
      m1=1
      m2=m1+m
      m3=m2+m
      if(info.eq.2) go to 420
      if(lw.lt.m2-1) go to 510
      call u12us(a,mda,m,n,b,mdb,nb,mode,krank,
     1            rnorm,w(m1),w(m1),iwork(m1),iwork(m2))
      return
c
  420 if(lw.lt.m3-1) go to 510
      call u12us(a,mda,m,n,b,mdb,nb,mode,krank,
     1            rnorm,w(m1),w(m2),iwork(m1),iwork(m2))
      return
c
c     error messages
c
  501 call xermsg ('slatec', 'ulsia',
     +   'solution only (info=1) but no right hand side (nb=0)', 1, 0)
      return
  502 call xermsg ('slatec', 'ulsia', 'm.lt.1', 2, 1)
      return
  503 call xermsg ('slatec', 'ulsia', 'n.lt.1', 2, 1)
      return
  504 call xermsg ('slatec', 'ulsia', 'n.lt.m', 2, 1)
      return
  505 call xermsg ('slatec', 'ulsia', 'mda.lt.m', 2, 1)
      return
  506 call xermsg ('slatec', 'ulsia', 'liw.lt.m+n', 2, 1)
      return
  507 call xermsg ('slatec', 'ulsia', 'nb.lt.0', 2, 1)
      return
  508 call xermsg ('slatec', 'ulsia', 'mdb.lt.n', 2, 1)
      return
  509 call xermsg ('slatec', 'ulsia', 'key out of range', 2, 1)
      return
  510 call xermsg ('slatec', 'ulsia', 'insufficient work space', 8, 1)
      info=-1
      return
  511 call xermsg ('slatec', 'ulsia', 're(i) .lt. 0', 2, 1)
      return
  512 call xermsg ('slatec', 'ulsia', 're(i) .gt. 1', 2, 1)
      return
  513 call xermsg ('slatec', 'ulsia', 'ae(i) .lt. 0', 2, 1)
      return
  514 call xermsg ('slatec', 'ulsia', 'info out of range', 2, 1)
      return
  515 call xermsg ('slatec', 'ulsia', 'mode out of range', 2, 1)
      return
  516 call xermsg ('slatec', 'ulsia', 'np out of range', 2, 1)
      return
      end
