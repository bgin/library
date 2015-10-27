*deck @(#)binom.f	1.1  11/30/90
      function binom(n,m)
c***begin prologue     binom
c***date written       901115  (yymmdd) 
c***revision date      yymmdd  (yymmdd)
c***keywords           binomial coefficient
c***author             martin, richard (lanl)
c***source             @(#)binom.f	1.1   11/30/90
c***purpose            returns binomial coefficient
c***description
c                      x=binom(n,m)
c                        x= n!/( (m!)(n-m)! )
c                           
c***references
c***routines called    factrl
c***end prologue       binom
      implicit integer(a-z)	
      real*8 binom
      real*8 factrl
c
c
      if(n.lt.0.or.m.lt.0) then
         call lnkerr('bad arguments to binom')
      else
         binom=factrl(n)/(factrl(m)*factrl(n-m))
      end if
c
c
      return
      end
