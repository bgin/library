      subroutine rmachdp(rmach)

      double precision rmach(5)
c      real*8 two
c      integer nexp, nman

c      nexp = 11
c      nman = 53
c      two = 2

c      rmach(1) = two**(-(2**(nexp-1) -3))
c      rmach(2) = two**(2**(nexp-1) -1)

c      rmach(3) = two**(1-nman)
c      rmach(4) = two**(1-nman)

c      rmach(5) = log10(two)

      rmach(1) = 0.445014771701440500D-307
      rmach(2) = 0.898846567431157500D+308
      rmach(3) = 0.222044604925031300D-15
      rmach(4) = 0.222044604925031300D-15
      rmach(5) = 0.301029995663981200D+00

      return
      end

