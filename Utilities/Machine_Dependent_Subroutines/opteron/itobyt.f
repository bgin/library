*deck %W%  %G%
      function itobyt(n)
c
c  number of bytes in an integer 
c
      implicit integer(a-z)
      integer itobyt
      integer iv(2)
c
c
      itobytfac = loc(iv(2))-loc(iv(1))
      itobyt=itobytfac*n
c
c
      return 
      end
