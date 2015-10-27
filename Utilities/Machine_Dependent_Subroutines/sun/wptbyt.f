*deck %W%  %G%
      function wptbyt(n)
c
c number of bytes in a working-precision number (real*8)
c
      implicit integer(a-z)
      integer wptbyt
      real*8 rv(2)
c
c
      wptbytfac = loc(rv(2))-loc(rv(1))
      wptbyt=wptbytfac*n
c
c
      return
      end
