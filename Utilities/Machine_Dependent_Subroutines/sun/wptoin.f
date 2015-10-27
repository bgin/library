*deck %W%  %G%
      function wptoin(i)
c
c     conversion from working precision to integer.
      implicit integer(a-z)
      integer wptoin
c
c
      wptoin=(wptbyt(1)/itobyt(1))*i
c
c
      return
      end
