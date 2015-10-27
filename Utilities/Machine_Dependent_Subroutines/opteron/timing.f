*deck %W%  %G%
      subroutine timing(a,b,c)
      real*8 a,b,c
c      real ta,tb,etime,time
      real tab(2), etime,time(2)
c
c     etime returns the elapsed cpu and system time. 
c     this routine is called at the beginning(lxopen) and end(chainx)
c     of each link and the code(tsumry) does its own differencing.
c     mesa passes real*8 numbers and etime returns real*4 precision
c     so a conversion(dble) is needed.
c
c      time=etime(ta,tb)
c      a=dble(ta)
c      b=dble(tb)
      time(1)=etime(tab)
      a=dble(tab(1))
      b=dble(tab(2))
      c=0.0d+00
c
c
      return
      end
