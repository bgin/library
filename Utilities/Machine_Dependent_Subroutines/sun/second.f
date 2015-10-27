      subroutine second(timeout)

      implicit none
      real*8 timeout

      external etime
      real*4 etime, tarray(2)

      timeout = etime(tarray)

      return
      end
