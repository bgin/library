*deck @(#)arrmax.f	5.1  11/6/94
      function arrmax(array,n)
c***begin prologue     arrmax
c***date written       850601  (yymmdd)
c***revision date      yymmdd  (yymmdd)
c***keywords           vector, maximum
c***author             martin, richard (lanl)
c***source
c***purpose            finds the maximum element of an array.
c***description
c                      call arrmax(array,n)
c                        array    input vector, n words long.
c                        n        length of array.
c
c***references
c***routines called    max
c***end prologue       arrmax
      implicit integer(a-z)
      real*8 array(n),x,arrmax
      real*8 zero
      parameter (zero=0.0d+00)
c
c
      x=zero
      do 10 i=1,n
         x=max(x,abs(array(i)))
   10 continue
      arrmax=x
c
c
      return
      end
