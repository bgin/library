      subroutine rdabs(number,buffer,nwords,il)
      real*8 buffer(*)
      integer*4 i8
      integer*4 number4, il1
      number4 = number
      il1 = il+1
      read(number4,rec=il1)(buffer(i),i=1,nwords)
      return
      end

