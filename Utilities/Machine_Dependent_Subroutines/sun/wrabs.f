      subroutine wrabs(number,buffer,nwords,il)
      real*8 buffer(*)
      integer*4 il1, number4
      il1 = il+1
      number4 = number
c **      write(number,rec=il+1)(buffer(i),i=1,nwords)
      write(number4,rec=il1)(buffer(i),i=1,nwords)
      return
      end
