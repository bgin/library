      subroutine abortmsg(msg,iunit)
      implicit none
      character*(*) msg
      integer i,msglen, iunit

      DO 10 i = len(msg), 1, -1
         if (msg(i:i) .ne. ' ') then
            msglen=i
            goto 20
         end if
 10   continue
      msglen=1
 20   continue
      write(iunit,30) msg(1:msglen)
 30   format ('abort with message', /, a)
      call abort()
      end
