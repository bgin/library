*deck @(#)iorm.f	1.1  11/30/90
      subroutine iorm(file)
c
c***begin prologue     iorm
c***date written       870706   (yymmdd)
c***revision date      yymmdd   (yymmdd)
c
c***keywords           removal of a file, rm
c***author             saxe, paul (lanl)
c***source             @(#)iorm.f	1.1   11/30/90
c
c***purpose            to destroy or remove a file .
c
c***description        
c
c***references         
c
c***routines called    (none)
c
c***end prologue       iorm
c
      implicit integer (a-z)
c
      character*(*) file
      integer num
      logical ex, od
c
      inquire(file=file,exist=ex,opened=od,number=num)
      if (ex) then
c     the file exists. find out if it is opened
          if (od) then
c     its been opened. close it with a destroy
              close(num,status='delete')
          else  
c     open it, then close and destroy it.
              open(num,status='old',file=file)
              close(num,status='delete')  
          endif
      endif
c
c
      return
      end
